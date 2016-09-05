{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
module LLVM.Instruction

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression                     (Expression (..))
import qualified AST.Expression                     as E (Expression' (..))
import           AST.Instruction                    (Guard, Instruction (..),
                                                     Instruction' (..))
import           AST.Object                         (Object' (..),
                                                     Object'' (..))
import           LLVM.Declaration
import           LLVM.Expression
import           LLVM.State
import           LLVM.Type
import           Location
import           Type                               as T
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (-=), (.=))
import           Control.Monad                      (when, zipWithM_)
import qualified Control.Monad                      as M (void)
import           Data.Foldable                      (toList)
import           Data.Monoid                        ((<>))
import           Data.Sequence                      (ViewR ((:>)))
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton, viewr,
                                                            (|>))
import           Data.Text                          (unpack)
import           Data.Word
import           LLVM.General.AST                   (BasicBlock (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import           LLVM.General.AST.Instruction       (FastMathFlags (..),
                                                     Named (..),
                                                     Terminator (..))
import qualified LLVM.General.AST.Instruction       as LLVM (Instruction (..))
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type
--------------------------------------------------------------------------------

guard :: Guard -> Name -> LLVM ()

guard (expr, decls, insts) falseLabel = do
  cond      <- expression expr
  trueLabel <- newLabel
  let condBr = CondBr { condition = cond
                      , trueDest  = trueLabel
                      , falseDest = falseLabel
                      , metadata' = []
                      }
  setLabel trueLabel $ Do condBr
  openScope
  mapM_ declaration decls
  mapM_ instruction insts
  closeScope



instruction :: Instruction -> LLVM ()
instruction Instruction {instLoc=Location(pos, _), inst'} = case inst' of
  Abort -> createTagAbort pos

  Assertion expr -> do
    -- Evaluate the condition expression
    cond <- expression expr
    -- Create both label
    trueLabel  <- newLabel
    falseLabel <- newLabel
    -- Create the conditional branch
    let condBr = CondBr { condition = cond
                        , trueDest  = trueLabel
                        , falseDest = falseLabel
                        , metadata' = []
                        }
    -- Set the false label to the abort
    setLabel falseLabel $ Do condBr
    -- And the true label to the next instructions
    createTagAssert trueLabel pos
    {- setLabel and createTagAssert set the label of the next block -}

  Assign { assignPairs } -> mapM_ assign' assignPairs
    where
      assign' (lval, expr) = do
        ref   <- objectRef lval
        label <- newLabel
        type' <- toLLVMType $ objType lval

        store <- if expType expr == GPointer GAny
          then return LLVM.Store
                    { LLVM.volatile = False
                    , LLVM.address  = ref
                    , LLVM.value    = ConstantOperand $ C.Null type'
                    , LLVM.maybeAtomicity = Nothing
                    , LLVM.alignment = 4
                    , LLVM.metadata  = []
                    }
          else do
              value <- expression expr
              return LLVM.Store
                    { LLVM.volatile = False
                    , LLVM.address  = ref
                    , LLVM.value    = value
                    , LLVM.maybeAtomicity = Nothing
                    , LLVM.alignment = 4
                    , LLVM.metadata  = []
                    }
        addInstruction (label := store)


  -- TODO!
  Conditional { cguards } -> do
    finalLabel <- newLabel
    abortLabel <- newLabel
    makeGuards (toList cguards) finalLabel abortLabel

    let branch = Br { dest      = finalLabel
                    , metadata' = []
                    }
    setLabel abortLabel $ Do branch
    createTagIf finalLabel pos
    where
      makeGuards [guard'] _ abortLabel =
        guard guard' abortLabel

      makeGuards (guard':xs) finalLabel abortLabel = do
        nextGuardLabel <- newLabel
        guard guard' nextGuardLabel
        let branch = Br { dest      = finalLabel
                        , metadata' = []
                        }
        setLabel nextGuardLabel $ Do branch

        makeGuards xs finalLabel abortLabel


  Block decls insts -> do
    openScope
    mapM_ declaration decls
    mapM_ instruction insts
    closeScope

  ProcedureCall { pname, pargs } -> do
    args <- mapM createArg pargs
    label <- newLabel
    let
      proc = Right . ConstantOperand $ C.GlobalReference voidType $ Name (unpack pname)

      call = LLVM.Call
                { LLVM.tailCallKind       = Nothing
                , LLVM.callingConvention  = CC.C
                , LLVM.returnAttributes   = []
                , LLVM.function           = proc
                , LLVM.arguments          = toList args
                , LLVM.functionAttributes = []
                , LLVM.metadata           = []}
    addInstruction $ label := call
    where
      -- Out and InOut arguments need to be passed as pointers to, so the address has to be casted
      -- If it is not an Out or InOut argument, then just pass a constant value.
      -- only basic types or pointers (because a pointer is just an integer) can be passed as a constant value.
      createArg (e,mode) = do  
        expr <- if mode == In && (expType e =:= basicOrPointer)
            then expression e 
            else do
              label <- newLabel
              ref   <- objectRef . E.theObj . exp' $ e
              type' <- ptr <$> (toLLVMType . expType $ e)
              let
                
                bitcast = LLVM.BitCast
                    { LLVM.operand0 = ref
                    , LLVM.type'    = type'
                    , LLVM.metadata = []
                    }
              addInstruction $ label := bitcast
              return $ LocalReference type' label
        return (expr,[])

      basicOrPointer = T.GOneOf [T.GBool,T.GChar,T.GInt,T.GFloat]
              
  Free { idName, freeType } -> do
    labelLoad  <- newLabel
    labelCast  <- newLabel
    labelCall  <- newLabel
    labelNull  <- newLabel
    labelStore <- newLabel
    ref        <- objectRef idName
    type' <- toLLVMType (T.GPointer freeType)
    
    let
      load = LLVM.Load { LLVM.volatile  = False
                       , LLVM.address   = ref
                       , LLVM.maybeAtomicity = Nothing
                       , LLVM.alignment = 4
                       , LLVM.metadata  = [] }

      ptr = LocalReference type' $ labelLoad

      bitcast = LLVM.BitCast
            { LLVM.operand0 = ptr
            , LLVM.type'    = PointerType i8 (AddrSpace 0)
            , LLVM.metadata = []
            }

      ptr8bytes = LocalReference type' labelCast

      fun = Right . ConstantOperand $ C.GlobalReference voidType $ Name "_free"

      arg = [(ptr8bytes,[])]

      call = LLVM.Call
                { LLVM.tailCallKind       = Nothing
                , LLVM.callingConvention  = CC.C
                , LLVM.returnAttributes   = []
                , LLVM.function           = fun
                , LLVM.arguments          = arg
                , LLVM.functionAttributes = []
                , LLVM.metadata           = []}

      store = LLVM.Store
                { LLVM.volatile = False
                , LLVM.address  = ref
                , LLVM.value    = ConstantOperand $ C.Null type'
                , LLVM.maybeAtomicity = Nothing
                , LLVM.alignment = 4
                , LLVM.metadata  = []
                }


    addInstructions . Seq.fromList $ [ labelLoad  := load
                                     , labelCast  := bitcast
                                     , labelCall  := call
                                     , labelStore := store]

  New { idName,nType } -> do
    labelCall  <- newLabel
    labelCast  <- newLabel
    labelStore <- newLabel
    ref        <- objectRef idName -- The variable that is being mallocated
    type'      <- toLLVMType (T.GPointer nType)
    let
      

      -- Call C malloc
      fun  = Right . ConstantOperand $ C.GlobalReference pointerType $ Name "_malloc"
      arg  = [(ConstantOperand $ C.Int 32 (sizeOf nType),[])]
      call = LLVM.Call
                { LLVM.tailCallKind       = Nothing
                , LLVM.callingConvention  = CC.C
                , LLVM.returnAttributes   = []
                , LLVM.function           = fun
                , LLVM.arguments          = arg
                , LLVM.functionAttributes = []
                , LLVM.metadata           = []}
      -- Cast the result pointer of malloc to the corrector type
      bitcast = LLVM.BitCast
            { LLVM.operand0 = LocalReference pointerType labelCall
            , LLVM.type'    = type'
            , LLVM.metadata = []
            }

      -- Store the casted pointer in the variable
      store = LLVM.Store
                { LLVM.volatile = False
                , LLVM.address  = ref
                , LLVM.value    = LocalReference type' labelCast
                , LLVM.maybeAtomicity = Nothing
                , LLVM.alignment = 4
                , LLVM.metadata  = []
                }

    addInstructions . Seq.fromList $ [ labelCall  := call
                                     , labelCast  := bitcast
                                     , labelStore := store]


  Write { ln, wexprs } -> do
    mapM_ write wexprs
    where
      write wexpr = do
        label <- newLabel
        -- Build the operand of the expression
        operand <- expression wexpr
        let
        -- Call the correct C write function
          fun = Right . ConstantOperand $ C.GlobalReference voidType $ fwrite ln (expType wexpr)
          arg = [(operand, [])]
          call =  LLVM.Call
                { LLVM.tailCallKind       = Nothing
                , LLVM.callingConvention  = CC.C
                , LLVM.returnAttributes   = []
                , LLVM.function           = fun
                , LLVM.arguments          = arg
                , LLVM.functionAttributes = []
                , LLVM.metadata           = []}
        -- return a named instruction
        addInstruction $ label := call

      {- Returns the name of the write function depending of on the type and `ln` -}
      fwrite True expType = Name $ case expType of
          T.GBool   -> writeLnBool
          T.GChar   -> writeLnChar
          T.GFloat  -> writeLnFloat
          T.GInt    -> writeLnInt
          T.GString -> writeLnString
          _         -> error "No se puede escribir algo q no sea basico :D"
      fwrite False expType = Name $ case expType of
          T.GBool   -> writeBool
          T.GChar   -> writeChar
          T.GFloat  -> writeFloat
          T.GInt    -> writeInt
          T.GString -> writeString
          _         -> error "No se puede escribir algo q no sea basico :D"

  Read { file, vars } -> case file of
    Nothing ->
      mapM_ readVarStdin vars
    Just file' -> error "No se puede con archivos"

    where
      readVarStdin var = do
        let
          t = objType var
          
          fread = Name $ case t of
            T.GChar   -> readCharStd
            T.GFloat  -> readFloatStd
            T.GInt    -> readIntStd
            _         -> error ":D no se soporta este tipo: " <> show t

        -- Call the C read function
        type' <- toLLVMType t
        let
          fun = Right . ConstantOperand $ C.GlobalReference type' fread
          call = LLVM.Call
                { LLVM.tailCallKind       = Nothing
                , LLVM.callingConvention  = CC.C
                , LLVM.returnAttributes   = []
                , LLVM.function           = fun
                , LLVM.arguments          = []
                , LLVM.functionAttributes = []
                , LLVM.metadata           = []}

        label <- newLabel
        -- Get the reference of the variable's memory
        objRef <- objectRef var
        -- Store the value saved at `label` in the variable memory
        let store = LLVM.Store
                { LLVM.volatile = False
                , LLVM.address  = objRef
                , LLVM.value    = LocalReference type' label
                , LLVM.maybeAtomicity = Nothing
                , LLVM.alignment = 4
                , LLVM.metadata  = []
                }
        label' <- newLabel
        addInstructions $ Seq.fromList [label := call, label' := store]

  e -> error "Esta instruccion no existe"


-- createInstruction :: Instruction -> LLVM ()
-- createInstruction (Instruction loc inst') = case inst' of

--   Skip        -> return ()


--   Abort -> do
--     createTagAbort (from loc)
--     return ()

--   -- GuardAction assert action -> do
--   --   createState "" assert
--   --   createInstruction action

--   -- LAssign [id] [exp] -> do
--   --   let ty = toType $ expType id
--   --   e'  <- createExpression exp
--   --   id' <- getStoreDir id
--   --   store ty id' e'
--   --   return ()

--   -- LAssign ids exps -> do
--   --   list <- zipWithM createAssign ids exps
--   --   zipWithM_ createMultyAssign ids list

--   Write True exp -> do
--     let ty  = expType exp
--     let ty' = voidType
--     e'     <- createExpression exp
--     case ty of
--       T.GInt      -> procedureCall ty' writeLnInt    [e']
--       T.GFloat    -> procedureCall ty' writeLnDouble [e']
--       T.GBoolean  -> procedureCall ty' writeLnBool   [e']
--       T.GChar     -> procedureCall ty' writeLnChar   [e']
--       T.GEmpty    -> procedureCall ty' writeLnString [e']
--     return ()

--   Write False exp -> do
--     let ty  = expType exp
--     let ty' = voidType
--     e'     <- createExpression exp

--     case ty of
--       T.GInt      -> procedureCall ty' writeInt    [e']
--       T.GFloat    -> procedureCall ty' writeDouble [e']
--       T.GBoolean  -> procedureCall ty' writeBool   [e']
--       T.GChar     -> procedureCall ty' writeChar   [e']
--       T.GEmpty    -> procedureCall ty' writeString [e']
--     return ()

--   Block st decs accs -> do
--     mapM_ accToAlloca decs
--     mapM_ createInstruction accs
--     return ()

--   Conditional guards -> do
--     final <- newLabel
--     abort <- newLabel
--     genGuards guards abort final

--     setLabel abort $ branch final
--     createTagIf final (from loc)
--     return ()

--   Repeat guards inv bound -> do
--     final   <- newLabel
--     initial <- newLabel

--     name <- getCount
--     let boundName = show name
--     op' <- alloca Nothing intType boundName
--     store intType op' $ constantInt maxInteger
--     addVarOperand (show boundName) op'

--     setLabel initial $ branch initial
--     createState "" inv
--     createState boundName bound
--     genGuards guards final initial
--     setLabel final $ branch initial
--     return ()

--   ProcCallCont pname st args c -> do
--     let dic   = getMap . getCurrent . procTable $ c
--     let nargp = procArgs c
--     exp <- createArguments dic nargp args
--     procedureCall voidType (TE.unpack pname) exp
--     return ()

--   Ran id t -> do
--     vars <- use varsLoc
--     let (ty, i) = (toType t, fromJust $ Map.lookup (TE.unpack id) vars)
--     let df      = Right $ definedFunction floatType (Name randomInt)
--     val <- caller ty df []
--     store ty i val
--     return ()



-- accToAlloca :: Declaration -> LLVM ()
-- accToAlloca Declaration {declType, declLvals, declExprs} = do



--   Id name -> do
--     let name' = TE.unpack name
--     dim <- typeToOperand name' t
--     let t' = toType t
--     alloca dim t' name'

--     case t of
--       T.GArray d ty -> createInstruction acc
--       _             -> do
--         initialize name' t
--         createInstruction acc

--   Assign lids _  -> do
--     mapM_ idToAlloca lids
--     createInstruction acc

--   Read Nothing types vars -> do
--     res <- mapM callRead types
--     ads <- mapM (getVarOperand . TE.unpack . fst) vars
--     mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
--     return ()

--   Read (Just file) types vars -> do
--     res <- mapM (callReadFile $ TE.unpack file) types
--     ads <- mapM (getVarOperand . TE.unpack . fst) vars
--     mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
--     return ()


-- convertFile :: String -> String
-- convertFile file = "__"++file

-- convertId :: String -> String
-- convertId name = '_':name

-- convertId' :: AST -> String
-- convertId' (AST _ _ _ ast') = case ast' of
--   ArrCall id _ -> "__" ++ TE.unpack id
--   Id      id   -> "__" ++ TE.unpack id
--   _            -> undefined

-- convertId'' :: AST -> String
-- convertId'' (AST _ _ _ ast') = case ast' of
--   ArrCall id _ -> "___" ++ TE.unpack id
--   Id      id   -> "___" ++ TE.unpack id
--   _            -> undefined

-- procedureCall :: Type -> String -> [Operand] -> LLVM Operand
-- procedureCall t pname es = do
--   let es' = map (\e -> (e, [])) es
--   let df  = Right $ definedFunction t (Name pname)
--   caller t df es'


-- getStoreDir :: AST -> LLVM Operand
-- getStoreDir (AST pos _ t ast') = case ast' of

--   Id name ->  getVarOperand (TE.unpack name)

--   ArrCall name exps -> do
--     ac' <- mapM createExpression exps
--     map <- use varsLoc
--     let (i, id) = (fromJust $ Map.lookup id map, TE.unpack name)
--     ac'' <- opsToArrayIndex id ac'
--     addUnNamedInstruction intType $ GetElementPtr True i [ac''] []

--   _ -> undefined


-- callReadFile :: String -> T.Type -> LLVM Operand
-- callReadFile arch T.GInt = do
--   let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
--   op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
--   caller intType (Right $ definedFunction intType (Name readFileInt)) [(op, [])]

-- callReadFile arch T.GFloat = do
--   let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
--   op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
--   caller doubleType (Right $ definedFunction doubleType (Name readFileDouble)) [(op, [])]

-- callReadFile arch T.GChar = do
--   let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
--   op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
--   caller charType (Right $ definedFunction charType (Name readFileChar)) [(op, [])]


-- callRead :: T.Type -> LLVM Operand
-- callRead T.GInt   =
--   caller intType   (Right $ definedFunction intType   (Name readIntStd))    []

-- callRead T.GChar  =
--   caller charType  (Right $ definedFunction charType  (Name readCharStd))   []

-- callRead T.GFloat =
--   caller floatType (Right $ definedFunction floatType (Name readDoubleStd)) []


-- createAssign :: AST -> AST -> LLVM String
-- createAssign id' exp = do
--   let id = convertId' id'
--   let ty = toType $ expType exp
--   e'  <- createExpression exp
--   op  <- checkVar id ty
--   res <- store ty op e'
--   return id


-- createMultyAssign :: AST -> String -> LLVM ()
-- createMultyAssign id aux = do
--   let ty = toType $ expType id
--   id'   <- getStoreDir id
--   e'    <- load aux ty
--   store ty id' e'
--   return ()

-- createArguments :: Map.Map TE.Text (Contents SymbolTable)
--                 -> [TE.Text] -> [AST] -> LLVM [Operand]
-- createArguments _ [] [] = return []
-- createArguments dicnp (nargp:nargps) (arg:args) = do
--   lr <- createArguments dicnp nargps args
--   let argt = argTypeArg $ fromJust $ Map.lookup nargp dicnp

--   case argt of
--     T.In -> do
--       arg' <- createExpression arg
--       return $ arg':lr
--     _    -> do
--       dicn <- use varsLoc
--       return $ fromJust (Map.lookup (TE.unpack $
--         fromJust $ AST.astToId arg) dicn) : lr


-- genGuards :: [AST] -> Name -> Name -> LLVM ()
-- genGuards [guard] none one = genGuard guard none

-- genGuards (guard:xs) none one = do
--   next <- newLabel
--   genGuard guard next
--   setLabel next $ branch one
--   genGuards xs none one


-- genGuard :: AST -> Name -> LLVM ()
-- genGuard (AST _ _ _ (Guard guard acc)) next = do
--   tag  <- createExpression guard
--   code <- newLabel
--   setLabel code $ condBranch tag code next
--   createInstruction acc

-- createState :: String -> AST -> LLVM ()
-- createState name (AST pos _ _ (States cond exp)) = do
--   e' <- createExpression exp
--   next     <- newLabel
--   warAbort <- newLabel

--   case cond of
--     AST.Pre        -> do let checkPre = "_resPre" ++ name
--                          op <- alloca Nothing boolType checkPre
--                          store boolType op e'
--                          addVarOperand checkPre op
--                          setLabel warAbort $ condBranch e' next warAbort
--                          createTagPre next pos

--     AST.Post       -> do let checkPre = "_resPre" ++ name
--                          op     <- load checkPre boolType
--                          a      <- addUnNamedInstruction boolType $ _not op
--                          check  <- addUnNamedInstruction boolType $ _or a e'
--                          setLabel warAbort $ condBranch check next warAbort
--                          createTagPost next pos

--     AST.Assertion -> do setLabel warAbort $ condBranch e' next warAbort
--                         createTagAssert next pos

--     AST.Invariant -> do setLabel warAbort $ condBranch e' next warAbort
--                         createTagInv next pos

--     AST.Bound     -> do checkZero <- newLabel
--                         warAbort' <- newLabel

--                         op    <- load name intType
--                         check <- addUnNamedInstruction intType $ _less e' op
--                         setLabel checkZero $ condBranch check checkZero warAbort

--                         check' <- addUnNamedInstruction intType $ _less e' $ constantInt 0
--                         var    <- getVarOperand name
--                         store intType var e'
--                         setLabel warAbort $ condBranch check' warAbort' next

--                         createTagBound warAbort' pos 1
--                         createTagBound next      pos 2
--   return ()

-- idToAlloca :: AST -> LLVM ()
-- idToAlloca (AST _ _ t (Id id)) = do
--   let id' = TE.unpack id
--   dim <- typeToOperand id' t
--   alloca dim (toType t) id'
--   return ()


-- typeToOperand :: String -> T.Type -> LLVM (Maybe Operand)
-- typeToOperand name (T.GArray dim ty) = do
--   r <- typeToOperand name ty
--   d <- dimToOperand dim
--   addDimToArray name d

--   case r of
--     Nothing -> return $ return d
--     Just op -> fmap Just $ addUnNamedInstruction intType $ _mul op d

-- typeToOperand _ _ = return Nothing
