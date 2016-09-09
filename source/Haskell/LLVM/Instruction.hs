{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}

module LLVM.Instruction where
--------------------------------------------------------------------------------
import           AST.Expression                     (Expression (..))
import qualified AST.Expression                     as E (Expression' (..))
import           AST.Instruction                    (Guard, Instruction (..),
                                                     Instruction' (..))
import qualified AST.Instruction                    as G (Instruction)
import           AST.Object                         (Object' (..),
                                                     Object'' (..))
import           LLVM.Abort                         (abort)
import qualified LLVM.Abort                         as Abort (Abort (Assert, If, Invariant, Manual, NegativeBound, NondecreasingBound))
import           LLVM.Declaration
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                       (warn)
import qualified LLVM.Warning                       as Warning (Warning (Manual))
import           Location
import           Treelike
import           AST.Type                               as T
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (-=), (.=))
import           Control.Monad                      (foldM, when, zipWithM_)
import           Data.Foldable                      (toList)
import           Data.Monoid                        ((<>))
import           Data.Sequence                      (ViewR ((:>)))
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton, viewr,
                                                            zip, (|>))
import           Data.Text                          (unpack)
import           Data.Word
import           LLVM.General.AST                   (BasicBlock (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import           LLVM.General.AST.Instruction       (FastMathFlags (..),
                                                     Instruction (..),
                                                     Named (..),
                                                     Terminator (..))
import qualified LLVM.General.AST.Instruction       as LLVM (Instruction)
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type
--------------------------------------------------------------------------------
import           Debug.Trace

guard :: Name -> Name -> Guard -> LLVM Name
guard finish checkLabel (expr, decls, insts) = do
  (checkLabel #)

  yes <- newLabel "instGuardYes"
  no  <- newLabel "instGuardNo"

  condition <- expression expr

  terminate' CondBr
    { condition
    , trueDest  = yes
    , falseDest = no
    , metadata' = [] }

  (yes #)
  openScope
  mapM_ declaration decls
  mapM_ instruction insts
  terminate' Br
    { dest      = finish
    , metadata' = [] }

  closeScope
  pure no


instruction :: G.Instruction -> LLVM ()
instruction i@Instruction {instLoc=Location(pos, _), inst'} = case inst' of
  Abort -> do
    abort Abort.Manual pos
    newLabel "unreachable" >>= (#)

  Warn -> do
    warn Warning.Manual pos

  Assertion expr -> do
    -- Evaluate the condition expression
    cond <- expression expr
    -- Create both label
    trueLabel  <- newLabel "assertTrue"
    falseLabel <- newLabel "assertFalse"
    -- Create the conditional branch
    terminate' CondBr
      { condition = cond
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = []
      }
    -- Set the false label to the abort
    -- And the true label to the next instructions
    (falseLabel #)
    abort Abort.Assert pos

    (trueLabel #)


  Assign { assignPairs } -> do
    -- get the values first
    values <- mapM expression exprs
    -- then store them
    zipWithM_ assign' lvals values
    -- this way, things like `a, b := b, a` just work (tm).

    where
      (lvals, exprs) = unzip . toList $ assignPairs
      assign' lval value = do
        ref <- objectRef lval
        type' <- toLLVMType . objType $ lval
        addInstruction $ Do Store
          { volatile       = False
          , address        = ref
          , value
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }


  Conditional { cguards } -> do
    entry  <- newLabel "ifExpEntry"
    finish <- newLabel "ifExpFinish"

    terminate' Br
      { dest      = entry
      , metadata' = [] }

    abortLabel <- foldM (guard finish) entry cguards

    (abortLabel #)
    abort Abort.If pos

    (finish #)


  Block decls insts -> do
    block <- newLabel "blockEntry"
    exit <- newLabel "blockExit"
    terminate' Br
      { dest      = block
      , metadata' = [] }

    (block #)
    openScope
    mapM_ declaration decls
    mapM_ instruction insts
    closeScope
    terminate' Br
      { dest      = exit
      , metadata' = [] }

    (exit #)

  ProcedureCall { pName, pArgs } -> do
    args <- mapM createArg pArgs
    addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType $ unpack pName
      , arguments          = toList args
      , functionAttributes = []
      , metadata           = [] }
    where
      -- Out and InOut arguments need to be passed as pointers to, so the address has to be casted
      -- If it is not an Out or InOut argument, then just pass a constant value.
      -- only basic types or pointers (because a pointer is just an integer) can be passed as a constant value.
      createArg (e,mode) =
        (,[]) <$> if mode == In && (expType e =:= basicT)
          then expression e
          else do
            label <- newLabel "argCast"
            ref   <- objectRef . E.theObj . exp' $ e
            type' <- ptr <$> (toLLVMType . expType $ e)
            addInstruction $ label := BitCast
              { operand0 = ref
              , type'    = type'
              , metadata = [] }
            pure $ LocalReference type' label
      basicT = T.GOneOf [T.GBool,T.GChar,T.GInt,T.GFloat]

  Free { idName, freeType } -> do
    labelLoad  <- newLabel "freeLoad"
    labelCast  <- newLabel "freeCast"
    labelNull  <- newLabel "freeNull"
    ref        <- objectRef idName
    type' <- toLLVMType (T.GPointer freeType)

    addInstruction $ labelLoad := Load
      { volatile  = False
      , address   = ref
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = [] }

    addInstruction $ labelCast := BitCast
      { operand0 = LocalReference type' labelLoad
      , type'    = PointerType i8 (AddrSpace 0)
      , metadata = [] }

    addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType freeString
      , arguments          = [(LocalReference type' labelCast,[])]
      , functionAttributes = []
      , metadata           = [] }

    addInstruction $ Do Store
      { volatile = False
      , address  = ref
      , value    = ConstantOperand $ C.Null type'
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = [] }

  New { idName,nType } -> do
    labelCall  <- newLabel "newCall"
    labelCast  <- newLabel "newCast"
    ref        <- objectRef idName -- The variable that is being mallocated
    type'      <- toLLVMType (T.GPointer nType)

    addInstruction $ labelCall := Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable pointerType mallocString
      , arguments          = [(ConstantOperand $ C.Int 32 (sizeOf nType),[])]
      , functionAttributes = []
      , metadata           = [] }

    addInstruction $ labelCast := BitCast
      { operand0 = LocalReference pointerType labelCall
      , type'    = type'
      , metadata = [] }

      -- Store the casted pointer in the variable
    addInstruction $ Do Store
      { volatile = False
      , address  = ref
      , value    = LocalReference type' labelCast
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = [] }


  Write { ln, wexprs } -> do
    operands <- mapM expression wexprs
    mapM_ write (Seq.zip operands (expType <$> wexprs))
    when ln . addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType lnString
      , arguments          = []
      , functionAttributes = []
      , metadata           = [] }
    where
      write (operand, t) = do
        -- Build the operand of the expression
        let
        -- Call the correct C write function
          fun = callable voidType $ case t of
            T.GBool   -> writeBString
            T.GChar   -> writeCString
            T.GFloat  -> writeFString
            T.GInt    -> writeIString
            T.GString -> writeSString
            _         -> error
              "internal error: attempted to write non-basic type."
        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = fun
          , arguments          = [(operand, [])]
          , functionAttributes = []
          , metadata           = []}


  Read { file, vars } -> case file of
    Nothing -> mapM_ readVarStdin vars
    Just file' -> error "No se puede con archivos"

    where
      readVarStdin var = do
        let
          t = objType var

          fread = case t of
            T.GChar   -> readCharStd
            T.GFloat  -> readFloatStd
            T.GInt    -> readIntStd
            _         -> error ":D no se soporta este tipo: " <> show t

        -- Call the C read function
        type' <- toLLVMType t
        readResult <- newLabel "readCall"
        addInstruction $ readResult := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable type' fread
          , arguments          = []
          , functionAttributes = []
          , metadata           = [] }

        -- Get the reference of the variable's memory
        objRef <- objectRef var
        -- Store the value saved at `readResult` in the variable memory
        addInstruction $ Do Store
          { volatile = False
          , address  = objRef
          , value    = LocalReference type' readResult
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }


  Repeat { rguards, rinv, rbound } -> do
    begin     <- newLabel "doBegin"
    again     <- newLabel "doAgain"
    checkGte0 <- newLabel "doCheckGte0"
    n         <- newLabel "doN"

    terminate' Br
      { dest      = begin
      , metadata' = [] }

    (begin #)
    addInstruction $ n := Alloca
      { allocatedType = intType
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = [] }

    boundVal0 <- expression rbound
    Just begin' <- use blockName
    terminate' Br
      { dest      = checkGte0
      , metadata' = [] }

    (again #)
    boundVal1 <- expression rbound
    oldBound <- newLabel "doOldBound"
    addInstruction $ oldBound := Load
      { volatile = False
      , address = LocalReference intType n
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata = [] }
    ltOld <- newLabel "doLtOld"
    addInstruction $ ltOld := ICmp
      { iPredicate = SLT
      , operand0   = boundVal1
      , operand1   = LocalReference intType oldBound
      , metadata   = [] }
    noLtOld <- newLabel "doNoLtOld"

    Just again' <- use blockName
    terminate' CondBr
      { condition = LocalReference boolType ltOld
      , trueDest  = checkGte0
      , falseDest = noLtOld
      , metadata' = [] }

    (noLtOld #)
    abort Abort.NondecreasingBound pos

    (checkGte0 #)
    boundVal <- newLabel "doBound"
    addInstruction $ boundVal := Phi
      { type' = intType
      , incomingValues =
        [ (boundVal0, begin')
        , (boundVal1, again') ]
      , metadata = [] }
    gte0 <- newLabel "doGte0"
    addInstruction $ gte0 := ICmp
      { iPredicate = SGE
      , operand0   = LocalReference intType boundVal
      , operand1   = ConstantOperand $ C.Int 32 0
      , metadata   = [] }

    yesGte0 <- newLabel "doGte0Yes"
    noGte0  <- newLabel "doGte0No"
    terminate' CondBr
      { condition = LocalReference boolType gte0
      , trueDest  = yesGte0
      , falseDest = noGte0
      , metadata' = [] }

    (noGte0 #)
    abort Abort.NegativeBound pos

    (yesGte0 #)
    invVal <- expression rinv
    yesInv <- newLabel "doInvYes"
    noInv  <- newLabel "doInvNo"
    terminate' CondBr
      { condition = invVal
      , trueDest  = yesInv
      , falseDest = noInv
      , metadata' = [] }

    (noInv #)
    abort Abort.Invariant pos

    (yesInv #)
    addInstruction $ Do Store
      { volatile       = False
      , address        = LocalReference intType n
      , value          = LocalReference intType boundVal
      , maybeAtomicity = Nothing
      , alignment      = 4
      , metadata       = [] }

    firstGuard <- newLabel "doGuards"
    terminate' Br
      { dest      = firstGuard
      , metadata' = [] }

    exit <- foldM (guard again) firstGuard rguards

    (exit #)

  Skip -> pure ()

  _ -> do
    traceM . drawTree . toTree $ i
    traceM "I don't know how to generate code for:"
    error "Unimplemented instruction"


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
--       T.GInt      -> procedureCall ty' writeIString    [e']
--       T.GFloat    -> procedureCall ty' writeDouble [e']
--       T.GBoolean  -> procedureCall ty' writeBString   [e']
--       T.GChar     -> procedureCall ty' writeCString   [e']
--       T.GEmpty    -> procedureCall ty' writeSString [e']
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