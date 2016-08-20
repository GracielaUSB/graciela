{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Expression

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression                          (Expression (..),
                                                          Expression' (..),
                                                          Object, Value (..))
import qualified AST.Expression                          as Op (BinaryOperator (..),
                                                                UnaryOperator (..))
import           AST.Object                              as O (Object' (..),
                                                          Object'' (..))
import           LLVM.State
import           LLVM.Type                               (boolType, toLLVMType
                                                         ,intType, floatType)
import           Location
import           SymbolTable
import           Type                                    as T
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (when)
import           Data.Char                               (ord)
import           Data.Foldable                           (toList)
import           Data.Monoid                             ((<>))
import           Data.Sequence                           as Seq (ViewR ((:>)),
                                                                 empty, (|>),
                                                                 fromList,
                                                                 singleton,
                                                                 viewr)
import           Data.Text                               (unpack)
import           Data.Word
import           LLVM.General.AST                        (Definition(..))
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.Global                 as Global (Global(..),
                                                              globalVariableDefaults)
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as LLVM (SomeFloat (Double))
import qualified LLVM.General.AST.FloatingPointPredicate as F (FloatingPointPredicate (..))
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate (..))
import           LLVM.General.AST.Name                   (Name (..))
import           LLVM.General.AST.Operand                (CallableOperand,
                                                          Operand (..))
import           LLVM.General.AST.Type
import           Prelude                                 hiding (Ordering (..))
import Debug.Trace
--------------------------------------------------------------------------------

object :: Object -> LLVM Operand
object obj@Object { objType, obj' } = case obj' of
  -- If the variable is marked as In, mean it was passed to the 
  -- procedure as a constant so doesn't need to be loaded
  Variable { name, mode } | mode == Just In -> do
     return $ LocalReference (toLLVMType objType) $ Name (unpack name)

  -- If not marked as In, just load the content of the variable
  _ -> do
      label <- newLabel
      -- Make a reference to the variable that will be loaded (e.g. %a)
      addrToLoad <- objectRef obj
      
      let 
        -- Load the value of the variable address on a label (e.g. %12 = load i32* %a, align 4)
        load = Load { volatile  = False
                    , address   = addrToLoad
                    , maybeAtomicity = Nothing
                    , alignment = 4
                    , metadata  = []
                    }
        -- `ref` is the reference to where the variable value was loaded (e.g. %12)
        ref = LocalReference (toLLVMType objType) label

      addInstructions $ Seq.fromList [label := load]

      return ref

    -- Index { inner, index } ->
    --   index <- expression index

    


-- Get the reference to the object.
objectRef :: Object -> LLVM Operand
objectRef obj@(Object loc objType obj') = case obj' of
  
  Variable { name } -> 
    return $ LocalReference (toLLVMType objType) $ Name (unpack name)

  Index inner index -> do
    ref <- objectRef inner
    label <- newLabel
    index <- expression index
    let 
      getelemPtr = GetElementPtr  
            { inBounds = False
            , address  = ref
            , indices  = [ConstantOperand $ C.Int 32 0, index]
            , metadata = []}

    addInstruction $ label := getelemPtr
    return $ LocalReference (toLLVMType objType) $ label

  Deref inner -> do
    ref        <- objectRef inner
    labelLoad  <- newLabel
    labelCast  <- newLabel
    labelNull  <- newLabel
    labelCond  <- newLabel
    trueLabel  <- newLabel
    falseLabel <- newLabel
    let 
      Location (pos,_) = loc
      type' = toLLVMType objType
      load = Load { volatile  = False
                  , address   = ref
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = []
                  }
      
      {- Generate assembly to verify if a pointer is NULL, when accessing to the pointed memory.
         In that case, abort the program giving the source line of the bad access instead of letting
         the OS ends the process
      -}
      cast = PtrToInt { operand0 = LocalReference type' labelLoad
                      , type'    = intType
                      , metadata = []
                      }
      null = PtrToInt { operand0 = ConstantOperand $ C.Null $ ptr type'
                      , type'    = intType
                      , metadata = []
                      }
      cond = ICmp { iPredicate = EQ
                  , operand0   = LocalReference intType labelCast
                  , operand1   = LocalReference intType labelNull
                  , metadata   = []
                  }
      condBr = CondBr { condition = LocalReference boolType labelCond
                      , trueDest  = trueLabel
                      , falseDest = falseLabel
                      , metadata' = []
                      }

    addInstructions $ Seq.fromList [ labelLoad := load
                                   , labelCast := cast
                                   , labelNull := null
                                   , labelCond := cond]
    
    setLabel trueLabel $ Do condBr
    createTagNullPtr falseLabel pos

    return $ LocalReference type' $ labelLoad



  _ -> error "Aun no hay soporte para estructuras"

  where
    getIndices :: ([Operand], Maybe Operand) -> Object -> LLVM ([Operand], Maybe Operand)
    getIndices (indices,ref) (Object _ _ (Index inner index)) = do
      index' <- expression index
      getIndices (index':indices,ref) inner

    getIndices (indices,ref) obj = do
      ref <- objectRef obj
      return (reverse indices, Just ref)



expression :: Expression -> LLVM Operand
expression Expression { expType, exp'} = case exp' of
  Value val -> pure $ case val of
    BoolV  theBool  ->
      ConstantOperand $ C.Int 1 (if theBool then 1 else 0)
    CharV  theChar  ->
      ConstantOperand $ C.Int 8 . fromIntegral . ord $ theChar
    IntV   theInt   ->
      ConstantOperand $ C.Int 32 . fromIntegral $ theInt
    FloatV theFloat ->
      ConstantOperand $ C.Float $ LLVM.Double theFloat

  NullPtr -> 
    return $ ConstantOperand $ C.Null intType

  StringLit theString -> do 
    -- Get the length of the string
    let n  = fromIntegral $ length theString + 1
    -- Create an array type
    let t = ArrayType n i16
    name <- newLabel
    -- Create a global definition for the string
    let def = GlobalDefinition $ Global.globalVariableDefaults
                { Global.name        = name
                , Global.isConstant  = True
                , Global.type'       = t
                , Global.initializer = Just $ 
                    C.Array i16 [C.Int 16 (toInteger (ord c)) | c <- theString ++ "\0"] 
                }
    -- and add it to the module's definitions
    moduleDefs %= (Seq.|> def)
    return $ ConstantOperand $ C.GetElementPtr True (C.GlobalReference i16 name) [C.Int 64 0, C.Int 64 0]

  Obj obj -> object obj

  Unary unOp inner -> do
    innerOperand <- expression inner

    label <- newLabel
    let inst = case expType of
          T.GInt   -> opInt   unOp innerOperand
          T.GBool  -> opBool  unOp innerOperand
          T.GFloat -> opFloat unOp innerOperand
          t        -> error $ "tipo " <> show t <> " no soportado"

    let operand = LocalReference (toLLVMType expType) label
    addInstructions $ fromList [label := inst]
    return operand

    where
      opInt :: Op.UnaryOperator -> Operand -> Instruction
      opInt op innerOperand = case op of
        Op.Abs    -> undefined

        Op.UMinus -> Mul  { nsw      = False
                          , nuw      = False
                          , operand0 = innerOperand
                          , operand1 = ConstantOperand $ C.Int 32 (-1)
                          , metadata = []
                          }
        Op.Succ   -> Add  { nsw      = False
                          , nuw      = False
                          , operand0 = innerOperand
                          , operand1 = ConstantOperand $ C.Int 32 1
                          , metadata = []
                          }
        Op.Pred   -> Sub  { nsw      = False
                          , nuw      = False
                          , operand0 = innerOperand
                          , operand1 = ConstantOperand $ C.Int 32 1
                          , metadata = []
                          }

      opFloat :: Op.UnaryOperator -> Operand -> Instruction
      opFloat op innerOperand = case op of
        Op.Abs    -> callUnaryFunction fabsString innerOperand

        Op.UMinus -> FMul  { fastMathFlags = NoFastMathFlags
                           , operand0 = innerOperand
                           , operand1 = ConstantOperand $ C.Float $ LLVM.Double (-1.0)
                           , metadata = []
                           }
        Op.Sqrt   -> callUnaryFunction sqrtString innerOperand


      opBool :: Op.UnaryOperator -> Operand -> Instruction
      opBool op innerOperand = case op of
        Op.Not -> Xor { operand0 = innerOperand
                      , operand1 = ConstantOperand $ C.Int 1 (-1)
                      , metadata = []
                      }

      callUnaryFunction :: String -> Operand -> Instruction
      callUnaryFunction fun innerOperand =
        let
          funRef = Right . ConstantOperand $ C.GlobalReference i32 $ Name fun
        in Call { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = funRef
                , arguments          = [(innerOperand,[])]
                , functionAttributes = []
                , metadata           = []
                }



  Binary { binOp, lexpr, rexpr } -> do
    -- Operate both inner expression
    lOperand <- expression lexpr
    rOperand <- expression rexpr

    -- Get the type of the left expr. Used at bool operator to know the type when comparing.

    operand <- case expType of
          T.GInt   -> opInt   binOp lOperand rOperand
          T.GBool  -> opBool  binOp lOperand rOperand
          T.GFloat -> opFloat binOp lOperand rOperand

          t        -> error $ "tipo " <> show t <> " no soportado"

    
    
    return operand

    where
      -- LLVM offers a set of secure operations that know when an int operation reach an overflow
      -- llvm.sadd.with.overflow.i32 (fun == intAdd)
      -- llvm.ssub.with.overflow.i32 (fun == intSub)
      -- llvm.smul.with.overflow.i32 (fun == intMul)
      callFunction fun lOperand rOperand =
        let
          type' = StructureType False [i32, i1]
          funRef = Right . ConstantOperand $ C.GlobalReference i32 $ Name fun
        in Call { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = funRef
                , arguments          = [(lOperand,[]), (rOperand,[])]
                , functionAttributes = []
                , metadata           = []
                }


      opInt op lOperand rOperand = do
        label <- newLabel
        addInstructions $ case op of
          Op.Plus   -> Seq.singleton $ label := Add
                            { nsw      = False
                            , nuw      = False
                            , operand0 = lOperand
                            , operand1 = rOperand
                            , metadata = []
                            }
          Op.BMinus -> Seq.singleton $ label := Sub
                            { nsw      = False
                            , nuw      = False
                            , operand0 = lOperand
                            , operand1 = rOperand
                            , metadata = []
                            }
          Op.Times  -> Seq.singleton $ label := Mul
                            { nsw      = False
                            , nuw      = False
                            , operand0 = lOperand
                            , operand1 = rOperand
                            , metadata = []
                            }
          Op.Div    -> Seq.singleton $ label := SDiv
                            { exact = True
                            , operand0 = lOperand
                            , operand1 = rOperand
                            , metadata = []
                            }
          Op.Mod    -> Seq.singleton $ label := SRem
                            { operand0 = lOperand
                            , operand1 = rOperand
                            , metadata = []
                            }
          Op.Min    ->
            Seq.singleton $ label := callFunction minnumString lOperand rOperand

          Op.Max    ->
            Seq.singleton $ label := callFunction maxnumString  lOperand rOperand
          _         -> error "opFloat"
        return $ LocalReference intType label

      opFloat op lOperand rOperand = do
        label <- newLabel
        addInstructions $ case op of
          Op.Plus   -> Seq.singleton $ label := FAdd
                            { fastMathFlags = NoFastMathFlags
                            , operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
          Op.BMinus -> Seq.singleton $ label := FSub
                            { fastMathFlags = NoFastMathFlags
                            , operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
          Op.Times  -> Seq.singleton $ label := FMul
                            { fastMathFlags = NoFastMathFlags
                            , operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
          Op.Div   ->  Seq.singleton $ label := FDiv
                            { fastMathFlags = NoFastMathFlags
                            , operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
          Op.Power  ->
            Seq.singleton $ label := callFunction powString     lOperand rOperand

          Op.Min    ->
            Seq.singleton $ label := callFunction minnumFstring lOperand rOperand

          Op.Max    ->
            Seq.singleton $ label := callFunction maxnumFstring lOperand rOperand
          _         -> error "opFloat"
        return $ LocalReference floatType label

      opBool op lOperand rOperand = do
        label <- newLabel
        let Expression _ lType _ = lexpr
        case op of
          Op.And     -> do
            let inst = And  { operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
            addInstructions $ Seq.fromList [label := inst]
          Op.Or -> do
            let inst = Or { operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
            addInstructions $ Seq.fromList [label := inst]
          Op.BEQ -> do
            let inst = ICmp { iPredicate = EQ
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]
          Op.BNE -> do
            let inst = ICmp { iPredicate = NE
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]

          Op.AEQ | lType =:= GPointer GAny -> do
            labelCast1 <- newLabel
            labelCast2 <- newLabel
            let 
            {- Both pointer must be casted to integer to be compared -}
              cast1 = PtrToInt 
                        { operand0 = lOperand
                        , type'    = intType
                        , metadata = []
                        }
              ptr1 = LocalReference intType labelCast1

              cast2 = PtrToInt 
                        { operand0 = rOperand
                        , type'    = intType
                        , metadata = []
                        }
              ptr2 = LocalReference intType labelCast2

              comp = ICmp { iPredicate = EQ
                          , operand0   = ptr1
                          , operand1   = ptr2
                          , metadata   = []
                          }

            addInstructions $ Seq.fromList [ labelCast1 := cast1
                                           , labelCast2 := cast2
                                           , label  := comp]

          Op.AEQ -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.OEQ
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = EQ
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]

          Op.ANE | lType =:= GPointer GAny -> do
            labelCast1 <- newLabel
            labelCast2 <- newLabel
            let 
              {- Both pointer must be casted to integer to be compared -}
              cast1 = PtrToInt 
                        { operand0 = lOperand
                        , type'    = intType
                        , metadata = []
                        }
              ptr1 = LocalReference intType labelCast1

              cast2 = PtrToInt 
                        { operand0 = rOperand
                        , type'    = intType
                        , metadata = []
                        }
              ptr2 = LocalReference intType labelCast2

              comp = ICmp { iPredicate = NE
                          , operand0   = ptr1
                          , operand1   = ptr2
                          , metadata   = []
                          }

            addInstructions $ Seq.fromList [ labelCast1 := cast1
                                           , labelCast2 := cast2
                                           , label  := comp]

          Op.ANE | lType =:= GOneOf[GFloat, GInt, GChar] -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.ONE
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = NE
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]

          Op.LT -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.OLT
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = SLT
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]
          Op.LE -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.OLE
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = SLE
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]
          Op.GT -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.OGT
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = SGT
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]
          Op.GE -> do
            let inst = if lType =:= GFloat
                  then FCmp { fpPredicate = F.OGE
                            , operand0    = lOperand
                            , operand1    = rOperand
                            , metadata    = []
                            }
                  else ICmp { iPredicate = SGE
                            , operand0   = lOperand
                            , operand1   = rOperand
                            , metadata   = []
                            }
            addInstructions $ Seq.fromList [label := inst]

          Op.Implies    -> do
            -- p ==> q ≡ (p ⋁ q ≡ q)
            label' <- newLabel
            -- Operate p ⋁ q and save the result at label'
            let orInst = Or { operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
            let result = LocalReference boolType label'

            -- Operate the t ≡ q, where t is the previous result and save it in label            
            let equal = ICmp { iPredicate = EQ
                             , operand0   = result
                             , operand1   = rOperand
                             , metadata   = []
                             }
            addInstructions $ Seq.fromList [label' := orInst, label := equal]


          Op.Consequent -> do
            -- p <== q ≡ p ⋁ q ≡ p
            label' <- newLabel
            -- Operate p ⋁ q and save the result at label'
            let orInst = Or { operand0      = lOperand
                            , operand1      = rOperand
                            , metadata      = []
                            }
            let result = LocalReference boolType label'

            -- Operate the t ≡ q, where t is the previous result and save it in label
            let equal = ICmp { iPredicate = EQ
                             , operand0   = lOperand
                             , operand1   = result
                             , metadata   = []
                             }
            addInstructions $ Seq.fromList [label' := orInst, label := equal]
        return $ LocalReference boolType label

  -- Dummy operand
  _ -> return $ ConstantOperand $ C.Int 32 10



-- createExpression :: Expression -> LLVM Operand
-- createExpression (Expression loc expType constant exp') = case exp' of

--   {- Basic Types -}
--   BoolLit True  -> return $ constantBool  1
--   BoolLit False -> return $ constantBool  0
--   CharLit   c   -> return $ constantChar  c
--   IntLit    i   -> return $ constantInt   i
--   FloatLit  f   -> return $ constantFloat f
--   String   str  -> addStringOpe str

--   Id name -> do
--     var <- use varsLoc
--     let (n, ty) = (TE.unpack name, toType t)
--     let check   = DM.lookup n var

--     case check of
--       Just add -> case t of
--         T.GArray _ _ -> return add
--         _            -> load n ty
--       Nothing -> return $ local ty (Name n)


--   Cond lguards -> do
--     final  <- newLabel
--     abort  <- newLabel
--     lnames <- genExpGuards lguards abort final
--     let t' = toType t
--     setLabel abort $ branch final
--     createTagIf final posFrom
--     condName .= final
--     addUnNamedInstruction t' $ Phi t' lnames []


--   ArrCall name' accs -> do
--     accs' <- mapM createExpression accs
--     map   <- use varsLoc
--     let (t', i, name) = (toType t, fromJust $ DM.lookup name map, TE.unpack name')
--     accs'' <- opsToArrayIndex name accs'
--     add    <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
--     addUnNamedInstruction t' $ Load False add Nothing 0 []

--   Conversion tType expr -> do
--     let t' = astType expr
--     expr'  <- createExpression expr

--     if t' == T.GChar && tType == ToInt
--       then do
--         op <- intToDouble expr'
--         doubleToInt op
--       else
--         addUnNamedInstruction (toType t) $ irConversion tType t' expr'


--   Arithmetic op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     case op of
--       Exp -> do a   <- intToDouble lexp'
--                 b   <- intToDouble rexp'
--                 val <- addUnNamedInstruction floatType $
--                        irArithmetic Exp T.GFloat a b
--                 doubleToInt val
--       Max -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
--       Min -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
--       Div -> checkDivZero  op posFrom lexp' rexp' t
--       Mod -> checkDivZero  op posFrom lexp' rexp' t
--       _   -> case t of
--         T.GInt   -> checkOverflow op posFrom lexp' rexp' t
--         T.GFloat -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'


--   Boolean op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     case op of
--       Implies    -> do
--         notA <- addUnNamedInstruction boolType $ _not lexp'
--         addUnNamedInstruction boolType $ _or notA rexp'

--       Consequent -> do
--         notA <- addUnNamedInstruction boolType $ _not rexp'
--         addUnNamedInstruction boolType $ _or notA lexp'

--       _ -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'


--   Relational op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     let t' = astType lexp
--     addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


--   Unary Abs exp | t == T.GInt -> do
--     exp'  <- createExpression exp
--     x     <- intToDouble exp'
--     val   <- addUnNamedInstruction intType $ irUnary Abs T.GFloat x
--     doubleToInt val


--   Unary Sqrt exp -> do
--     let ty = astType exp
--     let df = Right $ definedFunction floatType (Name sqrtString)
--     exp'  <- createExpression exp
--     case ty of
--       T.GFloat ->
--         addUnNamedInstruction floatType $ irUnary Sqrt ty exp'
--       T.GInt   -> do
--         x <- intToDouble exp'
--         addUnNamedInstruction floatType $ irUnary Sqrt T.GFloat x


--   Unary op exp -> do
--     exp' <- createExpression exp
--     addUnNamedInstruction (toType t) $ irUnary op t exp'


--   FCallExp fname st args -> do
--     exp     <- mapM createExpression args
--     let ty   =  toType t
--     let exp' = map (\i -> (i,[])) exp
--     let op   = definedFunction ty (Name $ TE.unpack fname)
--     caller ty (Right op) exp'


-- {- Comentado porque no estoy seguro de esto -}

-- -- createExpression (AST.QuantRan opQ varQ pos rangeExp termExp t) = do
-- --   let name  = TE.unpack varQ
-- --   let tyExp = AST.qVarType termExp

-- --   case opQ of
-- --     AST.ForAll -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp

-- --     AST.Exists -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp

-- --     _          -> do  check <- mapM (createQuant False opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp


-- -- createExpression (AST.QuantRanUn opQ varQ pos rangeExp termExp t) = do
-- --   let name  = TE.unpack varQ
-- --   let tyExp = AST.qVarType termExp

-- --   ranges  <- doRange opQ rangeExp pos
-- --   rangesF <- makeRanges ranges

-- --   case opQ of
-- --     AST.ForAll -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp

-- --     AST.Exists -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp

-- --     _          -> do  check <- mapM (createQuant' False opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp


-- genExpGuards :: [AST] -> Name -> Name -> LLVM [(Operand, Name)]
-- genExpGuards [guard] none one  = do
--   r <- genExpGuard guard none
--   return [r]

-- genExpGuards (guard:xs) none one = do
--   next <- newLabel
--   r    <- genExpGuard guard next
--   setLabel next $ branch one
--   rl   <- genExpGuards xs none one
--   return $ r:rl


-- createGuardExp :: AST -> Name -> LLVM (Operand, Name)
-- {- Ya estaba comentado (?)-}
-- --createGuardExp (AST.Cond lguards _ rtype) _ = do
-- --    final  <- newLabel
-- --    none   <- newLabel
-- --    lnames <- genExpGuards lguards none final
-- --    let rtype = toType rtype
-- --    setLabel none $ branch final
-- --    setLabel final $ Do $ Unreachable []
-- --    n <- addUnNamedInstruction rtype' $ Phi rtype' lnames []
-- --    return (n, final)

-- createGuardExp acc code = do
--   exp   <- createExpression acc
--   label <- use condName
--   return (exp, label)


-- genExpGuard :: AST -> Name -> LLVM (Operand, Name)
-- genExpGuard (AST _ _ _ (Guard guard acc)) next = do
--   tag  <- createExpression guard
--   code <- newLabel
--   condName .= code
--   setLabel code $ condBranch tag code next
--   createGuardExp acc code

-- checkDivZero :: BinaryOperator -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
-- checkDivZero op pos lexp' rexp' ty = do
--   next  <- newLabel
--   abort <- newLabel
--   condName .= next

--   case ty of
--     T.GInt   -> do
--       let zero = constantInt 0
--       check <- addUnNamedInstruction intType $ ICmp IL.EQ rexp' zero []
--       setLabel abort $ condBranch check abort next
--       createTagZero next pos
--       addUnNamedInstruction intType $ irArithmetic op ty lexp' rexp'

--     T.GFloat -> do
--       let zero = constantFloat 0.0
--       check <- addUnNamedInstruction floatType $ FCmp FL.OEQ rexp' zero []
--       setLabel abort $ condBranch check abort next
--       createTagZero next pos
--       addUnNamedInstruction floatType $ irArithmetic op ty lexp' rexp'


-- checkOverflow :: BinaryOperator -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
-- checkOverflow op pos lexp rexp ty = do
--   overAbort <- newLabel
--   next      <- newLabel
--   res       <- addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp rexp
--   check     <- extracValue res 1

--   setLabel overAbort $ condBranch check overAbort next
--   createTagOverflow next pos

--   condName .= next
--   extracValue res 0

-- intToDouble :: Operand -> LLVM Operand
-- intToDouble x = addUnNamedInstruction floatType $ _toFloat x


-- doubleToInt :: Operand -> LLVM Operand
-- doubleToInt x = addUnNamedInstruction intType $ _toInt x


-- irArithmetic :: BinaryOperator -> T.Type -> Operand -> Operand -> Instruction
-- --irArithmetic AST.Sum T.GInt   a b = _add  a b
-- --irArithmetic AST.Sub T.GInt   a b = Sub False False a b []
-- --irArithmetic AST.Mul T.GInt   a b = _mul a b
-- irArithmetic Sum T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intAdd))) [(a, []),(b, [])] [] []
-- irArithmetic Sub T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intSub))) [(a, []),(b, [])] [] []
-- irArithmetic Mul T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intMul))) [(a, []),(b, [])] [] []
-- irArithmetic Sum T.GFloat a b = _addF   a b
-- irArithmetic Mul T.GFloat a b = _mulF   a b
-- irArithmetic Sub T.GFloat a b = FSub NoFastMathFlags a b []
-- irArithmetic Div T.GInt   a b = SDiv True a b []
-- irArithmetic Div T.GFloat a b = FDiv NoFastMathFlags a b []
-- irArithmetic Mod T.GInt   a b = URem a b []
-- irArithmetic Mod T.GFloat a b = FRem NoFastMathFlags a b []
-- irArithmetic Exp T.GFloat a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                     (Name powString)))    [(a, []),(b, [])] [] []
-- irArithmetic Min T.GFloat a b = _minF a b
-- irArithmetic Max T.GFloat a b = _maxF a b
-- irArithmetic Max T.GInt   a b = _max  a b
-- irArithmetic Min T.GInt   a b = _min  a b


-- irBoolean :: BinaryOperator -> Operand -> Operand -> Instruction
-- irBoolean And a b = _and a b
-- irBoolean Or a b = _or  a b


-- irRelational :: BinaryOperator -> T.Type -> Operand -> Operand -> Instruction
-- irRelational AEQ  T.GFloat a b = FCmp FL.OEQ a b []
-- irRelational LT   T.GFloat a b = FCmp FL.OLT a b []
-- irRelational GT   T.GFloat a b = FCmp FL.OGT a b []
-- irRelational LE   T.GFloat a b = FCmp FL.OLE a b []
-- irRelational GE   T.GFloat a b = FCmp FL.OGE a b []
-- irRelational ANE  T.GFloat a b = FCmp FL.ONE a b []


-- irRelational AEQ  T.GInt   a b = ICmp IL.EQ  a b []
-- irRelational LT   T.GInt   a b = _less   a b
-- irRelational GT   T.GInt   a b = ICmp IL.SGT a b []
-- irRelational LE   T.GInt   a b = _lequal a b
-- irRelational GE   T.GInt   a b = ICmp IL.SGE a b []
-- irRelational ANE  T.GInt   a b = ICmp IL.NE  a b []


-- irConversion :: Conversion -> T.Type -> Operand -> Instruction
-- irConversion ToInt    T.GFloat a = _toInt   a
-- irConversion ToInt    T.GBoolean
--           (ConstantOperand (C.Int 1 0)) = _toInt $ constantInt 0
-- irConversion ToInt    T.GBoolean
--           (ConstantOperand (C.Int 1 1)) = _toInt $ constantInt 1
-- irConversion ToDouble T.GInt   a = _toFloat a
-- irConversion ToDouble T.GChar  a = _toFloat a
-- irConversion ToChar   T.GInt   a = Trunc  a charType  []
-- irConversion ToChar   T.GFloat a = FPToSI a charType  []


-- irUnary :: UnaryOperator -> T.Type -> Operand -> Instruction
-- irUnary Minus T.GInt   a = Sub False False      (constantInt 0) a []
-- irUnary Minus T.GFloat a = FSub NoFastMathFlags (constantFloat 0) a []
-- irUnary Abs   T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                      (Name fabsString))) [(a, [])] [] []
-- irUnary Sqrt  T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                      (Name sqrtString))) [(a, [])] [] []
-- irUnary Not   T.GBoolean  a = _not a


-- _and    a b = And a b []
-- _not    a   = Xor a (constantBool 1) []
-- _or     a b = Or  a b []
-- _less   a b = ICmp IL.SLT a b []
-- _lequal a b = ICmp IL.SLE a b []
-- _add    a b = Add False False a b []
-- _addF   a b = FAdd NoFastMathFlags a b []
-- _mul    a b = Mul False False a b []
-- _mulF   a b = FMul NoFastMathFlags a b []
-- _min    a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                          (Name minnumString)))  [(a, []),(b, [])] [] []
-- _minF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                          (Name minnumFstring))) [(a, []),(b, [])] [] []
-- _max    a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                          (Name maxnumString)))  [(a, []),(b, [])] [] []
-- _maxF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                          (Name maxnumFtring)))  [(a, []),(b, [])] [] []
-- _toFloat a = SIToFP a floatType []
-- _toInt   a = FPToSI a intType   []
