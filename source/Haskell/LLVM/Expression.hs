{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module LLVM.Expression

where
--------------------------------------------------------------------------------
import           AST.Expression                          (Expression (..),
                                                          Expression' (..),
                                                          Object, Value (..))
import qualified AST.Expression                          as Op (BinaryOperator (..),
                                                                UnaryOperator (..))
import           AST.Object                              (Object' (..),
                                                          Object'' (..))
import           AST.Type                                as T
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (DivisionByZero, If, NullPointerAccess, Overflow))
import           LLVM.Monad
import           LLVM.Quantification                     (quantification)
import           LLVM.State
import           LLVM.Type                               (boolType, floatType,
                                                          intType, toLLVMType)
import           Location
import           SymbolTable
import           Treelike                                (drawTree, toTree)
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (foldM, when)
import           Data.Array                              ((!))
import           Data.Char                               (ord)
import           Data.Foldable                           (toList)
import           Data.Maybe                              (fromMaybe)
import           Data.Monoid                             ((<>))
import           Data.Sequence                           ((|>))
import qualified Data.Sequence                           as Seq (ViewR ((:>)),
                                                                 empty,
                                                                 fromList,
                                                                 singleton,
                                                                 viewr)
import           Data.Text                               (unpack)
import           Data.Word                               (Word32)
import           LLVM.General.AST                        (Definition (..))
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as LLVM (SomeFloat (Double))
import qualified LLVM.General.AST.FloatingPointPredicate as F (FloatingPointPredicate (..))
import qualified LLVM.General.AST.Global                 as Global (Global (..), globalVariableDefaults)
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate (..))
import           LLVM.General.AST.Name                   (Name (..))
import           LLVM.General.AST.Operand                (CallableOperand,
                                                          Operand (..))
import           LLVM.General.AST.Type
import           LLVM.General.AST.Type                   as L (i64)
import           Prelude                                 hiding (Ordering (..))
--------------------------------------------------------------------------------
import           Debug.Trace

object :: Object -> LLVM Operand
object obj@Object { objType, obj' } = case obj' of
  -- If the variable is marked as In, mean it was passed to the
  -- procedure as a constant so doesn't need to be loaded
  Variable { mode } | mode == Just In -> objectRef obj

  -- If not marked as In, just load the content of the variable
  _ -> do
      label <- newLabel "varObj"
      -- Make a reference to the variable that will be loaded (e.g. %a)
      addrToLoad <- objectRef obj
      t <- toLLVMType objType

      let
        -- Load the value of the variable address on a label (e.g. %12 = load i32* %a, align 4)
        load = Load { volatile  = False
                    , address   = addrToLoad
                    , maybeAtomicity = Nothing
                    , alignment = 4
                    , metadata  = []
                    }
        -- `ref` is the reference to where the variable value was loaded (e.g. %12)
        ref = LocalReference t label

      addInstructions $ Seq.fromList [label := load]

      return ref


-- Get the reference to the object.
objectRef :: Object -> LLVM Operand
objectRef obj@(Object loc objType obj') = do
  objType' <- toLLVMType objType
  case obj' of

    Variable { name } -> do
      name' <- getVariableName name
      pure $ LocalReference objType' name'

    Index inner index -> do
      ref <- objectRef inner
      label <- newLabel "idx"
      index <- expression index
      let
        getelemPtr = GetElementPtr
              { inBounds = False
              , address  = ref
              , indices  = [ConstantOperand $ C.Int 32 0, index]
              , metadata = []}

      addInstruction $ label := getelemPtr
      return . LocalReference objType' $ label

    Deref inner -> do
      ref        <- objectRef inner
      labelLoad  <- newLabel "derefLoad"
      labelCast  <- newLabel "derefCast"
      labelNull  <- newLabel "derefNull"
      labelCond  <- newLabel "derefCond"
      trueLabel  <- newLabel "derefNullTrue"
      falseLabel <- newLabel "derefNullFalse"
      let
        Location (pos,_) = loc
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
        cast = PtrToInt { operand0 = LocalReference objType' labelLoad
                        , type'    = intType
                        , metadata = []
                        }
        null = PtrToInt { operand0 = ConstantOperand . C.Null $ ptr objType'
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

      terminate' condBr

      (trueLabel #)
      abort Abort.NullPointerAccess pos

      (falseLabel #)

      return . LocalReference objType' $ labelLoad


    Member { inner, field } -> do
      ref <- objectRef inner
      label <- newLabel $ "member" <> show field
      addInstruction $ label := GetElementPtr
          { inBounds = False
          , address  = ref
          , indices  = ConstantOperand . C.Int 32 <$> [0, field]
          , metadata = []}
      pure . LocalReference objType' $ label

  where
    getIndices :: ([Operand], Maybe Operand) -> Object -> LLVM ([Operand], Maybe Operand)
    getIndices (indices,ref) (Object _ _ (Index inner index)) = do
      index' <- expression index
      getIndices (index':indices,ref) inner

    getIndices (indices,ref) obj = do
      ref <- objectRef obj
      return (reverse indices, Just ref)



-- LLVM offers a set of secure operations that know when an int operation reach an overflow
-- llvm.sadd.with.overflow.i32 (fun == intAdd)
-- llvm.ssub.with.overflow.i32 (fun == intSub)
-- llvm.smul.with.overflow.i32 (fun == intMul)
safeOperation n label fun lOperand rOperand pos = do
  labelOp      <- newLabel "safeOp"
  labelCond     <- newLabel "safeCond"
  overflowLabel <- newLabel "safeOverflow"
  normalLabel   <- newLabel "safeOk"

  let
    safeStruct = StructureType False [IntegerType n, boolType]

  addInstruction $ labelOp  := Call
    { tailCallKind       = Nothing
    , callingConvention  = CC.C
    , returnAttributes   = []
    , function           = callable (IntegerType n) (fun n)
    , arguments          = [(lOperand,[]), (rOperand,[])]
    , functionAttributes = []
    , metadata           = [] }

  addInstruction $ label     := ExtractValue
    { aggregate = LocalReference safeStruct labelOp
    , indices'  = [0]
    , metadata  = [] }

  addInstruction $ labelCond := ExtractValue
    { aggregate = LocalReference safeStruct labelOp
    , indices'  = [1]
    , metadata  = [] }

  terminate' CondBr
    { condition = LocalReference boolType labelCond
    , trueDest  = overflowLabel
    , falseDest = normalLabel
    , metadata' = [] }

  (overflowLabel #)
  abort Abort.Overflow pos

  (normalLabel #)


callUnaryFunction :: String -> Operand -> Instruction
callUnaryFunction fun innerOperand = Call
  { tailCallKind       = Nothing
  , callingConvention  = CC.C
  , returnAttributes   = []
  , function           = callable i32 fun
  , arguments          = [(innerOperand,[])]
  , functionAttributes = []
  , metadata           = [] }

-- callfFunction :: Word32 -> String -> Operand -> Operand -> Instruction
-- callfFunction n fun lOperand rOperand =

expression :: Expression -> LLVM Operand
expression e@(Expression loc@(Location(pos,_)) expType exp') = case exp' of
  Value val -> pure $ case val of
    BoolV  theBool  ->
      ConstantOperand $ C.Int 1 (if theBool then 1 else 0)
    CharV  theChar  ->
      ConstantOperand . C.Int 8 . fromIntegral . ord $ theChar
    IntV   theInt   ->
      ConstantOperand . C.Int 32 . fromIntegral $ theInt
    FloatV theFloat ->
      ConstantOperand . C.Float $ LLVM.Double theFloat

  NullPtr ->
    pure . ConstantOperand $ C.Null i8

  StringLit { theStringId } ->
    (! theStringId) <$> use stringOps

  Obj obj -> object obj

  Unary unOp inner -> do
    innerOperand <- expression inner

    operand <- case expType of
      T.GInt   -> opInt 32  unOp innerOperand
      T.GChar  -> opInt 8  unOp innerOperand
      T.GBool  -> opBool  unOp innerOperand
      T.GFloat -> opFloat unOp innerOperand
      t        -> error $ "tipo " <> show t <> " no soportado"

    pure operand

    where
      opInt :: Word32 -> Op.UnaryOperator -> Operand -> LLVM Operand
      opInt n op innerOperand = do
        label <- newLabel $ case n of
          32 -> "unaryIntOp"
          8  -> "unaryCharOp"
          _  -> error "internal error: badUnaryIntOp"
        let
          minusOne = ConstantOperand $ C.Int 32 (-1)
          one = ConstantOperand $ C.Int n 1
        case op of
            Op.Abs    -> undefined -- TODO

            Op.UMinus ->
              safeOperation n label safeMul innerOperand minusOne pos

            Op.Succ   ->
              safeOperation n label safeAdd innerOperand one pos

            Op.Pred   ->
              safeOperation n label safeSub innerOperand one pos

        return $ LocalReference (IntegerType n) label

      opFloat :: Op.UnaryOperator -> Operand -> LLVM Operand
      opFloat op innerOperand = do
        label <- newLabel "opFloat"
        let
          insts = case op of
            Op.Abs    -> Seq.singleton $
              label := callUnaryFunction fabsString innerOperand

            Op.UMinus -> Seq.singleton $ label := FMul
                    { fastMathFlags = NoFastMathFlags
                    , operand0 = innerOperand
                    , operand1 = ConstantOperand . C.Float $ LLVM.Double (-1.0)
                    , metadata = []}

            Op.Sqrt   -> Seq.singleton $
              label := callUnaryFunction sqrtString innerOperand

        addInstructions insts
        return $ LocalReference floatType label

      opBool :: Op.UnaryOperator -> Operand -> LLVM Operand
      opBool op innerOperand = do
        label <- newLabel "opBool"
        let
          insts =  case op of
            Op.Not -> Seq.singleton $ label := Xor
                { operand0 = innerOperand
                , operand1 = ConstantOperand $ C.Int 1 (-1)
                , metadata = []
                }
        addInstructions insts
        return $ LocalReference boolType label

      callUnaryFunction :: String -> Operand -> Instruction
      callUnaryFunction fun innerOperand =
        let
          funRef = callable i32 fun
        in Call { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = funRef
                , arguments          = [(innerOperand,[])]
                , functionAttributes = []
                , metadata           = []
                }

  Binary { binOp, lexpr, rexpr } -> do
    -- Evaluate both expressions
    lOperand <- expression lexpr
    rOperand <- expression rexpr

    -- Get the type of the left expr. Used at bool operator to know the type when comparing.
    let
      op = case expType of
        T.GInt   -> opInt 32
        T.GChar  -> opInt 8
        T.GBool  -> opBool
        T.GFloat -> opFloat
        t        -> error $
          "internal error: type " <> show t <> " not supported"

    op binOp lOperand rOperand

    where
      opInt n op lOperand rOperand = do
        label <- newLabel $ case n of
          32 -> "intOp"
          8  -> "charOp"
          _  -> error "internal error: badIntOp"
        case op of
          Op.Plus   ->
            safeOperation n label safeAdd lOperand rOperand pos

          Op.BMinus ->
            safeOperation n label safeSub lOperand rOperand pos

          Op.Times  ->
            safeOperation n label safeMul lOperand rOperand pos

          Op.Div    -> do
            checkZero <- newLabel "divCheckZero"
            addInstruction $ checkZero := ICmp
              { iPredicate = EQ
              , operand0   = ConstantOperand $ C.Int n 0
              , operand1   = rOperand
              , metadata   = [] }

            isZero <- newLabel "divIsZero"
            isn'tZero <- newLabel "divIsn'tZero"
            terminate' CondBr
              { condition = LocalReference i1 checkZero
              , trueDest  = isZero
              , falseDest = isn'tZero
              , metadata' = [] }

            (isZero #)
            abort Abort.DivisionByZero pos

            (isn'tZero #)
            addInstruction $ label := SDiv
              { exact = True
              , operand0 = lOperand
              , operand1 = rOperand
              , metadata = [] }

          Op.Mod    -> do
            checkZero <- newLabel "modCheckZero"
            addInstruction $ checkZero := ICmp
              { iPredicate = EQ
              , operand0   = ConstantOperand $ C.Int n 0
              , operand1   = rOperand
              , metadata   = [] }

            isZero <- newLabel "modIsZero"
            isn'tZero <- newLabel "modIsn'tZero"
            terminate' CondBr
              { condition = LocalReference i1 checkZero
              , trueDest  = isZero
              , falseDest = isn'tZero
              , metadata' = [] }

            (isZero #)
            abort Abort.DivisionByZero pos

            (isn'tZero #)
            addInstruction $ label := SRem
              { operand0 = lOperand
              , operand1 = rOperand
              , metadata = [] }

          Op.Min    -> addInstruction $
            label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (IntegerType n) minnumString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

          Op.Max    -> addInstruction $
            label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (IntegerType n) maxnumString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
          _         -> error "opInt"

        return $ LocalReference (IntegerType n) label

      opFloat op lOperand rOperand = do
        label <- newLabel "floatBinaryResult"
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
          Op.Power  -> Seq.singleton $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable floatType powString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }

          Op.Min    -> Seq.singleton $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable floatType minnumFstring
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }

          Op.Max    -> Seq.singleton $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable floatType maxnumFstring
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }

          _         -> error "opFloat"
        return $ LocalReference floatType label

      opBool op lOperand rOperand = do
        label <- newLabel "boolBinaryResult"
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
            labelCast1 <- newLabel "pointerEq0"
            labelCast2 <- newLabel "pointerEq1"
            let
            {- Both pointers must be cast to integer to be compared -}
              cast1 = PtrToInt
                        { operand0 = lOperand
                        , type'    = i8
                        , metadata = []
                        }

              cast2 = PtrToInt
                        { operand0 = rOperand
                        , type'    = i8
                        , metadata = []
                        }

              comp = ICmp { iPredicate = EQ
                          , operand0   = LocalReference intType labelCast1
                          , operand1   = LocalReference intType labelCast2
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
            labelCast1 <- newLabel "pointerNe0"
            labelCast2 <- newLabel "pointerNe1"
            let
              {- Both pointers must be cast to integer to be compared -}
              cast1 = PtrToInt
                        { operand0 = lOperand
                        , type'    = i8
                        , metadata = []
                        } 

              cast2 = PtrToInt
                        { operand0 = rOperand
                        , type'    = i8
                        , metadata = []
                        }

              comp = ICmp { iPredicate = NE
                          , operand0   = LocalReference i8 labelCast1
                          , operand1   = LocalReference i8 labelCast2
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
            label' <- newLabel "impliesResult"
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
            -- p <== q ≡ (p ⋁ q ≡ p)
            label' <- newLabel "conseqResult"
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

  EConditional { eguards, trueBranch } -> do
    entry  <- newLabel "ifExpEntry"
    finish <- newLabel "ifExpFinish"

    expType' <- toLLVMType expType

    terminate' Br
      { dest      = entry
      , metadata' = [] }

    (defaultLabel, phiPairs) <- foldM (guard finish) (entry, []) eguards

    (defaultLabel #)
    result <- newLabel "ifResult"
    extraPair <- case trueBranch of
      Nothing -> do
        let Location (from, _to) = loc
        abort Abort.If from
        pure []

      Just  e -> do
        val <- expression e
        Just defaultLabel' <- use blockName
        terminate' Br
          { dest      = finish
          , metadata' = [] }
        pure [(val, defaultLabel')]

    (finish #)
    addInstruction $ result := Phi
      { type'          = expType'
      , incomingValues = extraPair <> phiPairs
      , metadata       = [] }

    pure $ LocalReference expType' result

    where
      guard finish (checkLabel, pairs) (left, right) = do
        -- Each guard starts with a label for the left side (checkLabel).
        -- A conditional break is generated depending on the value of the
        -- condition. The trueDest is the finish label, where all guards
        -- lead to and the final value is assigned. The falseDest is the
        -- next guard, or, in the case of the last guard, the abort or default
        -- value. The foldM takes care of chaining the falseDests, as well
        -- as accumulating the (value, originLabel) pairs for the Phi node.

        (checkLabel #)

        yes <- newLabel "ifExpGuardYes"
        no  <- newLabel "ifExpGuardNo"

        condition <- expression left
        terminate' CondBr
          { condition
          , trueDest  = yes
          , falseDest = no
          , metadata' = [] }

        (yes #)
        val <- expression right
        Just yes' <- use blockName
        terminate' Br
          { dest      = finish
          , metadata' = [] }

        pure (no, (val, yes') : pairs)

  FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc } -> do
    arguments <- toList <$> mapM expression fArgs
    callType <- toLLVMType expType

    recArgs <- if fRecursiveCall
      then do
        boundOperand <- fromMaybe (error "internal error: boundless recursive function 2.") <$> use boundOp
        pure [ConstantOperand $ C.Int 1 1, boundOperand]
      else if fRecursiveFunc
        then pure [ConstantOperand $ C.Int 1 0, ConstantOperand $ C.Int 32 0]
        else pure []

    label <- newLabel "funcResult"
    addInstruction $ label := Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable callType $ unpack fName
      , arguments          = fmap (,[]) $ recArgs <> arguments
      , functionAttributes = []
      , metadata           = [] }

    pure $ LocalReference callType label

  Quantification { qOp } ->
    quantification expression e

  -- Dummy operand
  _ -> do
    traceM . drawTree . toTree $ e
    traceM "I don't know how to generate code for:"
    return . ConstantOperand $ C.Int 32 10




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
