{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module LLVM.Expression

where
--------------------------------------------------------------------------------
import           AST.Expression                          (CollectionKind (..),
                                                          Expression (..),
                                                          Expression' (..),
                                                          Value (..))
import           AST.Definition
import           AST.Struct
import qualified AST.Expression                          as Op (BinaryOperator (..),
                                                                UnaryOperator (..))
import qualified AST.Expression                          as E (Expression (expType),
                                                               loc)
import           AST.Object                              (Object (..),
                                                          Object' (..))
import           AST.Type
import qualified AST.Type                                as G (Type)
import           Common
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (..))
import           LLVM.Boolean                            (boolean, wrapBoolean)
import           LLVM.Monad
import           LLVM.Object                             (object, objectRef)
import           LLVM.Quantification                     (collection,
                                                          quantification)
import           LLVM.State
import           LLVM.Type                               (boolType, floatType,
                                                          intType, llvmName,
                                                          pointerType, fill,
                                                          toLLVMType, tupleType,
                                                          sizeOf)
import           Location
import           Parser.Config
import           SymbolTable
import           Treelike                                (drawTree, toTree)
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Data.Array                              ((!))
import           Data.Char                               (ord)
import           Data.Map                                as Map (lookup)
import           Data.Maybe                              (fromMaybe, isJust)
import           Data.Sequence                           (ViewR ((:>)), (|>))
import qualified Data.Sequence                           as Seq (ViewR (EmptyR),
                                                                 empty,
                                                                 fromList,
                                                                 singleton,
                                                                 viewr)
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
import           Prelude                                 hiding (Ordering (..))
--------------------------------------------------------------------------------

expression' :: Expression -> LLVM Operand
expression' e@Expression { expType } = do

  t <- fill expType

  if t =:= GBool
    then wrapBoolean e { expType = t }
    else expression e { expType = t }
--------------------------------------------------------------------------------

-- LLVM offers a set of secure operations that know when an int operation reach an overflow
-- llvm.sadd.with.overflow.i32 (fun == intAdd)
-- llvm.ssub.with.overflow.i32 (fun == intSub)
-- llvm.smul.with.overflow.i32 (fun == intMul)
safeOperation :: Word32
              -> Name
              -> (Word32 -> String)
              -> Operand
              -> Operand
              -> SourcePos
              -> LLVM ()
safeOperation n label fun lOperand rOperand pos = do
  labelOp       <- newLabel "safeOp"
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

  addInstruction $ label := ExtractValue
    { aggregate = LocalReference safeStruct labelOp
    , indices'  = [0]
    , metadata  = [] }

  addInstruction $ labelCond := ExtractValue
    { aggregate = LocalReference safeStruct labelOp
    , indices'  = [1]
    , metadata  = [] }

  terminate CondBr
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

expression :: Expression -> LLVM Operand
expression e@Expression { expType = GBool} =
  internal $
    "generated boolean expression with `expression` instead of `boolean`\n" <>
    drawTree (toTree e)

expression e@Expression { E.loc = (Location(pos,_)), expType, exp'} = do 
  expType <- fill expType
  case exp' of
    Value val -> pure $ case val of
      -- BoolV  theBool  ->
      --   ConstantOperand $ C.Int 1 (if theBool then 1 else 0)
      CharV  theChar  ->
        ConstantOperand . C.Int 8 . fromIntegral . ord $ theChar
      IntV   theInt   ->
        ConstantOperand . C.Int 32 . fromIntegral $ theInt
      FloatV theFloat ->
        ConstantOperand . C.Float $ LLVM.Double theFloat

    NullPtr ->
      case expType of
        GPointer GAny -> pure . ConstantOperand . C.Null $ ptr i8
        _             -> ConstantOperand . C.Null  <$> toLLVMType expType

    SizeOf { sType } -> ConstantOperand . C.Int 32 <$> sizeOf sType

    t@Tuple { left, right } -> do
      l <- expression left
      r <- expression right

      lType <- fill $ E.expType left
      rType <- fill $ E.expType right

      lBitcast <- newLabel "lBitcast"
      rBitcast <- newLabel "rBitcast"
      tuple    <- newLabel "tuple"
      lPtr     <- newLabel "lPtr"
      rPtr     <- newLabel "rPtr"
      tupleT   <- toLLVMType $ expType

      addInstruction . (lBitcast :=) $ case lType of
        GFloat -> BitCast
          { operand0 = l
          , type' = i64
          , metadata = [] }

        GPointer _ -> PtrToInt
          { operand0 = l
          , type'    = i64
          , metadata = [] }

        _ -> ZExt
          { operand0 = l
          , type' = i64
          , metadata = [] }

      addInstruction . (rBitcast :=) $ case rType of
        GFloat -> BitCast
          { operand0 = r
          , type' = i64
          , metadata = [] }

        GPointer _ -> PtrToInt
          { operand0 = r
          , type'    = i64
          , metadata = [] }

        _ -> ZExt
          { operand0 = r
          , type' = i64
          , metadata = [] }

      addInstruction $ tuple := Alloca
        { allocatedType = tupleT
        , numElements   = Nothing
        , alignment     = 4
        , metadata      = [] }

      addInstruction $ lPtr := GetElementPtr
        { inBounds = False
        , address  = LocalReference tupleT tuple
        , indices  = ConstantOperand . C.Int 32 <$> [0, 0]
        , metadata = [] }

      addInstruction $ rPtr := GetElementPtr
        { inBounds = False
        , address  = LocalReference tupleT tuple
        , indices  = ConstantOperand . C.Int 32 <$> [0, 1]
        , metadata = [] }

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference i64 lPtr
        , value    = LocalReference i64 lBitcast
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference i64 rPtr
        , value    = LocalReference i64 rBitcast
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      pure $ LocalReference tupleT tuple

    StringLit { theStringId } ->
      (! theStringId) <$> use stringOps

    Obj obj -> object obj

    Unary unOp inner -> do
      innerOperand <- expression inner

      operand <- case expType of
        GInt   -> opInt 32  unOp innerOperand
        GChar  -> opInt 8  unOp innerOperand
        GFloat -> opFloat unOp innerOperand
        -- GSet _      -> opSet unOp innerOperand
        -- GMultiset _ -> opMultiset unOp innerOperand
        -- GSeq _      -> opSeq unOp innerOperand
        t      -> internal $ "type " <> show t <> " not supported (unary expression)"

      pure operand

      where
        opInt :: Word32 -> Op.UnaryOperator -> Operand -> LLVM Operand
        opInt n op innerOperand = do
          label <- newLabel $ case n of
            32 -> "unaryIntOp"
            8  -> "unaryCharOp"
            _  -> internal "badUnaryIntOp"
          let
            minusOne = ConstantOperand $ C.Int 32 (-1)
            one = ConstantOperand $ C.Int n 1
          case op of
              Op.UMinus ->
                safeOperation n label safeMul innerOperand minusOne pos

              Op.Succ   ->
                safeOperation n label safeAdd innerOperand one pos

              Op.Pred   ->
                safeOperation n label safeSub innerOperand one pos

              Op.Card -> addInstruction $ label := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable (ptr i8) $ case E.expType inner of
                  GSet _      -> sizeSetString
                  GMultiset _ -> sizeMultisetString
                  GSeq _      -> sizeSeqString
                  GRel _ _    -> sizeRelString
                  GFunc _ _   -> sizeFuncString
                , arguments          = [(innerOperand,[])]
                , functionAttributes = []
                , metadata           = [] }

          pure $ LocalReference (IntegerType n) label

        opFloat :: Op.UnaryOperator -> Operand -> LLVM Operand
        opFloat op innerOperand = do
          label <- newLabel "opFloat"
          addInstruction $ label := FMul
            { fastMathFlags = NoFastMathFlags
            , operand0 = innerOperand
            , operand1 = ConstantOperand . C.Float $ LLVM.Double (-1.0)
            , metadata = []}

          pure $ LocalReference floatType label

        -- opSet :: Op.UnaryOperator -> Operand -> LLVM Operand
        -- opSet op innerOperand = do
        --   label <- newLabel "opSet"
        --   addInstruction $ label := Call
        --     { tailCallKind       = Nothing
        --     , callingConvention  = CC.C
        --     , returnAttributes   = []
        --     , function           = callable (ptr i8) sizeSetString
        --     , arguments          = [(innerOperand,[])]
        --     , functionAttributes = []
        --     , metadata           = [] }

        --   pure $ LocalReference floatType label

        -- opMultiset :: Op.UnaryOperator -> Operand -> LLVM Operand
        -- opMultiset op innerOperand = do
        --   label <- newLabel "opMultiset"
        --   addInstruction $ label := Call
        --     { tailCallKind       = Nothing
        --     , callingConvention  = CC.C
        --     , returnAttributes   = []
        --     , function           = callable (ptr i8) sizeMultisetString
        --     , arguments          = [(innerOperand,[])]
        --     , functionAttributes = []
        --     , metadata           = [] }

        --   pure $ LocalReference floatType label

        -- opSeq :: Op.UnaryOperator -> Operand -> LLVM Operand
        -- opSeq op innerOperand = do
        --   label <- newLabel "opSeq"
        --   addInstruction $ label := Call
        --     { tailCallKind       = Nothing
        --     , callingConvention  = CC.C
        --     , returnAttributes   = []
        --     , function           = callable (ptr i8) sizeSeqString
        --     , arguments          = [(innerOperand,[])]
        --     , functionAttributes = []
        --     , metadata           = [] }

        --   pure $ LocalReference floatType label


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

    Binary { binOp
           , lexpr = lexpr@Expression { expType = lType' }
           , rexpr = rexpr@Expression { expType = rType' } } -> do
      -- Evaluate both expressions
      lOperand <- expression lexpr
      rOperand <- expression rexpr

      lType <- fill lType'
      rType <- fill rType'

      case binOp of
        Op.SeqAt    -> seqAt lOperand rOperand lType rType
        Op.BifuncAt -> bifuncAt lOperand rOperand lType rType
        _ -> do
          let
            op = case expType {- The type of the whole expression -} of
              GInt        -> opInt 32
              GChar       -> opInt 8
              GFloat      -> opFloat
              GSet _      -> opSet
              GMultiset _ -> opMultiset
              GSeq _      -> opSeq
              GFunc _ _   -> opFunc
              GRel _ _    -> opRel
              t      -> internal $ "type " <> show t <> " not supported\n" <>
                show binOp <> "\n" <>
                drawTree (toTree e)

          op binOp lOperand rOperand lType rType

      where
        opInt n op lOperand rOperand lType rType = do
          label <- newLabel $ case n of
            32 -> "intOp"
            8  -> "charOp"
            _  -> internal "badIntOp"
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
              isntZero <- newLabel "divIsntZero"
              terminate CondBr
                { condition = LocalReference i1 checkZero
                , trueDest  = isZero
                , falseDest = isntZero
                , metadata' = [] }

              (isZero #)
              abort Abort.DivisionByZero pos

              (isntZero #)
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
              isntZero <- newLabel "modIsntZero"
              terminate CondBr
                { condition = LocalReference i1 checkZero
                , trueDest  = isZero
                , falseDest = isntZero
                , metadata' = [] }

              (isZero #)
              abort Abort.DivisionByZero pos

              aux <- newLabel "modAux" 
              aux2 <- newLabel "modAux2" 
              auxSgn <- newLabel "modAuxSgn"
              isNeg <- newLabel "modAuxIsPoz"
              isntNeg <- newLabel "modAuxIsntPoz"
              wrapIt <- newLabel "modEnd"

              (isntZero #)
              addInstruction $ aux := SRem
                { operand0 = lOperand
                , operand1 = rOperand
                , metadata = [] }

              addInstruction $ auxSgn := ICmp
                { iPredicate = SLT
                , operand0   = LocalReference (case n of 8 -> i8; 32 -> i32) aux
                , operand1   = ConstantOperand $ C.Int n 0
                , metadata   = [] }

              terminate CondBr
                { condition = LocalReference i1 auxSgn
                , trueDest  = isNeg
                , falseDest = isntNeg
                , metadata' = [] }

              (isntNeg #)
              terminate Br
                { dest      = wrapIt 
                , metadata' = [] }

              (isNeg #)
              addInstruction $ aux2 := Add
                { nsw = False
                , nuw = False
                , operand0 = LocalReference (case n of 8 -> i8; 32 -> i32) aux
                , operand1 = rOperand
                , metadata = [] }

              terminate Br
                { dest      = wrapIt 
                , metadata' = [] }

              (wrapIt #)
              addInstruction $ label := Phi
                { type'          = case n of 8 -> i8; 32 -> i32
                , incomingValues = 
                  [ (LocalReference (case n of 8 -> i8; 32 -> i32) aux2, isNeg)
                  , (LocalReference (case n of 8 -> i8; 32 -> i32) aux, isntNeg)
                  ]
                , metadata       = [] }

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

            Op.Power -> do
              let posConstant =
                    (,[]) . ConstantOperand . C.Int 32 . fromIntegral . unPos
              addInstruction $ label := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable (IntegerType n) powIString
                , arguments          =
                  [ (lOperand,[])
                  , (rOperand,[])
                  , posConstant . sourceLine $ pos
                  , posConstant . sourceColumn $ pos ]
                , functionAttributes = []
                , metadata           = [] }

          pure $ LocalReference (IntegerType n) label

        opFloat op lOperand rOperand lType rType = do
          label <- newLabel "floatBinaryResult"
          case op of
            Op.Plus   -> addInstruction $ label := FAdd
              { fastMathFlags = NoFastMathFlags
              , operand0      = lOperand
              , operand1      = rOperand
              , metadata      = [] }

            Op.BMinus -> addInstruction $ label := FSub
              { fastMathFlags = NoFastMathFlags
              , operand0      = lOperand
              , operand1      = rOperand
              , metadata      = [] }

            Op.Times  -> addInstruction $ label := FMul
              { fastMathFlags = NoFastMathFlags
              , operand0      = lOperand
              , operand1      = rOperand
              , metadata      = [] }

            Op.Div   ->  addInstruction $ label := FDiv
              { fastMathFlags = NoFastMathFlags
              , operand0      = lOperand
              , operand1      = rOperand
              , metadata      = [] }

            Op.Power  -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable floatType powString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

            Op.Min    -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable floatType minnumFstring
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

            Op.Max    -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable floatType maxnumFstring
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

            _ -> internal $ "opFloat unsupported op " <> show op
          pure $ LocalReference floatType label

        opSet op lOperand rOperand lType rType = do
          label <- newLabel "setBinaryResult"
          case op of
            Op.Union -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GSet (GTuple _ _) -> unionSetPairString
                otherwise         -> unionSetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Intersection -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GSet (GTuple _ _) -> intersectSetPairString
                otherwise         -> intersectSetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Difference -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GSet (GTuple _ _) -> differenceSetPairString
                otherwise         -> differenceSetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            _ -> internal $ "unsupported set op " <> show op
          pure $ LocalReference pointerType label

        opMultiset op lOperand rOperand lType rType = do
          label <- newLabel "multisetBinaryResult"
          case op of
            Op.Union -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GMultiset (GTuple _ _) -> unionMultisetPairString
                otherwise              -> unionMultisetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Intersection -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GMultiset (GTuple _ _) -> intersectMultisetPairString
                otherwise              -> intersectMultisetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Difference -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GMultiset (GTuple _ _) -> differenceMultisetPairString
                otherwise              -> differenceMultisetString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.MultisetSum -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GMultiset (GTuple _ _) -> multisetPairSumString
                otherwise              -> multisetSumString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

          pure $ LocalReference pointerType label

        opSeq op lOperand rOperand lType rType = do
          label <- newLabel "sequenceBinaryResult"
          case op of
            Op.Concat -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) $ case lType of
                GSeq (GTuple _ _) -> concatSequencePairString
                otherwise         -> concatSequenceString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }

          pure $ LocalReference pointerType label

        opFunc op lOperand rOperand lType rType = do
          label <- newLabel "funcBinaryResult"
          case op of
            Op.Union -> do
              let
                SourcePos _ x y = pos
                line = ConstantOperand . C.Int 32 . fromIntegral $ unPos x
                col  = ConstantOperand . C.Int 32 . fromIntegral $ unPos y

              addInstruction $ label := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable (ptr i8) unionFunctionString
                , arguments          = [(lOperand,[]), (rOperand,[]), (line, []), (col, [])]
                , functionAttributes = []
                , metadata           = [] }
            Op.Intersection -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) intersectFunctionString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Difference -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) differenceFunctionString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
          pure $ LocalReference pointerType label

        opRel op lOperand rOperand lType rType = do
          label <- newLabel "funcBinaryResult"
          case op of
            Op.Union -> addInstruction $ label := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable (ptr i8) unionSetPairString
                , arguments          = [(lOperand,[]), (rOperand,[])]
                , functionAttributes = []
                , metadata           = [] }
            Op.Intersection -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) intersectSetPairString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
            Op.Difference -> addInstruction $ label := Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable (ptr i8) differenceSetPairString
              , arguments          = [(lOperand,[]), (rOperand,[])]
              , functionAttributes = []
              , metadata           = [] }
          pure $ LocalReference pointerType label

        seqAt lOp rOp lType rType = do
          let
            SourcePos _ x y = pos
            line = ConstantOperand . C.Int 32 . fromIntegral $ unPos x
            col  = ConstantOperand . C.Int 32 . fromIntegral $ unPos y

          let atString = case expType of
                GTuple {} -> atSequencePairString
                _         -> atSequenceString

          call <- newLabel "seqAt"
          addInstruction $ call := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) atString
            , arguments          = (,[]) <$> [lOp, rOp, line, col]
            , functionAttributes = []
            , metadata           = [] }

          seqAtResult <- newLabel "seqAtResult"

          llvmType <- toLLVMType expType
          case expType of
            GTuple {} -> do
              addInstruction $ seqAtResult := Alloca
                { allocatedType = tupleType
                , numElements   = Nothing
                , alignment     = 4
                , metadata      = [] }

              addInstruction $ Do Store
                { volatile = False
                , address  = LocalReference tupleType seqAtResult
                , value    = LocalReference tupleType call
                , maybeAtomicity = Nothing
                , alignment = 4
                , metadata  = [] }

            GFloat -> addInstruction $ seqAtResult := BitCast
              { operand0 = LocalReference i64 call
              , type' = floatType
              , metadata = [] }

            GPointer _ -> addInstruction $ seqAtResult := IntToPtr
              { operand0 = LocalReference i64 call
              , type'    = llvmType
              , metadata = [] }

            _ -> addInstruction $ seqAtResult := Trunc
              { operand0 = LocalReference i64 call
              , type'    = IntegerType $ case expType of
                GInt  -> 32
                GChar -> 8
                GBool -> 1
                GAny  -> 32
                _     -> internal $ "seqAtCast " <> show expType
              , metadata = [] }

          t <- toLLVMType expType
          pure $ LocalReference t seqAtResult


        bifuncAt lOp rOp lType rType = do
          rCast <- newLabel "rightCast"
          addInstruction $ rCast := case rType of
            GFloat -> BitCast
              { operand0 = rOp
              , type' = i64
              , metadata = [] }

            GPointer _ -> PtrToInt
              { operand0 = rOp
              , type'    = i64
              , metadata = [] }

            _ -> ZExt
              { operand0 = rOp
              , type' = i64
              , metadata = [] }

          bifuncAtResult <- newLabel "bifuncAt"
          case lType of
            GFunc {} -> do
              let
                SourcePos _ x y = pos
                line = ConstantOperand . C.Int 32 . fromIntegral $ unPos x
                col  = ConstantOperand . C.Int 32 . fromIntegral $ unPos y

              call <- newUnLabel
              addInstruction $ call := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable i64 evalFuncString
                , arguments          = (,[]) <$> [lOp, LocalReference i64 rCast, line, col]
                , functionAttributes = []
                , metadata           = [] }

              llvmType <- toLLVMType expType
              addInstruction $ bifuncAtResult := case expType of
                GFloat -> BitCast
                  { operand0 = LocalReference i64 call
                  , type' = llvmType
                  , metadata = [] }
                GPointer _ -> IntToPtr
                  { operand0 = LocalReference i64 call
                  , type'    = llvmType
                  , metadata = [] }

                _ -> Trunc
                  { operand0 = LocalReference i64 call
                  , type'    = IntegerType $ case expType of
                    GInt  -> 32
                    GChar -> 8
                    GAny  -> 32
                    _     -> internal $ "bifuncAtCast " <> show expType
                  , metadata = [] }

            GRel {} -> addInstruction $ bifuncAtResult := Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable (ptr i8) evalRelString
                , arguments          = (,[]) <$> [lOp, LocalReference i64 rCast]
                , functionAttributes = []
                , metadata           = [] }
            _ -> internal $ "impossible bifuncAt " <> show lType

          t <- toLLVMType expType
          pure $ LocalReference t bifuncAtResult


    EConditional { eguards, trueBranch } -> do
      entry  <- newLabel "ifExpEntry"
      finish <- newLabel "ifExpFinish"

      llType <- toLLVMType expType

      terminate Br
        { dest      = entry
        , metadata' = [] }

      (defaultLabel, phiPairs) <- foldM (guard finish) (entry, []) eguards

      (defaultLabel #)
      result <- newLabel "ifResult"
      extraPair <- case trueBranch of
        Nothing -> do
          abort Abort.If pos
          pure []

        Just  e -> do
          val <- expression e
          Just defaultLabel' <- use blockName
          terminate Br
            { dest      = finish
            , metadata' = [] }
          pure [(val, defaultLabel')]

      (finish #)
      addInstruction $ result := Phi
        { type'          = llType
        , incomingValues = extraPair <> phiPairs
        , metadata       = [] }

      pure $ LocalReference llType result

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

          boolean yes no left

          (yes #)
          val <- expression right
          Just yes' <- use blockName
          terminate Br
            { dest      = finish
            , metadata' = [] }

          pure (no, (val, yes') : pairs)

    AbstFunctionCall {fName, fArgs, fStructArgs} -> do
      fdt <- use fullDataTypes
      let 
        (dtName, typeArgs) = case fStructArgs of 
          Nothing -> internal $ "Calling an abstract function of unknown Abstract Data Type"
          Just x -> x
      case dtName `Map.lookup` fdt of
        Nothing -> internal $ "Could not find Data Type " <> show dtName
        Just (Struct{structProcs},_) -> case fName `Map.lookup` structProcs of
          Nothing -> internal $ "Could not find function " <> 
                                show fName <> " in Data Type " <> 
                                show dtName

          Just Definition{ def' = FunctionDef{ funcRecursive }} -> 
            expression e{exp'=FunctionCall fName fArgs False funcRecursive fStructArgs}

    FunctionCall { fName, fArgs }
      | fName `elem` fmap pack [traceTypeVarString, traceStringTypeVarString] -> do
        let strx = fName == pack traceStringTypeVarString
        case Seq.viewr fArgs of
          Seq.EmptyR -> internal "impossible trace"
          _ :> e -> do

            type' <- fill expType

            expression' $ case type' of
              GBool  -> e { exp' = exp'
                { fName = pack $ if strx
                  then traceStringBoolString
                  else traceBoolString }}
              GChar  -> e { exp' = exp'
                { fName = pack $ if strx
                  then traceStringCharString
                  else traceCharString }}
              GInt   -> e { exp' = exp'
                { fName = pack $ if strx
                  then traceStringIntString
                  else traceIntString }}
              GFloat -> e { exp' = exp'
                { fName = pack $ if strx
                  then traceStringFloatString
                  else traceFloatString }}
              _ -> internal "impossible trace 2"

      

    FunctionCall { fName = "_pointer2int", fArgs } -> do
      argument  <- expression . head . toList $ fArgs
      labelCast <- newLabel "ptr2int"
      
      addInstruction $ labelCast := PtrToInt
        { operand0 = argument
        , type'    = intType
        , metadata = [] }

      pure $ LocalReference intType labelCast

    FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc, fStructArgs } -> do
      arguments <- toList <$> mapM createArg fArgs
      callType <- toLLVMType expType

      fName' <- case fStructArgs of
        Just (structBaseName, typeArgs) -> do

          t' <- mapM fill (toList typeArgs)
          pure $ llvmName (fName <> "-" <> structBaseName) t'

        _ -> pure . unpack $ fName

      recArgs <- fmap (,[]) <$> if fRecursiveCall
        then do
          boundOperand <- fromMaybe (internal "boundless recursive function 2.") <$> use boundOp
          pure [ConstantOperand $ C.Int 1 1, boundOperand]
        else if fRecursiveFunc
          then pure [ConstantOperand $ C.Int 1 0, ConstantOperand $ C.Int 32 0]
          else pure []
      label <- newLabel "funcResult"
      addInstruction $ label := Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable callType fName'
        , arguments          = recArgs <> arguments
        , functionAttributes = []
        , metadata           = [] }

      pure $ LocalReference callType label

      where
        createArg expr@Expression { expType, exp' } = do

          type' <- fill expType

          (,[]) <$> if type' =:= basicT || type' == I64 || type' =:= highLevel
            then
              expression' expr
            else if type' =:= GPointer GAny 
              then do 
                expr <- expression expr
                let GPointer t = type'
                type' <- ptr <$> toLLVMType t

                label <- newLabel "argAlloc"
                addInstruction $ label := Alloca
                  { allocatedType = type'
                  , numElements   = Nothing
                  , alignment     = 4
                  , metadata      = [] }

                addInstruction $ Do Store
                  { volatile = False
                  , address  = LocalReference type' label
                  , value    = expr
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = [] }

                pure $ LocalReference type' label
            else case exp' of 
              Obj o         -> do 
                label <- newLabel "argCastOb"
                ref <- objectRef o
                type' <- ptr <$> toLLVMType type'
                addInstruction $ label := BitCast
                  { operand0 = ref
                  , type'    = type'
                  , metadata = [] }
                pure $ LocalReference type' label

        basicT = GOneOf [GBool,GChar,GInt,GFloat, GString]

    Quantification { } ->
      quantification e

    Collection { } ->
      collection e

    AddressOf (inner@Expression{ expType = iType, exp'}) -> case exp' of 
      Obj{ theObj } -> objectRef theObj
      _ -> internal $ "Cannot get the address of a non object expression\n" <> (drawTree . toTree $ e) 


    I64Cast { inner = inner @ Expression {expType = iType} } -> do
      i <- expression' inner

      type' <- fill iType

      t <- newUnLabel

      case type' of
        GTuple a b -> pure i
        _ -> do
          addInstruction $ t := case type' of
            GFloat -> BitCast
              { operand0 = i
              , type' = i64
              , metadata = [] }

            GPointer _ -> PtrToInt
              { operand0 = i
              , type'    = i64
              , metadata = [] }

            _ -> ZExt
              { operand0 = i
              , type' = i64
              , metadata = [] }
          pure $ LocalReference i64 t

    -- Dummy operand
    _ -> internal $
      "I don't know how to generate code for: " <> (drawTree . toTree $ e)

