{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module LLVM.Expression

where
--------------------------------------------------------------------------------
import           AST.Expression                          (CollectionKind (..),
                                                          Expression' (..),
                                                          Expression'' (..),
                                                          Value (..))
import qualified AST.Expression                          as Op (BinaryOperator (..),
                                                                UnaryOperator (..))
import qualified AST.Expression                          as E (loc)
import           AST.Object                              (Object' (..),
                                                          Object'' (..))
import           AST.Type
import qualified AST.Type                                as G (Type)
import           Error                                   (internal)
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (..))
import           LLVM.Boolean                            (boolean',
                                                          wrapBoolean')
import           LLVM.Monad
import           LLVM.Quantification                     (collection,
                                                          quantification)
import           LLVM.State
import           LLVM.Type                               (boolType, floatType,
                                                          intType, llvmName,
                                                          toLLVMType)
import           Location
import           SymbolTable
import           Treelike                                (drawTree, toTree)
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (foldM, when, zipWithM)
import           Data.Array                              ((!))
import           Data.Char                               (ord)
import           Data.Foldable                           (toList)
import           Data.Maybe                              (fromMaybe, isJust)
import           Data.Semigroup                          ((<>))
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
import           Prelude                                 hiding (Ordering (..))
--------------------------------------------------------------------------------
import           Debug.Trace

boolean :: Name -> Name -> Expression -> LLVM ()
boolean = boolean' expression' object objectRef

wrapBoolean :: Expression -> LLVM Operand
wrapBoolean = wrapBoolean' expression object objectRef

expression' :: Expression -> LLVM Operand
expression' e@Expression { expType } = if expType == GBool
  then wrapBoolean e
  else expression e
--------------------------------------------------------------------------------

object :: Object -> LLVM Operand
object obj@Object { objType, obj' } = case obj' of
  -- If the variable is marked as In, mean it was passed to the
  -- procedure as a constant so doesn't need to be loaded
  Variable { mode } | mode == Just In
    || (isJust mode && not (objType =:= basic)) -> objectRef obj False
    -- && objType =:= GOneOf [GBool,GChar,GInt,GFloat] -> objectRef obj

  -- If not marked as In, just load the content of the variable
  _ -> do
      label <- newLabel "varObj"
      -- Make a reference to the variable that will be loaded (e.g. %a)
      addrToLoad <- objectRef obj False
      t <- toLLVMType objType

      -- Load the value of the variable address on a label (e.g. %12 = load i32* %a, align 4)
      addInstruction $ label := Load { volatile  = False
                  , address   = addrToLoad
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = [] }

      -- The reference to where the variable value was loaded (e.g. %12)
      return $ LocalReference t label


-- Get the reference to the object.
-- indicate that the object is not a deref, array access or field access inside a procedure
objectRef :: Object -> Bool -> LLVM Operand
objectRef obj@(Object loc objType obj') flag = do
  objType' <- toLLVMType objType
  case obj' of

    Variable { name , mode } -> do
      name' <- getVariableName name
      if isJust mode && not flag
        then case objType of
          GPointer t -> do
            label <- newLabel "loadRef"
            addInstruction $ label := Load
              { volatile  = False
              , address   = LocalReference objType' name'
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }
            pure $ LocalReference objType' label
          _ -> pure $ LocalReference objType' name'
        else pure $ LocalReference objType' name'

    Index inner indices -> do
      ref   <- objectRef inner True

      iarrPtrPtr <- newLabel "idxStruct"
      addInstruction $ iarrPtrPtr := GetElementPtr
        { inBounds = False
        , address  = ref
        , indices  = ConstantOperand . C.Int 32 <$>
          [0, fromIntegral . length $ indices]
        , metadata = [] }

      iarrPtr <- newLabel "idxArrPtr"
      addInstruction $ iarrPtr := Load
        { volatile       = True
        , address        = LocalReference (ptr . ptr $ iterate (ArrayType 1) objType' !! length indices) iarrPtrPtr
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      inds <- zipWithM (idx ref) (toList indices) [0..]

      result <- newLabel "idxArr"
      addInstruction $ result := GetElementPtr
        { inBounds = False
        , address  = LocalReference (ptr $ iterate (ArrayType 1) objType' !! length indices) iarrPtr
        , indices  = ConstantOperand (C.Int 32 0) : inds
        , metadata = [] }

      pure . LocalReference objType' $ result

      where
        idx ref index n = do
          e <- expression index

          chkGEZ <- newLabel "idxChkGEZ"
          addInstruction $ chkGEZ := ICmp
            { iPredicate = SGE
            , operand0   = e
            , operand1   = ConstantOperand (C.Int 32 0)
            , metadata   = [] }

          gez <- newLabel "idxGEZ"
          notGez <- newLabel "idxNotGEZ"
          terminate CondBr
            { condition = LocalReference i1 chkGEZ
            , trueDest  = gez
            , falseDest = notGez
            , metadata' = [] }

          (notGez #)
          abort Abort.NegativeIndex (pos . E.loc $ index)

          (gez #)
          bndPtr <- newLabel "idxBoundPtr"
          addInstruction $ bndPtr := GetElementPtr
            { inBounds = False
            , address  = ref
            , indices  =
              [ ConstantOperand (C.Int 32 0)
              , ConstantOperand (C.Int 32 n)]
            , metadata = [] }

          bnd <- newLabel "idxBound"
          addInstruction $ bnd :=  Load
            { volatile       = False
            , address        = LocalReference i32 bndPtr
            , maybeAtomicity = Nothing
            , alignment      = 4
            , metadata       = [] }

          chkInBound <- newLabel "idxChkInBound"
          addInstruction $ chkInBound := ICmp
            { iPredicate = SLT
            , operand0   = e
            , operand1   = LocalReference i32 bnd
            , metadata   = [] }

          inBound <- newLabel "idxInBound"
          notInBound <- newLabel "idxNotInBound"
          terminate CondBr
            { condition = LocalReference i1 chkInBound
            , trueDest  = inBound
            , falseDest = notInBound
            , metadata' = [] }

          (notInBound #)
          abort Abort.OutOfBoundsIndex (pos . E.loc $ index)

          (inBound #)
          pure e


    Deref inner -> do
      ref        <- objectRef inner True
      labelLoad  <- newLabel "derefLoad"
      labelCast  <- newLabel "derefCast"
      labelNull  <- newLabel "derefNull"
      labelCond  <- newLabel "derefCond"
      trueLabel  <- newLabel "derefNullTrue"
      falseLabel <- newLabel "derefNullFalse"
      let
        Location (pos,_) = loc

      addInstruction $ labelLoad := Load
        { volatile  = False
        , address   = ref
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      {- Generate assembly to verify if a pointer is NULL, when accessing to the pointed memory.
         In that case, abort the program giving the source line of the bad access instead of letting
         the OS ends the process
      -}
      addInstruction $ labelCast := PtrToInt
        { operand0 = LocalReference objType' labelLoad
        , type'    = i64
        , metadata = [] }

      addInstruction $ labelNull := PtrToInt
        { operand0 = ConstantOperand . C.Null $ ptr objType'
        , type'    = i64
        , metadata = [] }

      addInstruction $ labelCond := ICmp
        { iPredicate = EQ
        , operand0   = LocalReference i64 labelCast
        , operand1   = LocalReference i64 labelNull
        , metadata   = [] }

      terminate CondBr
        { condition = LocalReference boolType labelCond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }


      (trueLabel #)
      abort Abort.NullPointerAccess pos

      (falseLabel #)

      return . LocalReference objType' $ labelLoad


    Member { inner, field } -> do
      ref <- objectRef inner True
      label <- newLabel $ "member" <> show field
      addInstruction $ label := GetElementPtr
          { inBounds = False
          , address  = ref
          , indices  = ConstantOperand . C.Int 32 <$> [0, field]
          , metadata = []}
      pure . LocalReference objType' $ label

  -- where
  --   getIndices :: ([Operand], Maybe Operand) -> Object -> LLVM ([Operand], Maybe Operand)
  --   getIndices (indices,ref) (Object _ _ (Index inner index)) = do
  --     index' <- expression index
  --     getIndices (index':indices,ref) inner
  --
  --   getIndices (indices,ref) obj = do
  --     ref <- objectRef obj False
  --     return (reverse indices, Just ref)



-- LLVM offers a set of secure operations that know when an int operation reach an overflow
-- llvm.sadd.with.overflow.i32 (fun == intAdd)
-- llvm.ssub.with.overflow.i32 (fun == intSub)
-- llvm.smul.with.overflow.i32 (fun == intMul)
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

  addInstruction $ label     := ExtractValue
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

expression e@Expression { E.loc = (Location(pos,_)), expType, exp'} = case exp' of
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

  StringLit { theStringId } ->
    (! theStringId) <$> use stringOps

  Obj obj -> object obj

  Unary unOp inner -> do
    innerOperand <- expression inner

    operand <- case expType of
      GInt   -> opInt 32  unOp innerOperand
      GChar  -> opInt 8  unOp innerOperand
      -- GBool  -> opBool  unOp innerOperand
      GFloat -> opFloat unOp innerOperand
      t      -> error $ "tipo " <> show t <> " no soportado"

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

        return $ LocalReference (IntegerType n) label

      opFloat :: Op.UnaryOperator -> Operand -> LLVM Operand
      opFloat op innerOperand = do
        label <- newLabel "opFloat"
        let
          insts = case op of
            Op.UMinus -> Seq.singleton $ label := FMul
                    { fastMathFlags = NoFastMathFlags
                    , operand0 = innerOperand
                    , operand1 = ConstantOperand . C.Float $ LLVM.Double (-1.0)
                    , metadata = []}

        addInstructions insts
        return $ LocalReference floatType label

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
         , lexpr = lexpr@Expression { expType = lType }
         , rexpr = rexpr@Expression { expType = rType } } -> do
    -- Evaluate both expressions
    lOperand <- expression lexpr
    rOperand <- expression rexpr

    -- Get the type of the left expr. Used at bool operator to know the type when comparing.
    let
      op = case expType of
        GInt        -> opInt 32
        GChar       -> opInt 8
        -- GBool  -> opBool
        GFloat      -> opFloat
        GSet _      -> opSet
        GMultiset _ -> opMultiset
        GSeq _      -> opSeq
        t      -> error $
          "internal error: type " <> show t <> " not supported"

    op binOp lOperand rOperand

    where
      opInt n op lOperand rOperand = do
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

            (isntZero #)
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

        return $ LocalReference (IntegerType n) label

      opFloat op lOperand rOperand = do
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

          _ -> error "opFloat"
        return $ LocalReference floatType label

      opSet op lOperand rOperand = do
        label <- newLabel "setBinaryResult"
        case op of
          Op.Union -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) unionSetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
          Op.Intersection -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) intersectSetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
          Op.Difference -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) differenceSetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
        return $ LocalReference floatType label

      opMultiset op lOperand rOperand = do
        label <- newLabel "multisetBinaryResult"
        case op of
          Op.Union -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) unionMultisetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
          Op.Intersection -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) intersectMultisetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
          Op.Difference -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) differenceMultisetString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }
        return $ LocalReference floatType label

      opSeq op lOperand rOperand = do
        label <- newLabel "multisetBinaryResult"
        case op of
          Op.Concat -> addInstruction $ label := Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable (ptr i8) concatSequenceString
            , arguments          = [(lOperand,[]), (rOperand,[])]
            , functionAttributes = []
            , metadata           = [] }

        return $ LocalReference floatType label

  EConditional { eguards, trueBranch } -> do
    entry  <- newLabel "ifExpEntry"
    finish <- newLabel "ifExpFinish"

    expType' <- toLLVMType expType

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

        boolean yes no left

        (yes #)
        val <- expression right
        Just yes' <- use blockName
        terminate Br
          { dest      = finish
          , metadata' = [] }

        pure (no, (val, yes') : pairs)

  FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc, fStructArgs } -> do
    arguments <- toList <$> mapM createArg fArgs
    callType <- toLLVMType expType

    fName' <- case fStructArgs of
      Just (structBaseName, typeArgs) -> do
        llvmName (fName <> "-" <> structBaseName) <$>
          mapM toLLVMType (toList typeArgs)
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
        subst <- use substitutionTable
        let
          type' = case subst of
            t:_ -> fillType t expType
            []  -> expType

        (,[]) <$> if type' =:= basicT
          then
            expression' expr
          else do
            label <- newLabel "argCast"
            ref   <- objectRef (theObj exp') False

            type' <- ptr <$> toLLVMType type'
            addInstruction $ label := BitCast
              { operand0 = ref
              , type'    = type'
              , metadata = [] }
            pure $ LocalReference type' label
      basicT = GOneOf [GBool,GChar,GInt,GFloat, GString]

  Quantification { } ->
    quantification expression boolean safeOperation e

  Collection { } ->
    collection expression boolean e

  -- Dummy operand
  _ -> do
    traceM . drawTree . toTree $ e
    traceM "I don't know how to generate code for:"
    return . ConstantOperand $ C.Int 32 10
