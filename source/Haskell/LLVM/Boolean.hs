{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}

module LLVM.Boolean
  ( boolean'
  , wrapBoolean'
  ) where
--------------------------------------------------------------------------------
import           AST.Expression                          as Op
import           AST.Type
import           Error                                   hiding (fArgs, fName)
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (..))
import           LLVM.Monad
import           LLVM.Quantification
import           LLVM.State
import           LLVM.Type
import           Location
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (&))
import           Control.Monad                           (forM)
import           Data.Foldable                           (toList)
import           Data.Maybe                              (fromMaybe)
import           Data.Semigroup                          ((<>))
import           Data.Text                               (unpack)
import qualified LLVM.General.AST.CallingConvention      as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant               as C (Constant (..))
import           LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate (OEQ, OGE, OGT, OLE, OLT))
import           LLVM.General.AST.Instruction            (Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate (EQ, SGE, SGT, SLE, SLT))
import           LLVM.General.AST.Name                   (Name)
import           LLVM.General.AST.Operand                (Operand (..))
import           LLVM.General.AST.Type                   (i1, i64, ptr)
import           Prelude                                 hiding (Ordering (..))
--------------------------------------------------------------------------------
import           Debug.Trace

boolean' :: (Expression -> LLVM Operand) -- ^ non-boolean expression code generator
         -> (Object -> LLVM Operand) -- ^ object code generator (both boolean and non-boolean)
         -> (Object -> Bool -> LLVM Operand) -- ^ object ref code generator (both boolean and non-boolean)
         -> Name -- ^ true destination
         -> Name -- ^ false destination
         -> Expression -- ^ boolean expression
         -> LLVM ()
boolean' _ _ _ _ _ Expression { expType }
  | expType /= GBool = internal
    "attempted to generate non-boolean expression with `boolean` instead of `expression`"

boolean' expr object obRef true false e@Expression { loc, exp' } = let boolean = boolean' expr object obRef in case exp' of
  NullPtr -> internal "Null ptr cannot be boolean"

  Value { theValue = BoolV b } ->
    terminate Br
      { dest = if b then true else false
      , metadata' = [] }

  Value {} -> internal "Char/Int/Float V cannot be boolean"
  StringLit {} -> internal "string lit cannot be boolean"
  Collection {} -> internal "collection cannot be boolean"
  Tuple {} -> internal "tuple cannot be boolean"

  Obj { theObj } -> do
    op <- object theObj
    terminate CondBr
      { condition = op
      , trueDest  = true
      , falseDest = false
      , metadata' = [] }

  Binary { binOp, lexpr, rexpr } -> case binOp of
    Op.And -> do
      middle <- newLabel "and"
      boolean middle false lexpr
      (middle #)
      boolean true false rexpr

    Op.Or -> do
      middle <- newLabel "or"
      boolean true middle lexpr
      (middle #)
      boolean true false rexpr

    Implies -> do
      middle <- newLabel "implies"
      boolean middle true lexpr
      (middle #)
      boolean true false rexpr

    Consequent -> do
      middle <- newLabel "consequent"
      boolean true middle lexpr
      (middle #)
      boolean false true rexpr

    _ | binOp `elem` [Elem, NotElem] -> do
        lOperand <- expr lexpr
        rOperand <- expr rexpr

        item <- newLabel "item"
        substs <- use substitutionTable
        lType' <- case substs of 
          subst : _ -> pure $ fillType subst (expType lexpr)
          _         -> pure $ expType lexpr
        addInstruction $ item := case lType' of
          GFloat -> BitCast
            { operand0 = lOperand
            , type' = i64
            , metadata = [] }
          lType | lType `elem` [ GInt, GChar ] -> ZExt
            { operand0 = lOperand
            , type' = i64
            , metadata = [] }

        elemCall <- newLabel "elemCall"

        addInstruction $ elemCall := Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable boolType $ case expType rexpr of
              GSet      _ -> isElemSetString
              GMultiset _ -> isElemMultisetString
              GSeq      _ -> isElemSeqString
              _           -> internal "impossible type for elem argument"
          , arguments = (,[]) <$> [rOperand, LocalReference i64 item]
          , functionAttributes = []
          , metadata = [] }

        let [trueDest, falseDest] = [true, false] & case binOp of
              Elem    -> id
              NotElem -> reverse

        terminate CondBr
          { condition = LocalReference i1 elemCall
          , trueDest
          , falseDest
          , metadata' = [] }
    _ | binOp `elem` [LT, LE, GT, GE] -> do
      lOperand <- expr lexpr -- The operands can only be chars, ints
      rOperand <- expr rexpr -- or floats, nothing else

      subst <- use substitutionTable
      let
        type' = case subst of
          targs:_ -> fillType targs (expType lexpr <> expType rexpr)
          []      -> expType lexpr <> expType rexpr

      comp <- newLabel "comp"

      let
        compOp = case type' of
          GFloat -> FCmp $ case binOp of
            LT -> OLT
            LE -> OLE
            GT -> OGT
            GE -> OGE
          _ | type' `elem` [GInt, GChar, GBool] -> ICmp $ case binOp of
            LT -> SLT
            LE -> SLE
            GT -> SGT
            GE -> SGE
          _ -> internal $ "invalid comparison type " <> show type' <> " " <> show loc

      addInstruction $ comp := compOp lOperand rOperand []

      terminate CondBr
        { condition = LocalReference i1 comp
        , trueDest  = true
        , falseDest = false
        , metadata' = [] }

    _ | binOp `elem` [AEQ, ANE, BEQ, BNE] -> do
      subst <- use substitutionTable
      let
        type' = case subst of
          targs:_ -> fillType targs (expType lexpr <> expType rexpr)
          []      -> expType lexpr <> expType rexpr

      [lOperand, rOperand] <- [lexpr, rexpr] `forM` \x ->
        case type' of
          GBool -> wrapBoolean' expr object obRef x

          _ |  type' =:= highLevel || type' =:= GTuple GAny GAny
            || type' `elem` [GInt, GChar, GFloat] -> expr x

          _ | type' =:= GPointer GAny -> do
            cast <- newLabel "ptrEqCast"
            operand0 <- expr x
            addInstruction $ cast := PtrToInt
              { operand0
              , type'    = i64
              , metadata = [] }
            pure $ LocalReference i64 cast

      comp <- newLabel "eqComp"

      case type' of
        _ | type' =:= GOneOf [GBool, GChar, GInt, GPointer GAny] ->
          addInstruction $ comp := ICmp
            { iPredicate = EQ
            , operand0 = lOperand
            , operand1 = rOperand
            , metadata = [] }
        GFloat ->
          addInstruction $ comp := FCmp
            { fpPredicate = OEQ
            , operand0 = lOperand
            , operand1 = rOperand
            , metadata = [] }

        _ |  type' =:= highLevel || type' =:= GTuple GAny GAny -> do
          addInstruction $ comp := Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function = callable i1 $ case type' of
              GSet      _ -> equalSetString
              GMultiset _ -> equalMultisetString
              GSeq      _ -> equalSeqString
              GFunc   _ _ -> equalFuncString
              GRel    _ _ -> equalRelString
              GTuple  _ _ -> equalTupleString
            , arguments = (,[]) <$> [lOperand, rOperand]
            , functionAttributes = []
            , metadata = [] }

      let [trueDest, falseDest] = [true, false] & if
            | binOp `elem` [AEQ, BEQ] -> id
            | binOp `elem` [ANE, BNE] -> reverse
      terminate CondBr
        { condition = LocalReference i1 comp
        , trueDest
        , falseDest
        , metadata' = [] }

    _ | binOp `elem` [Subset, SSubset, Superset, SSuperset] -> do
      [lOp, rOp] <- mapM expr [lexpr, rexpr]

      comp <- newLabel "setComp"

      let [lOp', rOp'] = [lOp, rOp] & if
            | binOp `elem` [Subset, SSubset] -> reverse
            | binOp `elem` [Superset, SSuperset] -> id
      addInstruction $ comp := Call
        { tailCallKind = Nothing
        , callingConvention = CC.C
        , returnAttributes = []
        , function = callable i1 $ case expType lexpr of
          GSet      _ -> if
            | binOp `elem` [Subset, Superset]   -> supersetSetString
            | binOp `elem` [SSubset, SSuperset] -> ssupersetSetString
          GMultiset _ -> if
            | binOp `elem` [Subset, Superset]   -> supersetMultisetString
            | binOp `elem` [SSubset, SSuperset] -> ssupersetMultisetString
        , arguments = (,[]) <$> [lOp', rOp']
        , functionAttributes = []
        , metadata = [] }

      terminate CondBr
        { condition = LocalReference i1 comp
        , trueDest  = true
        , falseDest = false
        , metadata' = [] }

    SeqAt -> internal
      "boolean sequences are not implemented so ! can't produce a boolean"

    BifuncAt -> internal
      "boolean funcs/rels are not implemented so @ can't produce a boolean"

    -- Plus, BMinus, Times, Div, Mod, Power, Max, Min,
    -- Difference, Intersection, Union, MultisetSum, Concat
    _ -> internal $ "operator `" <> show binOp <> "` never produces a boolean"

  Unary { unOp, inner } -> case unOp of
    Not -> boolean false true inner
    _ -> internal $ "operator `" <> show unOp <> "` cannot produce a boolean"

  I64Cast {} -> internal "i64-cast cannot produce a boolean"

  FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc, fStructArgs } -> do
    arguments <- toList <$> mapM createArg fArgs

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
      , function           = callable i1 fName'
      , arguments          = recArgs <> arguments
      , functionAttributes = []
      , metadata           = [] }

    terminate CondBr
      { condition = LocalReference i1 label
      , trueDest  = true
      , falseDest = false
      , metadata' = [] }

    where
      createArg x@Expression { expType, exp' } = do
        subst <- use substitutionTable
        let
          type' = case subst of
            targs:_ -> fillType targs expType
            []      -> expType

        (,[]) <$> if
          | type' == GBool -> wrapBoolean' expr object obRef x
          | type' =:= basicT -> expr x
          | otherwise -> do
            label <- newLabel "argCast"
            ref   <- obRef (theObj exp') False

            type' <- ptr <$> toLLVMType type'
            addInstruction $ label := BitCast
              { operand0 = ref
              , type'    = type'
              , metadata = [] }
            pure $ LocalReference type' label

      basicT = GOneOf [ GChar, GInt, GFloat, GString ]

  Quantification {} -> boolQ expr boolean true false e

  EConditional { eguards, trueBranch } -> do
    mapM_ guard eguards

    case trueBranch of
      Nothing -> abort Abort.If (pos loc)
      Just e  -> boolean true false e

    where
      guard (left, right) = do
        yes <- newLabel "ifExpGuardYes"
        no  <- newLabel "ifExpGuardNo"

        boolean yes no left

        (yes #)
        boolean true false right

        (no #)

wrapBoolean' :: (Expression -> LLVM Operand)
            -> (Object -> LLVM Operand) -- ^ object code generator (both boolean and non-boolean)
            -> (Object -> Bool -> LLVM Operand) -- ^ object ref code generator (both boolean and non-boolean)
            -> Expression
            -> LLVM Operand
wrapBoolean' _ _ _ Expression { expType }
  | expType /= GBool = internal
    "attempted to generate non-boolean expression with `wrapBoolean` instead of `expression`"
wrapBoolean' expr object obRef e@Expression { } = do
  [begin, true, false, end, val] <- mapM (newLabel . ("wrap" <>))
    ["Begin", "True", "False", "End", "Val"]

  terminate $ Br begin []

  (begin #)
  boolean' expr object obRef true false e

  (true #)
  terminate $ Br end []

  (false #)
  terminate $ Br end []

  (end #)
  addInstruction $ val := Phi
    { type' = i1
    , incomingValues =
      [ (ConstantOperand $ C.Int 1 1, true)
      , (ConstantOperand $ C.Int 1 0, false) ]
    , metadata = [] }

  pure $ LocalReference i1 val
