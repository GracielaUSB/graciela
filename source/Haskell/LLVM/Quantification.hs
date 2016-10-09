{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}


module LLVM.Quantification
  ( quantification
  , boolQ
  , collection) where
--------------------------------------------------------------------------------
import           AST.Expression                          (CollectionKind (..),
                                                          Expression' (..),
                                                          Expression'' (..),
                                                          QRange' (..),
                                                          QuantOperator (..))
import qualified AST.Expression                          as E (Expression' (expType))
import           AST.Type                                (Expression, QRange,
                                                          Type (..), (=:=))
import           Common
import           Error                                   (internal)
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (EmptyRange))
import           LLVM.Monad
import           LLVM.Type
import           Location
--------------------------------------------------------------------------------
import           Control.Monad                           (unless, when)
import           Data.Sequence                           (ViewL ((:<)), viewl)
import           Data.Word                               (Word32)
import qualified LLVM.General.AST.CallingConvention      as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant               as C (Constant (Float, Int, Undef))
import qualified LLVM.General.AST.Float                  as F (SomeFloat (Double))
import           LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate (OGT, OLT))
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate (..))
import           LLVM.General.AST.Name                   (Name)
import           LLVM.General.AST.Operand                (Operand (..))
import           LLVM.General.AST.Type                   (i1, i64)
--------------------------------------------------------------------------------

boolQ :: Num a
      => (Expression -> LLVM Operand) -- ^ The expression llvm-generator
      -> (Name -> Name -> Expression -> LLVM ()) -- ^ The boolean expression llvm-generator
      -> Name -- ^ true destination
      -> Name -- ^ false destination
      -> Expression -- ^ The Quantification for which to generate code
      -> LLVM ()
boolQ expr boolean true false e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
  Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> case qRange of
    EmptyRange -> case qOp of
      ForAll -> terminate $ Br true  []
      Exists -> terminate $ Br false []

    PointRange { thePoint } ->
      boolQ expr boolean true false e
        { exp' = exp'
          { qRange = ExpRange
            { low  = thePoint
            , high = thePoint }}}

    ExpRange { low, high }
      | qOp `elem` [ForAll, Exists] -> do
        l <- expr low
        h <- expr high

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = SLE
          , operand0   = l
          , operand1   = h
          , metadata   = [] }
        rangeNotEmpty <- newLabel "qRangeNotEmpty"
        terminate CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeNotEmpty
          , falseDest = case qOp of
            ForAll -> true
            Exists -> false
          , metadata' = [] }

        (rangeNotEmpty #)
        openScope

        iterator <- insertVar qVar
        t        <- toLLVMType qVarType
        addInstruction $ iterator := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = l
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        loop <- newLabel "qLoop"
        terminate Br
          { dest      = loop
          , metadata' = [] }

        (loop #)
        accum <- newLabel "qBody"
        getNext <- newLabel "qNext"

        boolean accum getNext qCond

        (accum #)
        let (true', false') = case qOp of
              ForAll -> (getNext, false)
              Exists -> (true,  getNext)
        boolean true' false' qBody

        (getNext #)
        prevIterator <- newLabel "qPrevIterator"
        addInstruction $ prevIterator := Load
          { volatile       = False
          , address        = LocalReference t iterator
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        nextIterator <- newLabel "qNextIterator"
        addInstruction $ nextIterator := Add
          { nsw = False
          , nuw = False
          , operand0 = LocalReference t prevIterator
          , operand1 = ConstantOperand $
            C.Int (case qVarType of GInt -> 32; GChar -> 8) 1
          , metadata = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = LocalReference t nextIterator
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        bound <- newLabel "qBound"
        addInstruction $ bound := ICmp
          { iPredicate = SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = h
          , metadata   = [] }
        terminate CondBr
          { condition = LocalReference i1 bound
          , trueDest  = loop
          , falseDest = case qOp of ForAll -> true; Exists -> false
          , metadata' = [] }

    SetRange { theSet } -> internal "set iteration not implemented yet"

  _ -> internal "boolQ only admits Quantification Expression"


quantification :: Num a
               => (Expression -> LLVM Operand) -- ^ The expression llvm-generator
               -> (Name -> Name -> Expression -> LLVM ()) -- ^ The boolean expression llvm-generator
               -> (a
                 -> Name
                 -> (Word32 -> String)
                 -> Operand
                 -> Operand
                 -> SourcePos
                 -> LLVM ()) -- ^ The safe operation llvm-generator
               -> Expression -- ^ The Quantification for which to generate code
               -> LLVM Operand
quantification expr boolean safe e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
  Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> case qRange of
    EmptyRange
      | qOp `elem` [ Minimum, Maximum ] -> do
        abort Abort.EmptyRange pos
        ConstantOperand . C.Undef  <$> toLLVMType expType
      | otherwise -> pure . ConstantOperand $ v0 qOp expType

    PointRange { thePoint } ->
      quantification expr boolean safe e
        { exp' = exp'
          { qRange = ExpRange
            { low = thePoint
            , high = thePoint }}}

    ExpRange { low, high }
      | qOp `elem` [Maximum, Minimum]   -> do
        l <- expr low
        h <- expr high

        qType <- toLLVMType expType

        qEnd <- newLabel "qEnd"

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = SLE
          , operand0   = l
          , operand1   = h
          , metadata   = [] }
        rangeEmpty    <- newLabel "qRangeEmpty"
        rangeNotEmpty <- newLabel "qRangeNotEmpty"
        terminate CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeNotEmpty
          , falseDest = rangeEmpty
          , metadata' = [] }

        (rangeEmpty #)
        abort Abort.EmptyRange pos

        (rangeNotEmpty #)
        openScope

        iterator <- insertVar qVar
        t        <- toLLVMType qVarType
        addInstruction $ iterator := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = l
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        partial <- newLabel "qPartial"
        addInstruction $ partial := Alloca
          { allocatedType = qType
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }

        valid <- newLabel "qValid"
        addInstruction $ valid := Alloca
          { allocatedType = i1
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType valid
          , value    = ConstantOperand $ C.Int 1 0
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        loop <- newLabel "qLoop"
        terminate Br
          { dest      = loop
          , metadata' = [] }

        (loop #)
        accum <- newLabel "qAccum"
        getNext <- newLabel "qNext"

        boolean accum getNext qCond

        (accum #)
        e <- expr qBody

        oldValid <- newLabel "qOldValid"
        addInstruction $ oldValid := Load
          { volatile       = False
          , address        = LocalReference qType valid
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }
        checkAccum <- newLabel "qCheckAccum"
        justAccum  <- newLabel "qJustAccum"
        justAccum' <- newLabel "qJustAccum1"
        terminate CondBr
          { condition = LocalReference i1 oldValid
          , trueDest  = checkAccum
          , falseDest = justAccum
          , metadata' = [] }

        (checkAccum #)
        oldPartial <- newLabel "qOldPartial"
        addInstruction $ oldPartial := Load
          { volatile       = False
          , address        = LocalReference qType partial
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }
        comp <- newLabel "qComp"
        addInstruction $ comp := case expType of
          GFloat -> FCmp
            { fpPredicate = case qOp of
              Maximum -> OGT
              Minimum -> OLT
            , operand0 = e
            , operand1 = LocalReference qType oldPartial
            , metadata = [] }
          _      -> ICmp
            { iPredicate = case qOp of
              Maximum -> SGT
              Minimum -> SLT
            , operand0 = e
            , operand1 = LocalReference qType oldPartial
            , metadata = [] }
        terminate CondBr
          { condition = LocalReference i1 comp
          , trueDest  = justAccum'
          , falseDest = getNext
          , metadata' = [] }

        (justAccum #)
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType valid
          , value    = ConstantOperand $ C.Int 1 1
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }
        terminate Br
          { dest      = justAccum'
          , metadata' = [] }

        (justAccum' #)
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType partial
          , value    = e
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate Br
          { dest      = getNext
          , metadata' = [] }

        (getNext #)
        prevIterator <- newLabel "qPrevIterator"
        addInstruction $ prevIterator := Load
          { volatile       = False
          , address        = LocalReference qType iterator
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        nextIterator <- newLabel "qNextIterator"
        addInstruction $ nextIterator := Add
          { nsw = False
          , nuw = False
          , operand0 = LocalReference qType prevIterator
          , operand1 = ConstantOperand $
            C.Int (case qVarType of GInt -> 32; GChar -> 8) 1
          , metadata = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = LocalReference t nextIterator
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        bound <- newLabel "qBound"
        addInstruction $ bound := ICmp
          { iPredicate = SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = h
          , metadata   = [] }
        terminate CondBr
          { condition = LocalReference i1 bound
          , trueDest  = loop
          , falseDest = qEnd
          , metadata' = [] }

        (qEnd #)
        closeScope

        finalValid <- newLabel "qFinalValid"
        addInstruction $ finalValid := Load
          { volatile       = False
          , address        = LocalReference qType valid
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        validResult   <- newLabel "qValidResult"
        invalidResult <- newLabel "qInvalidResult"
        terminate CondBr
          { condition = LocalReference i1 finalValid
          , trueDest  = validResult
          , falseDest = invalidResult
          , metadata' = [] }

        (invalidResult #)
        abort Abort.EmptyRange pos

        (validResult #)
        result <- newLabel "qResult"
        addInstruction $ result := Load
          { volatile       = False
          , address        = LocalReference qType partial
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        pure $ LocalReference qType result

      | qOp `elem` [Summation, Product, Count] -> do
        l <- expr low
        h <- expr high

        qType <- toLLVMType expType

        qEnd <- newLabel "qEnd"

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = SLE
          , operand0   = l
          , operand1   = h
          , metadata   = [] }
        rangeEmpty    <- newLabel "qRangeEmpty"
        rangeNotEmpty <- newLabel "qRangeNotEmpty"
        terminate CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeNotEmpty
          , falseDest = rangeEmpty
          , metadata' = [] }

        (rangeEmpty #)
        terminate Br
          { dest      = qEnd
          , metadata' = [] }

        (rangeNotEmpty #)
        openScope

        iterator <- insertVar qVar
        t        <- toLLVMType qVarType
        addInstruction $ iterator := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = l
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        partial <- newLabel "qPartial"
        addInstruction $ partial := Alloca
          { allocatedType = qType
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType partial
          , value    = ConstantOperand $ v0 qOp expType
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        loop <- newLabel "qLoop"
        terminate Br
          { dest      = loop
          , metadata' = [] }

        (loop #)
        accum <- newLabel "qAccum"
        getNext <- newLabel "qNext"

        boolean accum getNext qCond

        (accum #)
        e <- expr qBody
        oldPartial <- newLabel "qOldPartial"
        addInstruction $ oldPartial := Load
          { volatile       = False
          , address        = LocalReference qType partial
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }
        newPartial <- newLabel "qNewPartial"

        case expType of
          GFloat ->
            addInstruction . (newPartial :=) $
              (case qOp of Summation -> FAdd; Product -> FMul)
                NoFastMathFlags
                e
                (LocalReference qType oldPartial)
                []
          _ -> case qOp of
            Count -> do
              plusOne <- newLabel "qPlusOne"
              terminate CondBr
                { condition = e
                , trueDest  = plusOne
                , falseDest = getNext
                , metadata' = [] }

              (plusOne #)
              addInstruction $ newPartial := Add
                { nsw = False
                , nuw = False
                , operand0 = LocalReference qType oldPartial
                , operand1 = ConstantOperand $ C.Int 32 1
                , metadata = [] }

            _ -> safe
              (case expType of GInt -> 32; GChar -> 8)
              newPartial
              (case qOp of Summation -> safeAdd; Product -> safeMul)
              e
              (LocalReference qType oldPartial)
              pos

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType partial
          , value    = LocalReference qType newPartial
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate Br
          { dest      = getNext
          , metadata' = [] }

        (getNext #)
        prevIterator <- newLabel "qPrevIterator"
        addInstruction $ prevIterator := Load
          { volatile       = False
          , address        = LocalReference qType iterator
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        nextIterator <- newLabel "qNextIterator"
        addInstruction $ nextIterator := Add
          { nsw = False
          , nuw = False
          , operand0 = LocalReference qType prevIterator
          , operand1 = ConstantOperand $
            C.Int (case qVarType of GInt -> 32; GChar -> 8) 1
          , metadata = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = LocalReference t nextIterator
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        bound <- newLabel "qBound"
        endLoad <- newLabel "qEndLoad"
        addInstruction $ bound := ICmp
          { iPredicate = SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = h
          , metadata   = [] }
        terminate CondBr
          { condition = LocalReference i1 bound
          , trueDest  = loop
          , falseDest = endLoad
          , metadata' = [] }

        (endLoad #)
        finalVal <- newLabel "qFinalVal"
        addInstruction $ finalVal := Load
          { volatile       = False
          , address        = LocalReference qType partial
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }
        terminate Br
          { dest      = qEnd
          , metadata' = [] }

        (qEnd #)
        closeScope

        result <- newLabel "qResult"
        addInstruction $ result := Phi
          { type'          = qType
          , incomingValues =
            [ (ConstantOperand $ v0 qOp expType, rangeEmpty)
            , (LocalReference qType finalVal,    endLoad) ]
          , metadata       = [] }

        pure $ LocalReference qType result

    SetRange { theSet } -> pure . ConstantOperand $ C.Int 32 1

  _ -> internal "quantification only admits \
             \Quantification Expression"

  where
    v0 :: QuantOperator -> Type -> C.Constant
    v0 Count     _      = C.Int 32 0
    v0 Summation GChar  = C.Int  8 0
    v0 Summation GInt   = C.Int 32 0
    v0 Summation GFloat = C.Float . F.Double $ 0
    v0 Summation _      = internal "impossibly typed summation"
    v0 Product   GChar  = C.Int  8 1
    v0 Product   GInt   = C.Int 32 1
    v0 Product   GFloat = C.Float . F.Double $ 1
    v0 Product   _      = internal "impossibly typed product"
    v0 Minimum   _      = undefined
    v0 Maximum   _      = undefined

collection expression boolean e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
  Collection { colKind, colVar = Nothing, colElems } -> do
      theSet <- empty colKind colElems
      unless (null colElems) $
        mapM_ (callInsert colKind theSet) colElems
      pure theSet

  Collection { colKind, colVar = Just (name, ty, range, cond), colElems } -> case range of
    EmptyRange -> empty colKind colElems

    PointRange { thePoint } -> do
      let range' = ExpRange
            { low = thePoint
            , high = thePoint }

      collection expression boolean e
        { exp' = exp'
          { colVar = Just (name, ty, range', cond) }}

    ExpRange { low, high } -> do
      l <- expression low
      h <- expression high

      cType <- toLLVMType $ case expType of
        GSet t      -> t
        GSeq t      -> t
        GMultiset t -> t

      theSet <- empty colKind colElems

      cEnd <- newLabel "cEnd"

      checkRange <- newLabel "cCheckRange"
      addInstruction $ checkRange := ICmp
        { iPredicate = SLE
        , operand0   = l
        , operand1   = h
        , metadata   = [] }
      rangeEmpty    <- newLabel "cRangeEmpty"
      rangeNotEmpty <- newLabel "cRangeNotEmpty"
      terminate CondBr
        { condition = LocalReference i1 checkRange
        , trueDest  = rangeNotEmpty
        , falseDest = rangeEmpty
        , metadata' = [] }

      (rangeEmpty #)
      terminate Br
        { dest      = cEnd
        , metadata' = [] }

      (rangeNotEmpty #)
      openScope

      iterator <- insertVar name
      t        <- toLLVMType ty
      addInstruction $ iterator := Alloca
        { allocatedType = t
        , numElements   = Nothing
        , alignment     = 4
        , metadata      = [] }
      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iterator
        , value    = l
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      loop <- newLabel "qLoop"
      terminate Br
        { dest      = loop
        , metadata' = [] }

      (loop #)
      accum <- newLabel "cAccum"
      getNext <- newLabel "cNext"

      boolean accum getNext cond

      (accum #)

      mapM_ (callInsert colKind theSet) colElems

      terminate Br
        { dest      = getNext
        , metadata' = [] }

      (getNext #)
      prevIterator <- newLabel "cPrevIterator"
      addInstruction $ prevIterator := Load
        { volatile       = False
        , address        = LocalReference cType iterator
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      nextIterator <- newLabel "cNextIterator"
      addInstruction $ nextIterator := Add
        { nsw = False
        , nuw = False
        , operand0 = LocalReference cType prevIterator
        , operand1 = ConstantOperand $
          C.Int (case ty of GInt -> 32; GChar -> 8) 1
        , metadata = [] }
      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iterator
        , value    = LocalReference t nextIterator
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      bound <- newLabel "cBound"
      endLoad <- newLabel "cEndLoad"
      addInstruction $ bound := ICmp
        { iPredicate = SLE
        , operand0   = LocalReference t nextIterator
        , operand1   = h
        , metadata   = [] }
      terminate CondBr
        { condition = LocalReference i1 bound
        , trueDest  = loop
        , falseDest = cEnd
        , metadata' = [] }

      (cEnd #)
      closeScope

      pure theSet


  _ -> internal "collection only admits \
             \Collection Expression"

  where
    empty colKind colElems = do

      theSet <- newLabel "theSet"
      t <- toLLVMType expType
      case viewl colElems of
        t' :< _ | E.expType t' =:= GTuple GAny GAny ->
          addInstruction $ theSet := Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function = callable t $ case colKind of
              Set      -> newSetPairString
              Multiset -> newMultisetPairString
              Sequence -> newSeqPairString
            , arguments = []
            , functionAttributes = []
            , metadata = [] }

        otherwise ->
          addInstruction $ theSet := Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function = callable t $ case colKind of
              Set      -> newSetString
              Multiset -> newMultisetString
              Sequence -> newSeqString
            , arguments = []
            , functionAttributes = []
            , metadata = [] }

      pure $ LocalReference t theSet

    callInsert colKind theSet expr
      | E.expType expr =:= GTuple GAny GAny = do

        expr' <- expression expr
        t <- toLLVMType expType

        addInstruction $ Do Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable t $ case colKind of
            Set      -> insertSetPairString
            Multiset -> insertMultisetPairString
            Sequence -> insertSeqPairString
          , arguments = (,[]) <$> [theSet, expr']
          , functionAttributes = []
          , metadata = [] }

      | otherwise = do

        expr' <- expression expr
        t <- toLLVMType expType
        value <- newLabel "item_2"
        addInstruction . (value :=) $ case E.expType expr of
          GFloat -> BitCast
            { operand0 = expr'
            , type' = i64
            , metadata = [] }
          _ -> ZExt
            { operand0 = expr'
            , type' = i64
            , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable t $ case colKind of
            Set      -> insertSetString
            Multiset -> insertMultisetString
            Sequence -> insertSeqString
          , arguments = (,[]) <$> [theSet, LocalReference i64 value]
          , functionAttributes = []
          , metadata = [] }
