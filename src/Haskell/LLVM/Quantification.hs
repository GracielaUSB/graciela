{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}

module LLVM.Quantification
  ( quantification
  , boolQ
  , collection
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} LLVM.Expression (expression, safeOperation)
import {-# SOURCE #-} LLVM.Boolean    (boolean)
--------------------------------------------------------------------------------
import           AST.Expression                          (CollectionKind (..),
                                                          Expression (..),
                                                          Expression' (..),
                                                          QRange (..),
                                                          QuantOperator (..))
import qualified AST.Expression                          as E (Expression (expType))
import           AST.Type                                (Type (..), (=:=))
import           Common
import           LLVM.Abort                              (abort)
import qualified LLVM.Abort                              as Abort (Abort (EmptyRange))
import           LLVM.Monad
import           LLVM.Type
--------------------------------------------------------------------------------
import           Data.Sequence                           (ViewL ((:<)), viewl)
import           Data.Word                               (Word32)
import qualified LLVM.General.AST.CallingConvention      as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant               as C (Constant (Float, Int, Undef, Null))
import qualified LLVM.General.AST.Float                  as F (SomeFloat (Double))
import           LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate (OGT, OLT))
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate (..))
import           LLVM.General.AST.Name                   (Name)
import           LLVM.General.AST.Operand                (Operand (..))
import           LLVM.General.AST.Type                   (i1, i8, i64, ptr)
import qualified LLVM.General.AST.Type                   as LLVM
import           Prelude                                 hiding (EQ)
--------------------------------------------------------------------------------

boolQ :: Name -- ^ true destination
      -> Name -- ^ false destination
      -> Expression -- ^ The Quantification for which to generate code
      -> LLVM ()
boolQ true false e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
  Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> case qRange of
    EmptyRange -> case qOp of
      ForAll -> terminate $ Br true  []
      Exists -> terminate $ Br false []

    PointRange { thePoint } -> case qVarType of
      GFloat -> do
        p <- expression thePoint

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
          , value    = p
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        yesCond <- newUnLabel
        boolean yesCond (case qOp of ForAll -> true; Exists -> false) qCond

        (yesCond #)
        boolean true false qBody

        closeScope

      _ | qVarType `elem` [GInt, GChar, GBool] ->
        boolQ true false e
          { exp' = exp'
            { qRange = ExpRange
              { low  = thePoint
              , high = thePoint }}}
      _ -> internal $ "bad point range type " <> show qVarType

    ExpRange { low, high }
      | qOp `elem` [ForAll, Exists] -> do
        l <- expression low
        h <- expression high

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
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

        l0 <- newUnLabel
        addInstruction $ l0 := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = l
          , metadata   = [] }

        l1 <- newUnLabel
        terminate $ CondBr
          { condition = LocalReference i1 l0
          , trueDest  = case qOp of ForAll -> true; Exists -> false
          , falseDest = l1
          , metadata' = [] }

        (l1 #)
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = LocalReference t nextIterator
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        bound <- newLabel "qBound"
        addInstruction $ bound := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = h
          , metadata   = [] }
        terminate CondBr
          { condition = LocalReference i1 bound
          , trueDest  = loop
          , falseDest = case qOp of ForAll -> true; Exists -> false
          , metadata' = [] }

        closeScope

    SetRange { theSet = theSet@Expression{ expType = setType } } -> do
      set   <- expression theSet
      empty <- newLabel "emptySet"

      addInstruction $ empty := Call
        { tailCallKind = Nothing
        , callingConvention = CC.C
        , returnAttributes = []
        , function = callable (ptr i8) $ case setType of
          GSet      _ -> newSetString
          GMultiset _ -> newMultisetString
          GSeq      _ -> newSeqString
        , arguments = []
        , functionAttributes = []
        , metadata = [] }

      checkRange <- newLabel "qCheckRange"
      addInstruction $ checkRange := Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function = callable boolType $ case setType of
              GSet      _ -> equalSetString
              GMultiset _ -> equalMultisetString
              GSeq      _ -> equalSeqString
            , arguments = (,[]) <$> [set, LocalReference (ptr i8) empty]
            , functionAttributes = []
            , metadata = [] }

      rangeNotEmpty <- newLabel "qRangeNotEmpty"
      terminate CondBr
        { condition = LocalReference i1 checkRange
        , trueDest  = case qOp of
          ForAll -> true
          Exists -> false
        , falseDest = rangeNotEmpty
        , metadata' = [] }

      (rangeNotEmpty #)
      openScope

      iteratorVar <- insertVar qVar
      t        <- toLLVMType qVarType
      addInstruction $ iteratorVar := Alloca
        { allocatedType = t
        , numElements   = Nothing
        , alignment     = 4
        , metadata      = [] }

      (iteratorStruct, firstElement) <- firstIterator qVarType setType t set

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iteratorVar
        , value    = LocalReference t firstElement
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

      nextIterator <- newLabel "qNextIterator"
      addInstruction $ nextIterator := Call
        { tailCallKind = Nothing
        , callingConvention = CC.C
        , returnAttributes = []
        , function = callable (ptr iterator) $ case setType of
              GSet      _ -> nextSetString
              GMultiset _ -> nextMultisetString
              GSeq      _ -> nextSequenceString
        , arguments = [(LocalReference (ptr iterator) iteratorStruct, [])]
        , functionAttributes = []
        , metadata = [] }

      l0 <- newUnLabel
      addInstruction $ l0 := ICmp
        { iPredicate = EQ
        , operand0   = LocalReference (ptr iterator) nextIterator
        , operand1   = ConstantOperand . C.Null $ ptr iterator
        , metadata   = [] }

      l1 <- newUnLabel
      terminate $ CondBr
        { condition = LocalReference i1 l0
        , trueDest  = case qOp of ForAll -> true; Exists -> false
        , falseDest = l1
        , metadata' = [] }

      (l1 #)
  
      nextCast <- nextIteratorValue qVarType t nextIterator iteratorStruct

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iteratorVar
        , value    = LocalReference t nextCast
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      terminate Br
        { dest      = loop
        , metadata' = [] }

      closeScope


  _ -> internal "boolQ only admits Quantification Expression"


quantification :: Expression -- ^ The Quantification for which to generate code
               -> LLVM Operand
quantification e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
  Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> case qRange of
    EmptyRange
      | qOp `elem` [ Minimum, Maximum ] -> do
        abort Abort.EmptyRange pos
        newUnLabel >>= (#)
        ConstantOperand . C.Undef  <$> toLLVMType expType
      | otherwise -> pure . ConstantOperand $ v0 qOp expType

    PointRange { thePoint } -> case qVarType of
      GFloat -> do

        p <- expression thePoint

        openScope
        iterator <- insertVar qVar
        t        <- toLLVMType qVarType
        eType    <- toLLVMType expType
        addInstruction $ iterator := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = p
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        result  <- newLabel "Result"
        yesCond <- newUnLabel
        noCond  <- newUnLabel
        exit    <- newUnLabel


        boolean yesCond noCond qCond
        (yesCond #)
        
        op <- if qOp == Count
          then do 
            let 
              intSize = case expType of 
                GInt  -> 32
                GChar -> 8
                GBool -> 1

            countTrue  <- newLabel "countTrue"
            countFalse <- newLabel "countFalse"
            boolean countTrue countFalse qBody
            
            (countTrue #)
            terminate Br
              { dest      = exit
              , metadata' = [] }
            
            (countFalse #)
            terminate Br
              { dest      = exit
              , metadata' = [] }

            (noCond #)
            terminate Br
              { dest      = exit
              , metadata' = [] }

            (exit #)
            addInstruction $ result := Phi
              { type'          = eType
              , incomingValues =
                [ (ConstantOperand $ C.Int intSize 0, noCond)
                , (ConstantOperand $ C.Int intSize 1, countTrue)
                , (ConstantOperand $ C.Int intSize 0, countFalse) ]
              , metadata       = [] }

            pure $ LocalReference eType result
          else if qOp `elem` [Maximum, Minimum]
            then do
              e <- expression qBody
              terminate Br
                { dest      = exit
                , metadata' = [] }

              (noCond #)
              terminate Br
                { dest      = exit
                , metadata' = [] }

              (exit #)
              addInstruction $ result := Phi
                { type'          = eType
                , incomingValues =
                  [ (e, yesCond)
                  , (ConstantOperand . C.Float $ F.Double 0.0, noCond ) ]
                , metadata       = [] }

              pure $ LocalReference eType result
          else do
              e <- expression qBody
              terminate Br
                { dest      = exit
                , metadata' = [] }

              (noCond #)
              terminate Br
                { dest      = exit
                , metadata' = [] }

              (exit #)
              let dv = if qOp == Summation then 0.0 else 1.0
              addInstruction $ result := Phi
                { type'          = eType
                , incomingValues =
                  [ (e, yesCond)
                  , (ConstantOperand . C.Float $ F.Double dv, noCond ) ]
                , metadata       = [] }

              pure $ LocalReference eType result

        closeScope

        pure op

      _ | qVarType `elem` [GInt, GChar, GBool] ->
        quantification e
          { exp' = exp'
            { qRange = ExpRange
              { low = thePoint
              , high = thePoint }}}

    ExpRange { low, high }
      | qOp `elem` [Maximum, Minimum]   -> do
        l <- expression low
        h <- expression high

        qType <- toLLVMType expType

        qEnd <- newLabel "qEnd"

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
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
        e <- expression qBody

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
            C.Int (case qVarType of GInt -> 32; GChar -> 8; GBool -> 1) 1
          , metadata = [] }

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iterator
          , value    = LocalReference t nextIterator
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        bound <- newLabel "qBound"
        l0 <- newUnLabel
        addInstruction $ l0 := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = l
          , metadata   = [] }

        l1 <- newUnLabel
        terminate $ CondBr
          { condition = LocalReference i1 l0
          , trueDest  = qEnd
          , falseDest = l1
          , metadata' = [] }

        (l1 #)
        addInstruction $ bound := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
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
        l <- expression low
        h <- expression high

        qType <- toLLVMType expType

        qEnd <- newLabel "qEnd"

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
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
        newPartial <- newLabel "qNewPartial"
        oldPartial <- newLabel "qOldPartial"
        if qOp == Count
          then do
            plusOne <- newLabel "qPlusOne"
            e <- boolean plusOne getNext qBody

            (plusOne #)
            addInstruction $ oldPartial := Load
              { volatile       = False
              , address        = LocalReference qType partial
              , maybeAtomicity = Nothing
              , alignment      = 4
              , metadata       = [] }

            addInstruction $ newPartial := Add
              { nsw = False
              , nuw = False
              , operand0 = LocalReference qType oldPartial
              , operand1 = ConstantOperand $ C.Int 32 1
              , metadata = [] }


          else do
            e <- expression qBody
            addInstruction $ oldPartial := Load
              { volatile       = False
              , address        = LocalReference qType partial
              , maybeAtomicity = Nothing
              , alignment      = 4
              , metadata       = [] }
            

            case expType of
              GFloat ->
                addInstruction . (newPartial :=) $
                  (case qOp of Summation -> FAdd; Product -> FMul)
                    NoFastMathFlags
                    e (LocalReference qType oldPartial) []
              _ -> safeOperation
                  (case expType of GInt -> 32; GChar -> 8)
                  newPartial
                  (case qOp of Summation -> safeAdd; Product -> safeMul)
                  e (LocalReference qType oldPartial) pos

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

        l0 <- newUnLabel
        addInstruction $ l0 := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
          , operand0   = LocalReference t nextIterator
          , operand1   = l
          , metadata   = [] }

        endLoad <- newLabel "qEndLoad"
        l1 <- newUnLabel
        terminate $ CondBr
          { condition = LocalReference i1 l0
          , trueDest  = endLoad
          , falseDest = l1
          , metadata' = [] }

        (l1 #)
        bound <- newLabel "qBound"
        addInstruction $ bound := ICmp
          { iPredicate = case qVarType of GBool -> ULE; _ -> SLE
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

    SetRange { theSet = theSet@Expression{ expType = setType } }
      | qOp `elem` [Maximum, Minimum]   -> do
        qType <- toLLVMType expType
        qEnd <- newLabel "qEnd"

        set   <- expression theSet
        empty <- newLabel "emptySet"

        addInstruction $ empty := Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable (ptr i8) $ case setType of
            GSet      _ -> newSetString
            GMultiset _ -> newMultisetString
            GSeq      _ -> newSeqString
          , arguments = []
          , functionAttributes = []
          , metadata = [] }

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := Call
              { tailCallKind = Nothing
              , callingConvention = CC.C
              , returnAttributes = []
              , function = callable boolType $ case setType of
                GSet      _ -> equalSetString
                GMultiset _ -> equalMultisetString
                GSeq      _ -> equalSeqString
              , arguments = (,[]) <$> [set, LocalReference (ptr i8) empty]
              , functionAttributes = []
              , metadata = [] }

        rangeEmpty    <- newLabel " qRangeEmpty"
        rangeNotEmpty <- newLabel "qRangeNotEmpty"
        terminate CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeEmpty
          , falseDest = rangeNotEmpty
          , metadata' = [] }

        (rangeEmpty #)
        abort Abort.EmptyRange pos

        (rangeNotEmpty #)
        openScope

        iteratorVar <- insertVar qVar
        t        <- toLLVMType qVarType

        addInstruction $ iteratorVar := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
       
        (iteratorStruct, firstElement) <- firstIterator qVarType setType t set

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iteratorVar
          , value    = LocalReference t firstElement
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
        e <- expression qBody

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

        nextIterator <- newLabel "qNextIterator"
        addInstruction $ nextIterator := Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable (ptr iterator) $ case setType of
                GSet      _ -> nextSetString
                GMultiset _ -> nextMultisetString
                GSeq      _ -> nextSequenceString
          , arguments = [(LocalReference (ptr iterator) iteratorStruct, [])]
          , functionAttributes = []
          , metadata = [] }

        l0 <- newUnLabel
        addInstruction $ l0 := ICmp
          { iPredicate = EQ
          , operand0   = LocalReference (ptr iterator) nextIterator
          , operand1   = ConstantOperand . C.Null $ ptr iterator
          , metadata   = [] }

        l1 <- newUnLabel
        terminate $ CondBr
          { condition = LocalReference i1 l0
          , trueDest  = qEnd
          , falseDest = l1
          , metadata' = [] }

        (l1 #)
        nextCast <- nextIteratorValue qVarType t nextIterator iteratorStruct

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iteratorVar
          , value    = LocalReference t nextCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate Br
          { dest      = loop
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
        qType <- toLLVMType expType
        qEnd <- newLabel "qEnd"

        set   <- expression theSet
        empty <- newLabel "emptySet"

        addInstruction $ empty := Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable (ptr i8) $ case setType of
            GSet      _ -> newSetString
            GMultiset _ -> newMultisetString
            GSeq      _ -> newSeqString
          , arguments = []
          , functionAttributes = []
          , metadata = [] }

        checkRange <- newLabel "qCheckRange"
        addInstruction $ checkRange := Call
              { tailCallKind = Nothing
              , callingConvention = CC.C
              , returnAttributes = []
              , function = callable boolType $ case setType of
                GSet      _ -> equalSetString
                GMultiset _ -> equalMultisetString
                GSeq      _ -> equalSeqString
              , arguments = (,[]) <$> [set, LocalReference (ptr i8) empty]
              , functionAttributes = []
              , metadata = [] }

        rangeEmpty    <- newLabel " qRangeEmpty"
        rangeNotEmpty <- newLabel "qRangeNotEmpty"
        terminate CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeEmpty
          , falseDest = rangeNotEmpty
          , metadata' = [] }

        (rangeEmpty #)
        terminate Br
          { dest      = qEnd
          , metadata' = [] }

        (rangeNotEmpty #)
        openScope

        iteratorVar <- insertVar qVar
        t        <- toLLVMType qVarType

        addInstruction $ iteratorVar := Alloca
          { allocatedType = t
          , numElements   = Nothing
          , alignment     = 4
          , metadata      = [] }
       
        (iteratorStruct, firstElement) <- firstIterator qVarType setType t set

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iteratorVar
          , value    = LocalReference t firstElement
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

        case qOp of
          Count -> do
            evalBody <- newLabel "qEvalBody"
            boolean evalBody getNext qCond
            (evalBody #)
            boolean accum getNext qBody
          _     -> boolean accum getNext qCond

        (accum #)
        newPartial <- newLabel "qNewPartial"
        oldPartial <- newLabel "qOldPartial"
        if qOp == Count
          then do
            plusOne <- newLabel "qPlusOne"
            e <- boolean plusOne getNext qBody

            (plusOne #)
            addInstruction $ oldPartial := Load
              { volatile       = False
              , address        = LocalReference qType partial
              , maybeAtomicity = Nothing
              , alignment      = 4
              , metadata       = [] }

            addInstruction $ newPartial := Add
              { nsw = False
              , nuw = False
              , operand0 = LocalReference qType oldPartial
              , operand1 = ConstantOperand $ C.Int 32 1
              , metadata = [] }


          else do
            e <- expression qBody
            addInstruction $ oldPartial := Load
              { volatile       = False
              , address        = LocalReference qType partial
              , maybeAtomicity = Nothing
              , alignment      = 4
              , metadata       = [] }
            
            case expType of
              GFloat ->
                addInstruction . (newPartial :=) $
                  (case qOp of Summation -> FAdd; Product -> FMul)
                    NoFastMathFlags
                    e (LocalReference qType oldPartial) []
              _ -> safeOperation
                  (case expType of GInt -> 32; GChar -> 8)
                  newPartial
                  (case qOp of Summation -> safeAdd; Product -> safeMul)
                  e (LocalReference qType oldPartial) pos

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

        nextIterator <- newLabel "qNextIterator"
        addInstruction $ nextIterator := Call
          { tailCallKind = Nothing
          , callingConvention = CC.C
          , returnAttributes = []
          , function = callable (ptr iterator) $ case setType of
                GSet      _ -> nextSetString
                GMultiset _ -> nextMultisetString
                GSeq      _ -> nextSequenceString
          , arguments = [(LocalReference (ptr iterator) iteratorStruct, [])]
          , functionAttributes = []
          , metadata = [] }

        l0 <- newUnLabel
        addInstruction $ l0 := ICmp
          { iPredicate = EQ
          , operand0   = LocalReference (ptr iterator) nextIterator
          , operand1   = ConstantOperand . C.Null $ ptr iterator
          , metadata   = [] }

        l1 <- newUnLabel
        endLoad <- newLabel "qEndLoad"
        terminate $ CondBr
          { condition = LocalReference i1 l0
          , trueDest  = endLoad
          , falseDest = l1
          , metadata' = [] }

        (l1 #)
        nextCast <- nextIteratorValue qVarType t nextIterator iteratorStruct

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t iteratorVar
          , value    = LocalReference t nextCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate Br
          { dest      = loop
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
        result <- newLabel "qResult"
        addInstruction $ result := Phi
          { type'          = qType
          , incomingValues =
            [ (ConstantOperand $ v0 qOp expType, rangeEmpty)
            , (LocalReference qType finalVal,    endLoad)]
          , metadata       = [] }

        closeScope

        
        

        pure $ LocalReference qType result
      

      

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

collection e@Expression { loc = Location (pos, _), E.expType, exp' } = case exp' of
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

      collection e
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
        { iPredicate = case ty of GBool -> ULE; _ -> SLE
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

      l0 <- newUnLabel
      addInstruction $ l0 := ICmp
        { iPredicate = case ty of GBool -> ULE; _ -> SLE
        , operand0   = LocalReference t nextIterator
        , operand1   = l
        , metadata   = [] }

      l1 <- newUnLabel
      terminate $ CondBr
        { condition = LocalReference i1 l0
        , trueDest  = cEnd
        , falseDest = l1
        , metadata' = [] }

      (l1 #)
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
        { iPredicate = case ty of GBool -> ULE; _ -> SLE
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

    SetRange { theSet = theSet'@Expression{ expType = setType } } -> do

      cType <- toLLVMType $ case expType of
        GSet t      -> t
        GSeq t      -> t
        GMultiset t -> t

      theSet <- empty colKind colElems

      cEnd <- newLabel "cEnd"

      set   <- expression theSet'
      emptyS <- newLabel "emptySet"

      addInstruction $ emptyS := Call
        { tailCallKind = Nothing
        , callingConvention = CC.C
        , returnAttributes = []
        , function = callable (ptr i8) $ case setType of
          GSet      _ -> newSetString
          GMultiset _ -> newMultisetString
          GSeq      _ -> newSeqString
        , arguments = []
        , functionAttributes = []
        , metadata = [] }

      checkRange <- newLabel "qCheckRange"
      addInstruction $ checkRange := Call
            { tailCallKind = Nothing
            , callingConvention = CC.C
            , returnAttributes = []
            , function = callable boolType $ case setType of
              GSet      _ -> equalSetString
              GMultiset _ -> equalMultisetString
              GSeq      _ -> equalSeqString
            , arguments = (,[]) <$> [set, LocalReference (ptr i8) emptyS]
            , functionAttributes = []
            , metadata = [] }


      rangeEmpty    <- newLabel "cRangeEmpty"
      rangeNotEmpty <- newLabel "cRangeNotEmpty"
      terminate CondBr
        { condition = LocalReference i1 checkRange
        , trueDest  = rangeEmpty
        , falseDest = rangeNotEmpty
        , metadata' = [] }

      (rangeEmpty #)
      terminate Br
        { dest      = cEnd
        , metadata' = [] }

      (rangeNotEmpty #)
      openScope

      iteratorVar <- insertVar name
      t        <- toLLVMType ty
      addInstruction $ iteratorVar := Alloca
        { allocatedType = t
        , numElements   = Nothing
        , alignment     = 4
        , metadata      = [] }

      (iteratorStruct, firstElement) <- firstIterator ty setType t set

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iteratorVar
        , value    = LocalReference t firstElement
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
      nextIterator <- newLabel "qNextIterator"
      addInstruction $ nextIterator := Call
        { tailCallKind = Nothing
        , callingConvention = CC.C
        , returnAttributes = []
        , function = callable (ptr iterator) $ case setType of
              GSet      _ -> nextSetString
              GMultiset _ -> nextMultisetString
              GSeq      _ -> nextSequenceString
        , arguments = [(LocalReference (ptr iterator) iteratorStruct, [])]
        , functionAttributes = []
        , metadata = [] }

      l0 <- newUnLabel
      addInstruction $ l0 := ICmp
        { iPredicate = EQ
        , operand0   = LocalReference (ptr iterator) nextIterator
        , operand1   = ConstantOperand . C.Null $ ptr iterator
        , metadata   = [] }

      l1 <- newUnLabel
      terminate $ CondBr
        { condition = LocalReference i1 l0
        , trueDest  = cEnd
        , falseDest = l1
        , metadata' = [] }

      (l1 #)
      nextCast <- nextIteratorValue ty t nextIterator iteratorStruct

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference t iteratorVar
        , value    = LocalReference t nextCast
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      terminate Br
        { dest      = loop
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

firstIterator :: Type -> Type -> LLVM.Type -> Operand -> LLVM (Name, Name)
firstIterator qVarType setType castType set = do 
  iteratorStruct <- newLabel "iteratorStruct"
  first          <- newLabel "firstElementPtr"
  firstValue     <- newLabel "firstElementValue"
  cast           <- newLabel "castFirstElement"
  addInstruction $ iteratorStruct  := Call
    { tailCallKind = Nothing
    , callingConvention = CC.C
    , returnAttributes = []
    , function = callable (ptr iterator) $ case setType of
          GSet      _ -> firstSetString
          GMultiset _ -> firstMultisetString
          GSeq      _ -> firstSequenceString
    , arguments = [(set,[])]
    , functionAttributes = []
    , metadata = [] }

  addInstruction $ first := GetElementPtr
    { inBounds = False
    , address  = LocalReference (ptr iterator) iteratorStruct
    , indices  = ConstantOperand . C.Int 32 <$> [0, 0]
    , metadata = [] }
  
  addInstruction $ firstValue := Load
      { volatile       = False
      , address        = LocalReference (ptr i64) first
      , maybeAtomicity = Nothing
      , alignment      = 4
      , metadata       = [] }

  addInstruction . (cast :=) $ case qVarType of
    GFloat -> BitCast
      { operand0 = LocalReference i64 firstValue
      , type' = castType
      , metadata = [] }
    _ -> Trunc
      { operand0 = LocalReference i64 firstValue
      , type' = castType
      , metadata = [] }

  pure (iteratorStruct, cast)

nextIteratorValue :: Type -> LLVM.Type -> Name -> Name -> LLVM Name
nextIteratorValue qVarType castType nextIterator iteratorStruct = do
  newIteratorPtr <- newLabel "newIteratorPtr"
  addInstruction $ newIteratorPtr := Load
    { volatile       = False
    , address        = LocalReference (ptr i64) nextIterator
    , maybeAtomicity = Nothing
    , alignment      = 4
    , metadata       = [] }

  addInstruction $ Do Store
    { volatile = False
    , address  = LocalReference (ptr iterator) iteratorStruct
    , value    = LocalReference (ptr iterator) newIteratorPtr
    , maybeAtomicity = Nothing
    , alignment = 4
    , metadata  = [] }

  next <- newLabel "nextElementPtr"
  addInstruction $ next := GetElementPtr
    { inBounds = False
    , address  = LocalReference (ptr iterator) iteratorStruct
    , indices  = ConstantOperand . C.Int 32 <$> [0, 0]
    , metadata = [] }

  nextValue <- newLabel "nextElementValue"
  addInstruction $ nextValue := Load
      { volatile       = False
      , address        = LocalReference (ptr i64) next
      , maybeAtomicity = Nothing
      , alignment      = 4
      , metadata       = [] }

  nextCast <- newLabel "castNextElement"
  addInstruction . (nextCast :=) $ case qVarType of
    GFloat -> BitCast
      { operand0 = LocalReference i64 nextValue
      , type' = castType
      , metadata = [] }
    _ -> Trunc
      { operand0 = LocalReference i64 nextValue
      , type' = castType
      , metadata = [] }

  pure nextCast