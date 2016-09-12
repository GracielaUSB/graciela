{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module LLVM.Quantification
  (quantification) where

--------------------------------------------------------------------------------
import           AST.Expression
import           AST.Type                          (Type (..))
import           LLVM.Abort                        (abort)
import qualified LLVM.Abort                        as Abort (Abort (EmptyRange))
import           LLVM.Monad
import           LLVM.Type
import           Location
--------------------------------------------------------------------------------
import qualified LLVM.General.AST.Constant         as C (Constant (Float, Int, Undef))
import qualified LLVM.General.AST.Float            as F (SomeFloat (Double))
import           LLVM.General.AST.Instruction      (Instruction (..),
                                                    Named (..), Terminator (..))
import           LLVM.General.AST.IntegerPredicate (IntegerPredicate (..))
import           LLVM.General.AST.Operand          (Operand (..))
import           LLVM.General.AST.Type             (i1, i32)
--------------------------------------------------------------------------------

quantification
  :: (Expression -> LLVM Operand) -- ^ The expression llvm-generator
  -> Expression                   -- ^ The Quantification for which to generate code
  -> LLVM Operand
quantification expr e@Expression { loc = Location (from, to), expType, exp' } = case exp' of
  Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> case qRange of
    EmptyRange
      | qOp `elem` [ Minimum, Maximum ] -> do
        abort Abort.EmptyRange from
        ConstantOperand . C.Undef  <$> toLLVMType expType
      | otherwise -> pure . ConstantOperand $ v0 qOp expType

    PointRange { thePoint } ->
      quantification expr e
        { exp' = exp'
          { qRange = ExpRange
            { low = thePoint
            , high = thePoint }}}

    ExpRange { low, high }
      | qOp `elem` [Maximum, Minimum]   -> pure . ConstantOperand $ C.Int 32 1
      | qOp `elem` [ForAll, Exists]     -> pure . ConstantOperand $ C.Int  1 1
      | qOp   ==    Count               -> pure . ConstantOperand $ C.Int 32 1
      | qOp `elem` [Summation, Product] -> do
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
        terminate' CondBr
          { condition = LocalReference i1 checkRange
          , trueDest  = rangeNotEmpty
          , falseDest = rangeEmpty
          , metadata' = [] }

        (rangeEmpty #)
        terminate' Br
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
        terminate' Br
          { dest      = loop
          , metadata' = [] }

        (loop #)
        cond <- expr qCond

        accum <- newLabel "qAccum"
        getNext <- newLabel "qNext"
        terminate' CondBr
          { condition = cond
          , trueDest  = accum
          , falseDest = getNext
          , metadata' = [] }

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
        addInstruction $ newPartial :=
          (case qOp of Summation -> Add; Product -> Mul)
            False
            False
            (LocalReference qType oldPartial)
            e
            []
        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference qType partial
          , value    = LocalReference qType newPartial
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate' Br
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
          , operand1 = ConstantOperand $ C.Int 32 1
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
        terminate' CondBr
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
        terminate' Br
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

  _ -> error "internal error: quantification only admits \
             \Quantification Expression"

  where
    v0 :: QuantOperator -> Type -> C.Constant
    v0 ForAll    _      = C.Int  1 1
    v0 Exists    _      = C.Int  1 0
    v0 Count     _      = C.Int 32 0
    v0 Summation GChar  = C.Int  8 0
    v0 Summation GInt   = C.Int 32 0
    v0 Summation GFloat = C.Float . F.Double $ 0
    v0 Summation _      = error "internal error: impossibly typed summation"
    v0 Product   GChar  = C.Int  8 1
    v0 Product   GInt   = C.Int 32 1
    v0 Product   GFloat = C.Float . F.Double $ 1
    v0 Product   _      = error "internal error: impossibly typed product"
    v0 Minimum   _      = undefined
    v0 Maximum   _      = undefined
