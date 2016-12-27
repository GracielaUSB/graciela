{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PostfixOperators         #-}

module LLVM.Object
  ( object
  , objectRef
  ) where

--------------------------------------------------------------------------------
import {-# SOURCE #-} LLVM.Expression (expression)
--------------------------------------------------------------------------------
import qualified AST.Expression                    as E (loc)
import           AST.Object                        (Object (..), Object' (..))
import           AST.Type                          (ArgMode (..), Type (..),
                                                    basic, (=:=), highLevel)
import           Common
import           LLVM.Abort                        (abort)
import qualified LLVM.Abort                        as Abort (Abort (..))
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type                         (toLLVMType)
--------------------------------------------------------------------------------
import           Control.Lens                      (use)
import           Data.Maybe                        (isJust)
import Data.Text (unpack)
import qualified LLVM.General.AST.Constant         as C
import           LLVM.General.AST.Instruction      (Instruction (..),
                                                    Named (..), Terminator (..))
import           LLVM.General.AST.IntegerPredicate (IntegerPredicate (..))
import           LLVM.General.AST.Operand          (Operand (..))
import           LLVM.General.AST.Type             (Type (ArrayType), i1, i32,
                                                    i64, ptr)
import           LLVM.General.AST.Name (Name(..))
import           Prelude                           hiding (Ordering (..))
--------------------------------------------------------------------------------

object :: Object -> LLVM Operand
object obj@Object { objType, obj' } = do 
  dfunc <- use doingFunction
  objRef <- objectRef obj
  case obj' of
    Variable {name, mode} | dfunc && t' && mode /= Nothing -> 
       pure objRef
    
    _ -> do
      label <- newLabel "varObj"
      t <- toLLVMType objType

      addInstruction $ label := Load 
        { volatile       = False
        , address        = objRef
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      pure $ LocalReference t label

  where 
    t' = objType =:= basic     || objType == I64 
      || objType =:= highLevel || objType =:= GATypeVar


-- objectRef :: Object -> LLVM Operand
-- objectRef o = fst <$> objectRef' o False


-- Get the reference to the object.
-- indicate that the object is not a deref, array access or field access inside a procedure
objectRef :: Object -> LLVM Operand
objectRef (Object loc objType obj') = do
  objType' <- toLLVMType objType
  
  ref <- case obj' of

    Variable { name , mode } -> do
      name' <- getVariableName name
      pure $ LocalReference objType' name'
      

    Index inner indices -> do
      ref <- objectRef inner

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
      ref        <- objectRef inner
      labelLoad  <- newLabel "derefLoad"
      labelCast  <- newLabel "derefCast"
      labelNull  <- newLabel "derefNull"
      labelCond  <- newLabel "derefCond"
      trueLabel  <- newLabel "derefNullTrue"
      falseLabel <- newLabel "derefNullFalse"
      let
        Location (p,_) = loc

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
        { condition = LocalReference i1 labelCond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }

      (trueLabel #)
      abort Abort.NullPointerAccess p

      (falseLabel #)

      pure . LocalReference objType' $ labelLoad


    Member { inner, field } -> do
      ref <- objectRef inner
      label <- newLabel $ "member" <> show field
      addInstruction $ label := GetElementPtr
          { inBounds = False
          , address  = ref
          , indices  = ConstantOperand . C.Int 32 <$> [0, field]
          , metadata = []}
      pure . LocalReference objType' $ label

  pure $ ref
    
