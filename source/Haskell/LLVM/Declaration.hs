{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Declaration
  ( declaration
  ) where
--------------------------------------------------------------------------------
import           AST.Declaration              (Declaration (..))
import           AST.Expression
import           LLVM.Abort
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           SymbolTable
import           Type                         (Type (..), (=:=))
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (%=), (.=))
import           Control.Monad                (when, zipWithM_)
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq (empty, fromList,
                                                      singleton)
import           Data.Text                    (Text, unpack)
import           Data.Word
import           Debug.Trace
import qualified LLVM.General.AST.Constant    as C (Constant (..))
import qualified LLVM.General.AST.Float       as LLVM (SomeFloat (Double))
import           LLVM.General.AST.Instruction (Instruction (..), Named (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

import           Debug.Trace

declaration :: Declaration -> LLVM ()
declaration Declaration { declType, declIds } =
  mapM_ (alloc declType) declIds

declaration Initialization { declType, declPairs } =
  mapM_ (initialize declType) declPairs

{- Allocate a variable -}
alloc :: Type -> Text -> LLVM ()
alloc gtype lval = do
  name <- insertName $ unpack lval
  t    <- toLLVMType gtype

  addInstruction $ Name name := Alloca
    { allocatedType = t
    , numElements   = Nothing
    , alignment     = 4
    , metadata      = [] }

  when (gtype =:= GOneOf [GInt, GChar, GFloat, GBool, GPointer GAny]) $ do
      defaultValue <- value gtype
      addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t (Name name)
          , value    = defaultValue
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = []
          }
  where 
    value t = case t of 
      GBool    -> pure $ ConstantOperand $ C.Int 1 0
      GChar    -> pure $ ConstantOperand $ C.Int 8 0
      GInt     -> pure $ ConstantOperand $ C.Int 32 0
      GFloat   -> pure $ ConstantOperand $ C.Float $ LLVM.Double 0
      t@(GPointer _) -> ConstantOperand . C.Null  <$> toLLVMType t


{- Store an expression in a variable memory -}
initialize :: Type -> (Text, Expression) -> LLVM ()
initialize gtype (lval, expr) = do
  name <- insertName $ unpack lval
  t    <- toLLVMType gtype

  addInstruction $ Name name := Alloca
    { allocatedType = t
    , numElements   = Nothing
    , alignment     = 4
    , metadata      = [] }

  value <- expression expr
  -- The store is an unamed instruction, so get the next instruction label
  addInstruction $ Do Store
    { volatile = False
    , address  = LocalReference t (Name name)
    , value    = value
    , maybeAtomicity = Nothing
    , alignment = 4
    , metadata  = [] }
