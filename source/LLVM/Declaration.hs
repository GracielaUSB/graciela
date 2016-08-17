{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Declaration
  ( declaration
  ) where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Declaration              (Declaration (..))
import           AST.Expression
import           Limits
import           LLVM.Expression
import           LLVM.State
import           LLVM.Type
import           SymbolTable
import qualified Type                         as T
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (%=), (.=))
import           Control.Monad                (zipWithM)
import           Data.Foldable                (toList)
import           Data.Monoid                  ((<>))
import           Data.Text                    (unpack)
import           Data.Word
import           LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Instruction (Named (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM [Named LLVM.Instruction]
declaration BadDeclaration {} =
  error "internal error: converting BadDeclaration to LLVM"

declaration Declaration { declType, declIds } =
  toList <$> mapM (alloc declType) declIds

declaration Initialization { declType, declPairs } = do
  allocations <- toList <$> mapM (alloc declType . fst) declPairs

  stores <- toList <$> mapM (store declType) declPairs

  pure $ allocations <> concat stores

{- Allocate a variable -}
alloc gtype lval = do
  let
    alloc' = LLVM.Alloca
      { allocatedType = toLLVMType gtype
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = []
      }
  return $ Name (unpack lval) := alloc'

{- Store an expression in a variable memory -}
store gtype (lval, expr) = do
  (value, insts) <- expression expr
  let
    store = LLVM.Store
      { volatile = False
      , address  = LocalReference (toLLVMType gtype) (Name (unpack lval))
      , value    = value
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = []
      }
  -- The store is an unamed instruction, so get the next instruction label
  label <- nextLabel
  return $ insts <> [label := store]
