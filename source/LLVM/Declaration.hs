{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Declaration
  ( declaration
  ) where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Declaration              (Declaration (..))
import           AST.Expression
import           LLVM.Expression
import           LLVM.State
import           LLVM.Type
import           SymbolTable
import           Type                         (Type)
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (%=), (.=))
import           Control.Monad                (zipWithM_)
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq (empty, fromList,
                                                      singleton)
import           Data.Text                    (Text, unpack)
import           Data.Word
import           LLVM.General.AST.Instruction (Instruction (..), Named ((:=)))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM ()
declaration BadDeclaration {} =
  error "internal error: converting BadDeclaration to LLVM"

declaration Declaration { declType, declIds } =
  mapM_ (alloc declType) declIds

declaration Initialization { declType, declPairs } = do
  mapM_ (alloc declType . fst) declPairs

  mapM_ (store declType) declPairs

{- Allocate a variable -}
alloc :: Type -> Text -> LLVM ()
alloc gtype lval = do
  let
    alloc' = Alloca
      { allocatedType = toLLVMType gtype
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = []
      }
  addInstruction $ Name (unpack lval) := alloc'

{- Store an expression in a variable memory -}
store :: Type -> (Text, Expression) -> LLVM ()
store gtype (lval, expr) = do
  value <- expression expr
  let
    store = Store
      { volatile = False
      , address  = LocalReference (toLLVMType gtype) (Name (unpack lval))
      , value    = value
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = []
      }
  -- The store is an unamed instruction, so get the next instruction label
  label <- newLabel
  addInstruction (label := store)
