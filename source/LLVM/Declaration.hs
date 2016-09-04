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
import           Type                         (Type (..), (=:=))
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (%=), (.=))
import           Control.Monad                (zipWithM_, when)
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq (empty, fromList,
                                                      singleton)
import           Data.Text                    (Text, unpack)
import           Data.Word
import           Debug.Trace
import qualified LLVM.General.AST.Constant    as C (Constant (..))
import qualified LLVM.General.AST.Float       as LLVM (SomeFloat (Double))
import           LLVM.General.AST.Instruction (Instruction (..), Named ((:=)))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM ()
declaration Declaration { declType, declIds } = do
  mapM_ (alloc declType) declIds

declaration Initialization { declType, declPairs } = do
  mapM_ (initialize declType) declPairs



{- Allocate a variable -}
alloc :: Type -> Text -> LLVM ()
alloc gtype lval = do
  name <- insertName $ unpack lval
  let
    alloc' = Alloca
      { allocatedType = toLLVMType gtype
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = []
      }
  addInstruction $ Name name := alloc'

  when (gtype =:= GOneOf [GInt, GChar, GFloat, GBool, GPointer GAny]) $ do
      let
        store = Store
          { volatile = False
          , address  = LocalReference (toLLVMType gtype) (Name name)
          , value    = value gtype
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = []
          }
      label <- newLabel
      addInstruction (label := store)
  where 
    value GBool          = ConstantOperand $ C.Int 1 0
    value GChar          = ConstantOperand $ C.Int 8 0
    value GInt           = ConstantOperand $ C.Int 32 0
    value GFloat         = ConstantOperand $ C.Float $ LLVM.Double 0
    value t@(GPointer _) = ConstantOperand $ C.Null (toLLVMType  t)



{- Store an expression in a variable memory -}
initialize :: Type -> (Text, Expression) -> LLVM ()
initialize gtype (lval, expr) = do
  name <- insertName $ unpack lval
  let
    alloc' = Alloca
      { allocatedType = toLLVMType gtype
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = []
      }
  addInstruction $ Name name := alloc'
  value <- expression expr
  let
    store = Store
      { volatile = False
      , address  = LocalReference (toLLVMType gtype) (Name name)
      , value    = value
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata  = []
      }
  -- The store is an unamed instruction, so get the next instruction label
  label <- newLabel
  addInstruction $ label := store
