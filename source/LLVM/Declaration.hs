{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Declaration

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression
import           AST.Declaration                        (Declaration(..))

import           Limits
import           LLVM.State
import           LLVM.Expression
import           LLVM.Type                               
import           SymbolTable
import qualified Type                                    as T
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (zipWithM_)
import           Data.Monoid                             ((<>))
import           Data.Sequence                           (Seq)
import qualified Data.Sequence                           as Seq (singleton,
                                                         fromList, empty)
import           Data.Text                               (unpack)
import           Data.Word
import           LLVM.General.AST.Name                  (Name(..))
import           LLVM.General.AST.Instruction           (Instruction(..))
import           LLVM.General.AST.Instruction           (Named(..))
import           LLVM.General.AST.Operand               (Operand(..), CallableOperand)
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM ()
declaration Declaration {declType, declLvals, declExprs } = do 
  
  if null declExprs
    then mapM_ alloc declLvals
    else do 
      mapM_ alloc declLvals
      zipWithM_ store declLvals declExprs


  where
    {- LLVM type of all the variables in the declaration -}
    type' = toLLVMType declType
    
    {- Allocate a variable -}
    alloc lval = do 
      let alloc' = Alloca 
            { allocatedType = type'
            , numElements   = Nothing
            , alignment     = 4
            , metadata      = []
            }
      addInstructions $ Seq.singleton $ Name (unpack lval) := alloc'

    {- Store an expression in a variable memory -}
    store lval expr = do
      value <- expression expr
      let store = Store
            { volatile = False
            , address  = LocalReference type' $ Name (unpack lval) 
            , value    = value
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = []
            }
      -- The store is an unamed instruction, so get the next instrucction label
      label <- newLabel
      addInstructions $ Seq.singleton (label := store)
      