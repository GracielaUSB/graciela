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
import           Control.Monad                           (zipWithM)
import           Data.Monoid                             ((<>))
import           Data.Text                               (unpack)
import           Data.Word
import           LLVM.General.AST.Name                  (Name(..))
import           LLVM.General.AST.Instruction           as LLVM (Instruction(..))
import           LLVM.General.AST.Instruction           (Named(..))
import           LLVM.General.AST.Operand               (Operand(..), CallableOperand)
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM [Named LLVM.Instruction]
declaration Declaration {declType, declLvals, declExprs } = do 
  
  allocations <- mapM alloc declLvals
  
  -- Declarations can be, With Assign or Without Assign
  if null declExprs
    then return allocations
    else do 
      stores <- zipWithM store declLvals declExprs
      return  $ allocations <> concat stores

  where
    {- LLVM type of all the variables in the declaration -}
    type' = toLLVMType declType
    
    {- Allocate a variable -}
    alloc lval = do 
      let alloc' = LLVM.Alloca 
            { allocatedType = type'
            , numElements   = Nothing
            , alignment     = 4
            , metadata      = []
            }
      return $ Name (unpack lval) := alloc'

    {- Store an expression in a variable memory -}
    store lval expr = do
      (value, insts) <- expression expr
      let store = LLVM.Store
            { volatile = False
            , address  = LocalReference type' $ Name (unpack lval) 
            , value    = value
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = []
            }
      -- The store is an unamed instruction, so get the next instrucction label
      label <- nextLabel
      return $ insts <> [label := store]
      