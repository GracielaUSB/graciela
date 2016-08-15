module LLVM.Instruction

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression
import           AST.Declaration                        (Declaration)

import           Limits
import           LLVM.State
import           LLVM.Expression
import           LLVM.Type                               
import           SymbolTable
import qualified Type                                    as T
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad                           (zipWithM)
import           Data.Foldable                           (toList)
import qualified Data.Map                                as DM
import           Data.Maybe
import           Data.Range.Range                        as RA
import qualified Data.Text                               as TE
import           Data.Word
import           LLVM.General.AST.Name                  (Name(..))
import           LLVM.General.AST.Instruction           (Instruction(..), Named(..),
                                                         Terminator(..), FastMathFlags(..))
import           LLVM.General.AST.Operand               (Operand(..), CallableOperand)
--------------------------------------------------------------------------------

 -- graciela 2.0
-- alloc :: Declaration -> LLVM ()
-- alloc Declaration {declType, declLvals, declExpr } = if null declType 
--   then mapM allocWithoutAssign declLvals 
--   else zipWithM allocWithAssign declLvals declExpr
--   where

--     type' = toLLVMType declType

--     allocWithoutAssign lval = do 
--       let alloc = alloca Nothing type' lval
--       instrs %= (|> Name lval := alloc)

--     allocWithAssign lval expr = do 
--       let alloc = Alloc Nothing type' lval
--       instrs %= (|> Name lval := alloc)
--       let local = LocalReference (unpack lval) declType
      












