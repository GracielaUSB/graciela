module LLVM.Expression where

import AST.Expression (Expression)
import LLVM.Monad (LLVM)
import LLVM.General.AST.Operand (Operand)
import Data.Word (Word32)
import Location (SourcePos)
import LLVM.General.AST.Name (Name)

expression :: Expression -> LLVM Operand

safeOperation :: Word32 -> Name -> (Word32 -> String) -> Operand -> Operand -> SourcePos -> LLVM ()
