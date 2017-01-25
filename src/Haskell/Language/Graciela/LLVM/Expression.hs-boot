module Language.Graciela.LLVM.Expression where

import           Data.Word                        (Word32)
import           Language.Graciela.AST.Expression (Expression)
import           Language.Graciela.LLVM.Monad     (LLVM)
import           Language.Graciela.Location       (SourcePos)
import           LLVM.General.AST.Name            (Name)
import           LLVM.General.AST.Operand         (Operand)

expression :: Expression -> LLVM Operand

safeOperation :: Word32 -> Name -> (Word32 -> String) -> Operand -> Operand -> SourcePos -> LLVM ()
