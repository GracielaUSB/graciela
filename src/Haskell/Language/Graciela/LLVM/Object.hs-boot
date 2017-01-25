module Language.Graciela.LLVM.Object where

import Language.Graciela.AST.Object (Object)
import Language.Graciela.LLVM.Monad (LLVM)
import LLVM.General.AST.Operand (Operand)

object :: Object -> LLVM Operand
objectRef :: Object -> LLVM Operand
