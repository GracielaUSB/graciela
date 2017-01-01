module LLVM.Object where

import AST.Object (Object)
import LLVM.Monad (LLVM)
import LLVM.General.AST.Operand (Operand)

object :: Object -> LLVM Operand
objectRef :: Object -> LLVM Operand
