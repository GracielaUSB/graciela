module LLVM.Boolean where

import LLVM.General.AST.Name (Name)
import LLVM.Monad            (LLVM)
import AST.Expression        (Expression)

boolean :: Name -> Name -> Expression -> LLVM ()
