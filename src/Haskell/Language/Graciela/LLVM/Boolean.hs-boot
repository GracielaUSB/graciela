module Language.Graciela.LLVM.Boolean where

import LLVM.General.AST.Name (Name)
import Language.Graciela.LLVM.Monad            (LLVM)
import Language.Graciela.AST.Expression        (Expression)

boolean :: Name -> Name -> Expression -> LLVM ()
