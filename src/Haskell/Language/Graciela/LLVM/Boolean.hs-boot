module Language.Graciela.LLVM.Boolean where

import           Language.Graciela.AST.Expression (Expression)
import           Language.Graciela.LLVM.Monad     (LLVM)
import           LLVM.General.AST.Name            (Name)

boolean :: Name -> Name -> Expression -> LLVM ()
