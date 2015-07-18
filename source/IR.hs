module IR where

import qualified AST as AST
import LLVM.General.AST.Global
import Control.Monad.State
import Control.Applicative
import LLVM.General.AST 
import Type


irArithmetic AST.Sum MyInt   a b = Add False False a b []
irArithmetic AST.Sum MyFloat a b = FAdd NoFastMathFlags a b []
irArithmetic AST.Sub MyInt   a b = Sub False False a b []
irArithmetic AST.Sub MyFloat a b = FSub NoFastMathFlags a b []
irArithmetic AST.Mul MyInt   a b = Mul False False a b []
irArithmetic AST.Mul MyFloat a b = FMul NoFastMathFlags a b []
irArithmetic AST.Div MyInt   a b = SDiv True a b []
irArithmetic AST.Div MyFloat a b = FDiv NoFastMathFlags a b []
irArithmetic AST.Mod MyInt   a b = URem a b []
irArithmetic AST.Mod MyFloat a b = FRem NoFastMathFlags a b []
