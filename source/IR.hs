module IR where

import qualified LLVM.General.AST.FloatingPointPredicate as FL 
import qualified AST                                     as AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.AST 
import Type


irArithmetic :: AST.OpNum -> Type.Type -> Operand -> Operand -> Instruction
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
--irArithmetic AST.Exp MyInt   a b = URem a b []
--irArithmetic AST.Exp MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic AST.Min MyInt   a b = URem a b []
--irArithmetic AST.Min MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic AST.Max MyInt   a b = URem a b []
--irArithmetic AST.Max MyFloat a b = FRem NoFastMathFlags a b []


irBoolean :: AST.OpBool -> Operand -> Operand -> Instruction
irBoolean AST.Con a b = And a b []
irBoolean AST.Dis a b = Or  a b []
--irBoolean AST.Implies a b = And a b []
--irBoolean AST.Conse a b = Or  a b []


irRelational :: AST.OpRel -> Operand -> Operand -> Instruction
irRelational AST.Equ     a b = FCmp FL.OEQ a b []
irRelational AST.Less    a b = FCmp FL.OLT a b []
irRelational AST.Greater a b = FCmp FL.OGT a b []
irRelational AST.LEqual  a b = FCmp FL.OLE a b []
irRelational AST.GEqual  a b = FCmp FL.OGE a b []
irRelational AST.Ine     a b = FCmp FL.OEQ a b []
irRelational AST.Equal   a b = FCmp FL.ONE a b []


irConvertion :: AST.Conv -> Type.Type -> Operand -> Instruction
irConvertion AST.ToInt    MyFloat a = FPToSI a i32    [] 
irConvertion AST.ToInt    MyChar  a = FPToSI a i32    [] 
irConvertion AST.ToDouble MyInt   a = SIToFP a double [] 
irConvertion AST.ToDouble MyChar  a = SIToFP a double [] 
irConvertion AST.ToChar   MyInt   a = Trunc  a i8     [] 
irConvertion AST.ToChar   MyFloat a = FPToSI a i8     [] 


irUnary :: AST.OpUn -> Type.Type -> Operand -> Instruction
irUnary AST.Minus MyInt   a = FPToSI a VoidType [] 
--irUnary AST.Minus MyFloat a = FPToSI a VoidType [] 
--irUnary AST.Not   MyFloat a = FPToSI a VoidType [] 
--irUnary AST.Abs   MyFloat a = FPToSI a VoidType [] 
--irUnary AST.Sqrt  MyFloat a = FPToSI a VoidType [] 
