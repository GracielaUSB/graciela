module IR where

import qualified LLVM.General.AST.FloatingPointPredicate as FL 
import qualified AST                                     as MyAST
import qualified LLVM.General.AST.Attribute as AT
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Global
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.AST 
import Type


irArithmetic :: MyAST.OpNum -> Type.Type -> Operand -> Operand -> Instruction
irArithmetic MyAST.Sum MyInt   a b = Add False False a b []
irArithmetic MyAST.Sum MyFloat a b = FAdd NoFastMathFlags a b []
irArithmetic MyAST.Sub MyInt   a b = Sub False False a b []
irArithmetic MyAST.Sub MyFloat a b = FSub NoFastMathFlags a b []
irArithmetic MyAST.Mul MyInt   a b = Mul False False a b []
irArithmetic MyAST.Mul MyFloat a b = FMul NoFastMathFlags a b []
irArithmetic MyAST.Div MyInt   a b = SDiv True a b []
irArithmetic MyAST.Div MyFloat a b = FDiv NoFastMathFlags a b []
irArithmetic MyAST.Mod MyInt   a b = URem a b []
irArithmetic MyAST.Mod MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic MyAST.Exp MyInt   a b = URem a b []
--irArithmetic MyAST.Exp MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic MyAST.Min MyInt   a b = URem a b []
--irArithmetic MyAST.Min MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic MyAST.Max MyInt   a b = URem a b []
--irArithmetic MyAST.Max MyFloat a b = FRem NoFastMathFlags a b []


irBoolean :: MyAST.OpBool -> Operand -> Operand -> Instruction
irBoolean MyAST.Con a b = And a b []
irBoolean MyAST.Dis a b = Or  a b []
--irBoolean MyAST.Implies a b = And a b []
--irBoolean MyAST.Conse a b = Or  a b []


irRelational :: MyAST.OpRel -> Operand -> Operand -> Instruction
irRelational MyAST.Equ     a b = FCmp FL.OEQ a b []
irRelational MyAST.Less    a b = FCmp FL.OLT a b []
irRelational MyAST.Greater a b = FCmp FL.OGT a b []
irRelational MyAST.LEqual  a b = FCmp FL.OLE a b []
irRelational MyAST.GEqual  a b = FCmp FL.OGE a b []
irRelational MyAST.Ine     a b = FCmp FL.OEQ a b []
irRelational MyAST.Equal   a b = FCmp FL.ONE a b []


irConvertion :: MyAST.Conv -> Type.Type -> Operand -> Instruction
irConvertion MyAST.ToInt    MyFloat a = FPToSI a i32    [] 
irConvertion MyAST.ToInt    MyChar  a = FPToSI a i32    [] 
irConvertion MyAST.ToDouble MyInt   a = SIToFP a double [] 
irConvertion MyAST.ToDouble MyChar  a = SIToFP a double [] 
irConvertion MyAST.ToChar   MyInt   a = Trunc  a i8     [] 
irConvertion MyAST.ToChar   MyFloat a = FPToSI a i8     [] 


irUnary :: MyAST.OpUn -> Type.Type -> Operand -> Instruction
irUnary MyAST.Minus MyInt   a = FPToSI a VoidType [] 
--irUnary MyAST.Minus MyFloat a = FPToSI a VoidType [] 
--irUnary MyAST.Not   MyFloat a = FPToSI a VoidType [] 
--irUnary MyAST.Abs   MyFloat a = FPToSI a VoidType [] 
--irUnary MyAST.Sqrt  MyFloat a = FPToSI a VoidType [] 


toArgs :: [Operand] -> [(Operand, [AT.ParameterAttribute])]
toArgs = map (\x -> (x, []))


--irCallExp :: Operand -> [Operand] -> Instruction
irCallExp ::  CallableOperand -> [(Operand, [AT.ParameterAttribute])] -> Instruction
irCallExp fn args = Call False C [] fn args [] []
