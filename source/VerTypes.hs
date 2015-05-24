module VerTypes where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import MyParseError                       as PE
import MyTypeError                        as PT
import SymbolTable
import Location
import Token
import Type
import AST


--Para revisar algun tipo de una lista
--Mejorar poniendo los errores al estado desde aqui
checkListType _ False _ = False
checkListType x True  y = if (x == y) then True else False 


verType MyError _  = MyError 
verType _  MyError = MyError 
verType x  y       = if (x == y) then x else MyError


verArithmetic :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArithmetic MyInt   = return MyInt 
verArithmetic MyFloat = return MyFloat 
verArithmetic err	    = return MyError 


verRelational :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRelational MyError = return MyError 
verRelational _       = return MyBool


verBoolean :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verBoolean MyBool     = return MyBool     
verBoolean err        = return MyError 


verConvertion :: Conv -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verConvertion ToInt    = return MyInt   
verConvertion ToDouble = return MyFloat 
verConvertion ToString = return MyString
verConvertion ToChar   = return MyChar  


verWrite :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verWrite  MyError          = return MyError 
verWrite (MyArray     _ _) = return MyError
verWrite (MyFunction  _ _) = return MyError
verWrite (MyProcedure _  ) = return MyError
verWrite  _                = return MyEmpty


verUnary :: OpUn -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verUnary Minus   MyInt        = return MyInt  
verUnary Minus   MyFloat      = return MyFloat
verUnary Minus   err          = return MyError 
verUnary Not     MyBool       = return MyBool 
verUnary Not     err          = return MyError 
verUnary Abs     MyInt        = return MyInt  
verUnary Abs     MyFloat      = return MyFloat
verUnary Abs     err          = return MyError
verUnary Sqrt    MyInt        = return MyInt  
verUnary Sqrt    MyFloat      = return MyFloat
verUnary Sqrt    err          = return MyError 
verUnary Length (MyArray t n) = return MyInt   
verUnary Length  MyString     = return MyString
verUnary Length  err          = return MyError


verGuardAction :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verGuardAction assert action = case ((MyBool == assert) && (MyEmpty == action)) of
                                   True  -> return MyEmpty
                                   False -> return MyError 


verGuard :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verGuard exp action = case ((MyBool == exp) && (MyEmpty == action)) of
                          True  -> return MyEmpty 
                          False -> return MyError 


verDefProc :: [Type] -> Type -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verDefProc accs pre post bound = let func = checkListType MyEmpty
                                 in case ((foldl func True accs) && (MyBool == pre )  && 
                                                (MyInt == bound) && (MyBool == post)) of
                                        True  -> return MyEmpty 
                                        False -> return MyError


verBlock :: [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verBlock accs = let func = checkListType MyEmpty
                in case (foldl func True accs) of
                       True  -> return MyEmpty
                       False -> return MyError


verProgram :: [Type] -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verProgram defs accs = let func = checkListType MyEmpty
                       in case ((foldl func True defs) && (foldl func True accs)) of
                              True  -> return $ MyEmpty
                              False -> return $ MyError


verCond :: [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verCond guards = let func = checkListType MyBool
                 in case (foldl func True guards) of
                        True  -> return MyEmpty
                        False -> return MyError


verState :: [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verState exprs = let func = checkListType MyBool 
                 in case (foldl func True exprs) of
                        True  -> return MyEmpty 
                        False -> return MyError


verRept :: [Type] -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRept guard inv bound = let func = checkListType MyBool
                          in case ((foldl func True guard) && (MyBool == inv) && (MyInt == bound)) of
                              True  -> return MyEmpty 
                              False -> return MyError


verQuant :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verQuant range term  = case ((verRango range) && not(MyError == term)) of
                           True  -> return MyBool 
                           False -> return MyError



-----------NECESITO TABLA-----------
verCallExp :: Token -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verCallExp name args = return MyEmpty


verProcCall :: Token -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verProcCall name args = return MyEmpty


verLAssign :: [Type] -> [(Token, [AST ()])] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verLAssign explist idlist = return MyEmpty


verID :: Token -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verID name = return MyEmpty


verDefFun :: Token -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verDefFun name body bound = return MyEmpty


verRandom :: Token -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRandom var = return MyEmpty


verArray :: Token -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArray name args = return MyEmpty


--------------------
verRango range = True
