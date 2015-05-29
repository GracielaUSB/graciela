module VerTypes where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import MyParseError                       as PE
import MyTypeError                        as PT
import qualified Data.Text                as T
import Contents
import SymbolTable
import Location
import Token
import Type
import AST

--Para revisar algun tipo de una lista
--Mejorar poniendo los errores al estado desde aqui
checkListType _ False _ = False
checkListType x True  y = x == y


verType MyError _  = MyError 
verType _  MyError = MyError 
verType x  y       = if (x == y) then x else MyError


verArithmetic :: Type -> Location -> OpNum -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArithmetic MyInt   _ _    = return MyInt 
verArithmetic MyFloat _ _    = return MyFloat 
verArithmetic err     loc op = do RWSS.tell $ DS.singleton $ ArithmeticError op loc
                                  return MyError

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
verCallExp :: T.Text -> [Type] -> Location -> [Location] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verCallExp name args loc locarg = 
    do sb <- RWSS.ask
       case lookUpRoot name sb of
       { Nothing -> 
            addUndecFuncError loc name
       ; Just x  -> 
            case symbolType x of
            { MyFunction args' ts ->
                  if length args /= length args' then addNumberArgsError loc name
                  else let t = zip args args' in
                          if   and $ map (uncurry (==)) $ t then return $ ts
                          else do mapM_ (\ ((arg, arg'), larg) -> 
                                              if arg /= arg' then addFunArgError arg' arg larg 
                                              else return MyEmpty
                                        ) (zip t locarg) 
                                  return $ MyError
            ; otherwise           -> 
                  addUndecFuncError loc name
            }
       }


verProcCall :: T.Text -> [Type] -> Location -> [Location] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verProcCall name args loc locarg = 
    do sb <- RWSS.ask
       case lookUpRoot name sb of
       { Nothing -> 
            addUndecFuncError loc name
       ; Just x  -> 
            case symbolType x of
            { MyProcedure args' ->
                  if length args /= length args' then addNumberArgsError loc name
                  else let t = zip args args' in
                          if   and $ map (uncurry (==)) $ t then return $ MyEmpty
                          else do mapM_ (\ ((arg, arg'), larg) -> 
                                              if arg /= arg' then addFunArgError arg' arg larg 
                                              else return MyEmpty
                                        ) (zip t locarg) 
                                  return $ MyError
            ; otherwise           -> 
                  addUndecFuncError loc name
            }
       }

verLAssign :: [Type] -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verLAssign explist idlist = 
        let check = foldl (\acc (t, expT) -> checkListType t acc expT) True $ zip idlist explist
                              in case check of 
                                     True  -> return MyEmpty
                                     False -> return MyError

verDefFun :: T.Text -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verDefFun name body bound loc = do sb <- RWSS.ask
                                   case lookUpRoot name sb of
                                   { Nothing -> addUndecFuncError loc name
                                   ; Just c  -> case symbolType c of
                                                { MyFunction _ tf -> case tf == body of
                                                                     { True  -> return tf
                                                                     ; False -> addRetFuncError loc tf name
                                                                     }
                                                ; otherwise       -> addUndecFuncError loc name
                                                } 
                                   }

addFunArgError :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
addFunArgError t t' loc = addTypeError $ FunArgError t t' loc

addNumberArgsError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addNumberArgsError loc name = addTypeError $ NumberArgsError loc name

addUndecFuncError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addUndecFuncError loc name = addTypeError $ UndecFunError loc name

addRetFuncError :: Location -> Type -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addRetFuncError loc tf name = addTypeError $ RetFuncError name tf loc

addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError

verRandom :: Token -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRandom var = return MyEmpty


verArray :: Token -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArray name args = return MyEmpty


--------------------
verRango range = True
