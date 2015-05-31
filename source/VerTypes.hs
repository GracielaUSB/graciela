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


checkListType :: Type -> Bool -> Type -> Bool 
checkListType _ False _ = False
checkListType x True  y = x == y
 

checkError :: Type -> Type -> Type
checkError acc t = let check = verType acc t
                   in case acc of
                      { MyError   -> MyError 
                      ; MyEmpty   -> case check of
                                     { MyError   -> MyError
                                     ; otherwise -> MyEmpty
                                     }  
                      ; otherwise -> case check of
                                     { MyError   -> MyError
                                     ; otherwise -> check
                                     }  
                      }


verType :: Type -> Type -> Type
verType MyError _  = MyError 
verType _  MyError = MyError 
verType x  y       = if (x == y) then x else MyEmpty


verArithmetic :: Type -> Type -> Location -> OpNum -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArithmetic ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyInt     -> return MyInt
                                      ; MyFloat   -> return MyFloat   
                                      ; MyError   -> return MyError
                                      ; otherwise -> do addTypeError $ ArithmeticError ltype rtype op loc 
                                      }                   

verBoolean :: Type -> Type -> Location -> OpBool -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verBoolean ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyBool    -> return MyBool
                                      ; MyError   -> return MyError
                                      ; otherwise -> do addTypeError $ BooleanError ltype rtype op loc 
                                      }                   

verRelational :: Type -> Type -> Location -> OpRel -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRelational ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyError   -> return MyError
                                      ; MyEmpty   -> do addTypeError $ RelationalError ltype rtype op loc 
                                      ; otherwise -> return MyBool
                                      }

verConvertion :: Conv -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verConvertion ToInt    = return MyInt   
verConvertion ToDouble = return MyFloat 
verConvertion ToString = return MyString
verConvertion ToChar   = return MyChar  


verWrite :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verWrite  MyError          = return MyError 
verWrite  _                = return MyEmpty


verUnary :: OpUn -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verUnary _       MyError      _   = return MyError

verUnary Minus   MyInt        loc = return MyInt  
verUnary Minus   MyFloat      loc = return MyFloat
verUnary Minus   errType      loc = do addTypeError $ UnaryError errType Minus loc

verUnary Not     MyBool       loc = return MyBool 
verUnary Not     errType      loc = do addTypeError $ UnaryError errType Not   loc

verUnary Abs     MyInt        loc = return MyInt  
verUnary Abs     MyFloat      loc = return MyFloat
verUnary Abs     errType      loc = do addTypeError $ UnaryError errType Abs   loc

verUnary Sqrt    MyInt        loc = return MyInt  
verUnary Sqrt    MyFloat      loc = return MyFloat
verUnary Sqrt    errType      loc = do addTypeError $ UnaryError errType Sqrt  loc
 
   
verUnary Length  MyString     loc = return MyString
verUnary Length  errType      loc = do addTypeError $ UnaryError errType Length loc


verGuardAction :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verGuardAction assert action = case ((MyBool == assert) && (MyEmpty == action)) of
                               { True  -> return MyEmpty
                               ; False -> return MyError 
                               }


verGuard :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq MyTypeError) () (Type)
verGuard exp action loc = case action of
                          { MyError -> return MyError
                          ; MyEmpty -> case exp of
                                       { MyError   -> return MyError
                                       ; MyBool    -> return MyEmpty
                                       ; otherwise -> do addTypeError $ GuardError exp loc
                                       }
                          }


verGuardExp :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verGuardExp exp action loc = case action of
                             { MyError   -> return MyError
                             ; otherwise -> case exp of
                                            { MyError   -> return MyError
                                            ; MyBool    -> return action
                                            ; otherwise -> do addTypeError $ GuardError exp loc
                                            }                      
                             }


verDefProc :: [Type] -> Type -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verDefProc accs pre post bound = let func = checkListType MyEmpty
                                 in case ((foldl func True accs) && (MyBool == pre ) && (MyBool == post)) of
                                    { True  -> return MyEmpty 
                                    ; False -> return MyError
                                    }


verBlock :: [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verBlock accs = let func = checkListType MyEmpty
                in case (foldl func True accs) of
                   { True  -> return MyEmpty
                   ; False -> return MyError
                   }


verProgram :: [Type] -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verProgram defs accs = let func = checkListType MyEmpty
                       in case ((foldl func True defs) && (foldl func True accs)) of
                          { True  -> return $ MyEmpty
                          ; False -> return $ MyError
                          }


verCond :: [Type] -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verCond guards loc = let checkSame  = (\acc t -> if (acc == t      ) then acc else MyError)  
                         checkT     = foldl1 checkSame guards               
                     in case (foldl checkError MyEmpty guards) of
                        { MyError   -> return MyError
                        ; otherwise -> case checkT of
                                       { MyError   -> do addTypeError $ CondError loc   
                                       ; MyEmpty   -> return MyEmpty
                                       ; otherwise -> return checkT  
                                       }
                        }


verState :: Type -> Location -> StateCond -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verState expr loc stateCond = case expr of
                              { MyError   -> return MyError
                              ; otherwise -> let checkT = case stateCond of
                                                          { Bound     -> MyInt
                                                          ; otherwise -> MyBool 
                                                          }
                                             in case (expr == checkT) of
                                                { True  -> return checkT 
                                                ; False -> do addTypeError $ StateError checkT stateCond loc 
                                                }
                              }


verRept :: [Type] -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRept guard inv bound = let func = checkListType MyEmpty
                          in case ((foldl func True guard) && (MyBool == inv) && (MyInt == bound)) of
                             { True  -> return MyEmpty 
                             ; False -> return MyError
                             }


verQuant :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verQuant range term  = case (isCuantificable range && MyBool == term) of
                       { True  -> return MyBool 
                       ; False -> return MyError
                       }


verCallExp :: T.Text -> [Type] -> Location -> [Location] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verCallExp name args loc locarg = 
    do sb <- RWSS.ask
       case (lookUpRoot name sb) of
       { Nothing -> 
            addUndecFuncError loc name
       ; Just x  -> 
            case (symbolType x) of
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
       case (lookUpRoot name sb) of
       { Nothing -> 
            addUndecFuncError loc name
       ; Just x  -> 
            case (symbolType x) of
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


verLAssign :: [Type] -> [(Token, Type)] -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verLAssign explist idlist loc = 
        let checkError' = (\acc t -> if (not(acc == MyError) && not(t == MyError)) then MyEmpty else MyError) 
            addError    = (\acc ((tok, t), expT) -> case (checkListType t True expT) of 
                                                    { True  -> acc 
                                                    ; False -> acc ++ [AssignError tok t expT loc]
                                                    } )  
            check       = foldl addError [] $ zip idlist explist
        in case (foldl1 checkError' explist) of
           { MyError   -> return MyError
           ; otherwise -> case check of 
                          { []        -> return MyEmpty
                          ; otherwise -> do mapM_ addListError check
                                            return MyError
                          }
            }


verArrayCall :: Token -> [Type] -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verArrayCall name args t loc = 
        case (foldl checkError MyInt args) of
        { MyError   -> return MyError
        ; MyInt     -> return t
        ; otherwise -> let addError = (\acc expT -> case (checkListType MyInt True expT) of 
                                                    { True  -> acc 
                                                    ; False -> acc ++ [ArrayCallError name expT loc]
                                                    } )  
                           check    = foldl addError [] args
                       in do mapM_ addListError check
                             return MyError
        }


verDefFun :: T.Text -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verDefFun name body bound loc = do sb <- RWSS.ask
                                   case lookUpRoot name sb of
                                   { Nothing -> addUndecFuncError loc name
                                   ; Just c  -> case (symbolType c) of
                                                { MyFunction _ tf -> case (tf == body) of
                                                                     { True  -> return MyEmpty
                                                                     ; False -> addRetFuncError loc tf name
                                                                     }
                                                ; otherwise       -> addUndecFuncError loc name
                                                } 
                                   }


addFunArgError :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
addFunArgError t t' loc = addTypeError $ FunArgError t t' loc

addListError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () ()
addListError err = do RWSS.tell $ DS.singleton $ err

addNumberArgsError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addNumberArgsError loc name = addTypeError $ NumberArgsError loc name


addUndecFuncError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addUndecFuncError loc name = addTypeError $ UndecFunError loc name


addRetFuncError :: Location -> Type -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addRetFuncError loc tf name = addTypeError $ RetFuncError name tf loc


addTypeError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError

verRandom :: Token -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verRandom var = return MyEmpty
