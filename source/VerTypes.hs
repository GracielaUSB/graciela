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
import TypeState
import Data.Maybe

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


verArithmetic :: Type -> Type -> Location -> OpNum -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verArithmetic ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyInt     -> return MyInt
                                      ; MyFloat   -> return MyFloat   
                                      ; MyError   -> return MyError
                                      ; otherwise -> addTypeError $ ArithmeticError ltype rtype op loc 
                                      }                   

verBoolean :: Type -> Type -> Location -> OpBool -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verBoolean ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyBool    -> return MyBool
                                      ; MyError   -> return MyError
                                      ; otherwise -> addTypeError $ BooleanError ltype rtype op loc 
                                      }                   

verRelational :: Type -> Type -> Location -> OpRel -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verRelational ltype rtype loc op = let checkT = verType ltype rtype
                                   in case checkT of
                                      { MyError   -> return MyError
                                      ; MyEmpty   -> addTypeError $ RelationalError ltype rtype op loc 
                                      ; otherwise -> return MyBool
                                      }

verConvertion :: Conv -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verConvertion ToInt    = return MyInt   
verConvertion ToDouble = return MyFloat 
verConvertion ToString = return MyString
verConvertion ToChar   = return MyChar  


verWrite :: Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verWrite  MyError          = return MyError 
verWrite  _                = return MyEmpty


verUnary :: OpUn -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verUnary _       MyError      _   = return MyError

verUnary Minus   MyInt        loc = return MyInt  
verUnary Minus   MyFloat      loc = return MyFloat
verUnary Minus   errType      loc = addTypeError $ UnaryError errType Minus loc

verUnary Not     MyBool       loc = return MyBool 
verUnary Not     errType      loc = addTypeError $ UnaryError errType Not   loc

verUnary Abs     MyInt        loc = return MyInt  
verUnary Abs     MyFloat      loc = return MyFloat
verUnary Abs     errType      loc = addTypeError $ UnaryError errType Abs   loc

verUnary Sqrt    MyInt        loc = return MyInt  
verUnary Sqrt    MyFloat      loc = return MyFloat
verUnary Sqrt    errType      loc = addTypeError $ UnaryError errType Sqrt  loc
 
   
verUnary Length  MyString     loc = return MyString
verUnary Length  errType      loc = addTypeError $ UnaryError errType Length loc


verGuardAction :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verGuardAction assert action = case ((assert == MyBool) && (action == MyEmpty)) of
                               { True  -> return MyEmpty
                               ; False -> return MyError 
                               }


verGuard :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq MyTypeError) () Type
verGuard exp action loc = case action of
                          { MyError -> return MyError
                          ; MyEmpty -> case exp of
                                       { MyError   -> return MyError
                                       ; MyBool    -> return MyEmpty
                                       ; otherwise -> addTypeError $ GuardError exp loc
                                       }
                          }


verGuardExp :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verGuardExp exp action loc = case action of
                             { MyError   -> return MyError
                             ; otherwise -> case exp of
                                            { MyError   -> return MyError
                                            ; MyBool    -> return action
                                            ; otherwise -> addTypeError $ GuardError exp loc
                                            }                      
                             }


verDefProc :: [Type] -> Type -> Type -> Type -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verDefProc accs pre post bound decs = 
    let func = checkListType MyEmpty
    in case ((foldl func True accs) && (pre == MyBool) && (post == MyBool) && (and $ map (== MyEmpty) decs)) of
       { True  -> return MyEmpty 
       ; False -> return MyError
       }


verBlock :: [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verBlock accs = let func = checkListType MyEmpty
                in case (foldl func True accs) of
                   { True  -> return MyEmpty
                   ; False -> return MyError
                   }


verProgram :: [Type] -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verProgram defs accs = let func = checkListType MyEmpty
                       in case ((foldl func True defs) && (foldl func True accs)) of
                          { True  -> return $ MyEmpty
                          ; False -> return $ MyError
                          }


verCond :: [Type] -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verCond guards loc = let checkSame  = (\acc t -> if (acc == t) then acc else MyError)  
                         checkT     = foldl1 checkSame guards               
                     in case (foldl checkError MyEmpty guards) of
                        { MyError   -> return MyError
                        ; otherwise -> case checkT of
                                       { MyError   -> addTypeError $ CondError loc   
                                       ; MyEmpty   -> return MyEmpty
                                       ; otherwise -> return checkT  
                                       }
                        }


verState :: Type -> Location -> StateCond -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verState expr loc stateCond = case expr of
                              { MyError   -> return MyError
                              ; otherwise -> let checkT = case stateCond of
                                                          { Bound     -> MyInt
                                                          ; otherwise -> MyBool 
                                                          }
                                             in case (expr == checkT) of
                                                { True  -> return expr 
                                                ; False -> addTypeError $ StateError expr stateCond loc 
                                                }
                              }


verRept :: [Type] -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verRept guard inv bound = let func = checkListType MyEmpty
                          in case ((foldl func True guard) && (inv == MyBool) && (bound == MyInt)) of
                             { True  -> return MyEmpty 
                             ; False -> return MyError
                             }


verRandom :: Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verRandom t loc = case ((t == MyInt) || (t == MyFloat)) of
                { True  -> return t 
                ; False -> addTypeError $ RanError t loc
                }


verQuant :: Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verQuant range term  = case ((isCuantificable range) && (term == MyBool)) of
                       { True  -> return MyBool 
                       ; False -> return MyError
                       }

verConsAssign :: [(T.Text, Location)] -> Location -> [Type] -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verConsAssign xs loc ts t =
    if length xs /= length ts then
        addDifSizeDecError loc
    else
        do r <- fmap and $ fmap (map (== MyEmpty)) $ mapM f (zip xs ts)
           if r then return MyEmpty
           else return MyError
  where
    f (((id, loc'), t')) = 
      if t' /= t then
        addTypeDecError id loc' t' t
      else
        return $ MyEmpty

verCallExp :: T.Text -> [Type] -> Location -> [Location] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verCallExp name args loc locarg = 
    do sb <- RWSS.ask
       case (lookUpRoot name sb) of
       { Nothing -> 
            addUndecFuncError name loc
       ; Just x  -> 
            case (symbolType x) of
            { MyFunction args' ts ->
                  let wtL = length args
                      prL = length args'
                  in if (wtL /= prL) then addNumberArgsError name wtL prL loc
                  else let t = zip args args' in
                          if   and $ map (uncurry (==)) $ t then return $ ts
                          else do mapM_ (\ ((arg, arg'), larg) -> 
                                              if arg /= arg' then addFunArgError arg' arg larg 
                                              else return MyEmpty
                                        ) (zip t locarg) 
                                  return $ MyError
            ; otherwise           -> 
                  addUndecFuncError name loc
            }
       }


verProcCall :: T.Text -> SymbolTable -> [(T.Text, Type)] -> Location -> [Location] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verProcCall name sbc args'' loc locarg = 
    do sb <- RWSS.ask
       case (lookUpRoot name sb) of
       { Nothing -> 
            addUndecFuncError name loc
       ; Just (ProcCon _ t ln sb)  -> 
            case t of
            { MyProcedure args' ->
                  let wtL = length args''
                      prL = length args'
                  in if (wtL /= prL) then addNumberArgsError name wtL prL loc
                  else let args = map snd args''
                           t    = zip args args' in
                          if   and $ map (uncurry (==)) $ t then do r <- validProcArgs ln (map fst args'') locarg sb sbc
                                                                    if r  then return MyEmpty
                                                                    else return $ MyError
                          else do mapM_ (\ ((arg, arg'), larg) -> 
                                              if arg /= arg' then addFunArgError arg' arg larg 
                                              else return MyEmpty
                                        ) (zip t locarg) 
                                  return $ MyError
            ; otherwise           -> 
                  addUndecFuncError name loc
            }
       }

validProcArgs :: [T.Text] -> [T.Text] -> [Location] -> SymbolTable -> SymbolTable -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Bool
validProcArgs lnp lnc locarg sbp sbc = 
    let lat = map getProcArgType $  map fromJust $ map ((flip checkSymbol) sbp) lnp
        lvt = map getVarBeh $       map fromJust $ map ((flip checkSymbol) sbc) lnc
        xs  = zip lat lvt
    in
        fmap and $ mapM compare (zip xs (zip lnc locarg))

    where
      compare ((Just Out, Just Contents.Constant), (id, loc)) = 
          do addInvalidPar id loc
             return False
      compare _                                               =
             return True

addLAssignError:: Location -> [MyTypeError] -> (((T.Text, Type), [Type]), Type) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () [MyTypeError]
addLAssignError loc acc (((tok, (MyArray t tam)), expArrT), expT) = 
        do arrT <- verArrayCall tok expArrT (MyArray t tam) loc 
           case arrT of
           { MyError   -> return acc 
           ; otherwise -> case (checkListType arrT True expT) of 
                          { True  -> return acc 
                          ; False -> return $ acc ++ [AssignError tok arrT expT loc]
                          }  
           }
addLAssignError loc acc (((tok, t), _), expT) = 
        case (checkListType t True expT) of 
        { True  -> return acc 
        ; False -> return $ acc ++ [AssignError tok t expT loc]
        }  


verLAssign :: [Type] -> [(T.Text, Type)] -> [[Type]] -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verLAssign explist idlist expArrT loc = 
        do check <- RWSS.foldM (addLAssignError loc) [] $ zip (zip idlist expArrT) explist
           let checkError' = (\acc t -> if (not(acc == MyError) && not(t == MyError)) then MyEmpty else MyError)   
           case (foldl1 checkError' explist) of
           { MyError   -> return MyError
           ; otherwise -> case check of 
                          { []        -> return MyEmpty
                          ; otherwise -> do mapM_ addListError check
                                            return MyError
                          }
            }


verArrayCall :: T.Text -> [Type] -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verArrayCall name args t loc =
        let waDim = getDimention t 0
            prDim = length args
        in case (waDim == prDim) of
           { False -> addTypeError $ ArrayDimError name waDim prDim loc   
           ; True  -> case (foldl checkError MyInt args) of
                      { MyError   -> return MyError
                      ; MyInt     -> return $ getType t
                      ; otherwise -> let addError = (\acc expT -> case (checkListType MyInt True expT) of 
                                                                  { True  -> acc 
                                                                  ; False -> acc ++ [ArrayCallError name expT loc]
                                                                  } )  
                                         check    = foldl addError [] args
                                     in do mapM_ addListError check
                                           return MyError
                      }
          }


verDefFun :: T.Text -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
verDefFun name body bound loc = do sb <- RWSS.ask
                                   case lookUpRoot name sb of
                                   { Nothing -> addUndecFuncError name loc
                                   ; Just c  -> case (symbolType c) of
                                                { MyFunction _ tf -> case (tf == body) of
                                                                     { True  -> return MyEmpty
                                                                     ; False -> addRetFuncError name tf body loc
                                                                     }
                                                ; otherwise       -> addUndecFuncError name loc
                                                } 
                                   }
