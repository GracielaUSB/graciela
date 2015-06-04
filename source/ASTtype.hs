module ASTtype where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import MyParseError                       as PE
import MyTypeError                        as PT
import SymbolTable
import VerTypes
import Type 
import AST
import Token
import qualified Data.Text                as T
import qualified Control.Applicative as AP
import TypeState

runTVerifier stable stree = RWSS.evalRWS (verTypeAST stree) stable () 

verTypeAST :: (AST (Type)) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (AST (Type.Type))
verTypeAST ((AST.Program name loc defs accs _)) = do defs'    <- verTypeASTlist defs
                                                     accs'    <- verTypeASTlist accs
                                                     let defsT = map tag defs'
                                                     let accsT = map tag accs'
                                                     checkT   <- verProgram defsT accsT
                                                     return (AST.Program name loc defs' accs' checkT)




verTypeAST ((Constant loc True  max _)) = return (Constant loc True  max MyInt  )
verTypeAST ((Constant loc False max _)) = return (Constant loc False max MyFloat)


verTypeAST ((Arithmetic t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verArithmetic (tag lexpr') (tag rexp') loc t 
                                                  return (Arithmetic t loc lexpr' rexp' checkT)


verTypeAST ((Relational t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verRelational (tag lexpr') (tag rexp') loc t
                                                  return (Relational t loc lexpr' rexp' checkT)
                                                         

verTypeAST ((Boolean    t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verBoolean (tag lexpr') (tag rexp') loc t
                                                  return (Boolean t loc lexpr' rexp' checkT)
                                                         

verTypeAST ((Convertion t loc exp _)) = do exp'   <- verTypeAST (exp) 
                                           checkT <- verConvertion t
                                           return (Convertion t loc exp' checkT)


verTypeAST ((Unary op loc exp _)) = do exp'   <- verTypeAST (exp) 
                                       checkT <- verUnary op (tag exp') loc
                                       return (Unary op loc exp' checkT)


verTypeAST ((Block accs loc _)) = do accs'  <- verTypeASTlist accs
                                     checkT <- verBlock (map tag accs')
                                     return (Block accs' loc checkT)


verTypeAST ((Skip    loc _)) = return (Skip  loc (MyEmpty))
verTypeAST ((Abort   loc _)) = return (Abort loc (MyEmpty))

verTypeAST ((Ran var loc _)) = do checkT <- verRandom var
                                  return (Ran var loc checkT)


verTypeAST ((Write ln exp loc _)) = do exp'   <- verTypeAST (exp) 
                                       checkT <- verWrite (tag exp')
                                       return (Write ln exp' loc checkT)


verTypeAST ((ArrCall loc name args t)) = do args'  <- verTypeASTlist args
                                            checkT <- verArrayCall name (map tag args') t loc 
                                            return (ArrCall loc name args' checkT)    


verTypeAST ((Guard exp action loc _)) = do exp'    <- verTypeAST exp     
                                           action' <- verTypeAST action
                                           checkT  <- verGuard (tag exp') (tag action') loc
                                           return (Guard exp' action' loc checkT)
                                                  

verTypeAST ((GuardExp exp action loc _)) = do exp'    <- verTypeAST exp     
                                              action' <- verTypeAST action
                                              checkT  <- verGuardExp (tag exp') (tag action') loc  
                                              return (GuardExp exp' action' loc checkT)
                                                


verTypeAST ((States t loc expr _)) = do expr' <- verTypeAST expr
                                        checkT <- verState (tag expr') loc t 
                                        return (States t loc expr' checkT)    


verTypeAST ((GuardAction loc assert action _)) = do assert' <- verTypeAST assert  
                                                    action' <- verTypeAST action
                                                    checkT  <- verGuardAction (tag assert') (tag action')
                                                    return (GuardAction loc assert' action' checkT)
                                                           


verTypeAST ((LAssign idlist explist loc _)) = do explist' <- verTypeASTlist explist  
                                                 checkT   <- verLAssign (map tag explist') (map fst idlist) loc
                                                 return (LAssign [] explist' loc checkT)


verTypeAST ((Cond guard loc _)) = do guard' <- verTypeASTlist guard  
                                     checkT <- verCond (map tag guard') loc
                                     return (Cond guard' loc checkT)


verTypeAST ((Rept guard inv bound loc _)) = do guard' <- verTypeASTlist guard
                                               inv'   <- verTypeAST inv  
                                               bound' <- verTypeAST bound
                                               checkT <- verRept (map tag guard') (tag inv') (tag bound')
                                               return (Rept guard' inv' bound' loc checkT)
                                                      

verTypeAST ((ProcCall name args loc _)) = do args'  <- verTypeASTlist args  
                                             locs  <- getLocArgs args
                                             checkT <- verProcCall (text name) (map tag args') loc locs
                                             return (ProcCall name args' loc checkT)

  
verTypeAST ((FunBody loc exp _)) = do exp' <- verTypeAST (exp)
                                      return (FunBody loc exp' (tag exp'))


verTypeAST ((FCallExp loc name args _)) = do args' <- verTypeASTlist args
                                             locs  <- getLocArgs args
                                             checkT <- verCallExp (text name) (map tag args') loc locs
                                             return (FCallExp loc name args' checkT)

verTypeAST ((DefFun name loc body bound _)) = do body'  <- verTypeAST body   
                                                 bound' <- verTypeAST bound
                                                 checkT <- verDefFun (text name) (tag body') (tag bound') loc
                                                 return (DefFun name loc body' bound' checkT)
                                                    

verTypeAST ((DefProc name accs pre post bound _)) = do accs'  <- verTypeASTlist accs 
                                                       pre'   <- verTypeAST pre  
                                                       post'  <- verTypeAST post 
                                                       bound' <- verTypeAST bound
                                                       checkT <- verDefProc (map tag accs') (tag pre'  )
                                                                            (tag     post') (tag bound')
                                                       return (DefProc name accs' pre' post' bound' checkT)
                                                              

verTypeAST ((Quant op var loc range term _)) = do range' <- verTypeAST range  
                                                  term'  <- verTypeAST term 
                                                  checkT <- verQuant (tag range') (tag term')
                                                  case checkT of
                                                    MyError   -> return (Quant op var loc range' term' checkT)
                                                    otherwise -> let id = text var in
                                                                 do r <- occursCheck range id
                                                                    case r of 
                                                                      True  -> return $ Quant op var loc range' term' checkT
                                                                      False -> do addNotOccursVarError id loc
                                                                                  return $ Quant op var loc range' term' MyError
                                                         
verTypeAST ast = return $ ast

occursCheck :: AST a -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq MyTypeError) () (Bool)
occursCheck (Arithmetic _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Relational _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Boolean    _ _ l r _) id = AP.liftA2 (&&) (occursCheck l id) (occursCheck r id)
occursCheck (ID _ t _            ) id = return $ id == (text t)
occursCheck (EmptyRange _ _      ) _  = return $ True
occursCheck _ _                       = return $ False

-- verTypeAST ((ID     loc cont t)) = return (ID     loc cont t       )
-- verTypeAST ((Int    loc cont _)) = return (Int    loc cont MyInt   )
-- verTypeAST ((Float  loc cont _)) = return (Float  loc cont MyFloat )
-- verTypeAST ((Bool   loc cont _)) = return (Bool   loc cont MyBool  )
-- verTypeAST ((Char   loc cont _)) = return (Char   loc cont MyChar  )
-- verTypeAST ((String loc cont _)) = return (String loc cont MyString)
-- verTypeAST (EmptyAST)            = return EmptyAST

getLocArgs args = return $ fmap AST.location args

verTypeASTlist []     = return []
verTypeASTlist (x:xs) = do r  <- verTypeAST x
                           rs <- verTypeASTlist xs
                           return (r:rs)
