module ASTtype where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import MyParseError                       as PE
import MyTypeError                        as PT
import SymbolTable
import VerTypes
import Type 
import AST

runTVerifier stable stree = RWSS.evalRWS (verTypeAST stree) stable () 

verTypeAST :: (AST (Type)) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (AST (Type.Type))
verTypeAST ((AST.Program name loc defs accs _)) = do defs'    <- verTypeASTlist defs
                                                     accs'    <- verTypeASTlist accs
                                                     let defsT = map tag defs'
                                                     let accsT = map tag accs'
                                                     checkT   <- verProgram defsT accsT
                                                     return (AST.Program name loc defs' accs' checkT)


verTypeAST ((ID loc cont t)) = return $ ID loc cont t

verTypeAST ((Int    loc cont _)) = return (Int    loc cont MyInt   )
verTypeAST ((Float  loc cont _)) = return (Float  loc cont MyFloat )
verTypeAST ((Bool   loc cont _)) = return (Bool   loc cont MyBool  )
verTypeAST ((Char   loc cont _)) = return (Char   loc cont MyChar  )
verTypeAST ((String loc cont _)) = return (String loc cont MyString)


verTypeAST ((Constant loc True  max _)) = return (Constant loc True  max MyInt  )
verTypeAST ((Constant loc False max _)) = return (Constant loc False max MyFloat)


verTypeAST ((Arithmetic t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verArithmetic (verType (tag lexpr') (tag rexp')) loc t 
                                                  return (Arithmetic t loc lexpr' rexp' checkT)


verTypeAST ((Relational t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verRelational $ verType (tag lexpr') (tag rexp')
                                                  return (Relational t loc lexpr' rexp' checkT)
                                                         

verTypeAST ((Boolean    t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  checkT <- verBoolean $ verType (tag lexpr') (tag rexp')
                                                  return (Boolean t loc lexpr' rexp' checkT)
                                                         

verTypeAST ((Convertion t loc exp _)) = do exp'   <- verTypeAST (exp) 
                                           checkT <- verConvertion t
                                           return (Convertion t loc exp' checkT)


verTypeAST ((Unary op loc exp _)) = do exp'   <- verTypeAST (exp) 
                                       checkT <- verUnary op (tag exp')
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


verTypeAST ((ArrCall loc name args _)) = do args'  <- verTypeASTlist args
                                            checkT <- verArray name (map tag args') 
                                            return (ArrCall loc name args' checkT)    


verTypeAST ((Guard exp action loc _)) = do exp'    <- verTypeAST exp     
                                           action' <- verTypeAST action
                                           checkT  <- verGuard (tag exp') (tag action') 
                                           return (Guard exp' action' loc checkT)
                                                  

verTypeAST ((GuardExp exp action loc _)) = do exp'    <- verTypeAST exp     
                                              action' <- verTypeAST action
                                              checkT  <- verGuard (tag exp') (tag action')
                                              return (GuardExp exp' action' loc checkT)
                                                


verTypeAST ((States t loc exprs _)) = do exprs' <- verTypeASTlist exprs
                                         checkT <- verState (map tag exprs')
                                         return (States t loc exprs' checkT)    


verTypeAST ((GuardAction loc assert action _)) = do assert' <- verTypeAST assert  
                                                    action' <- verTypeAST action
                                                    checkT  <- verGuardAction (tag assert') (tag action')
                                                    return (GuardAction loc assert' action' checkT)
                                                           


verTypeAST ((LAssign idlist explist loc _)) = do explist' <- verTypeASTlist explist  
                                                 checkT   <- verLAssign (map tag explist') idlist
                                                 return (LAssign [] explist' loc checkT)


verTypeAST ((Cond guard loc _)) = do guard' <- verTypeASTlist guard  
                                     checkT <- verCond (map tag guard')
                                     return (Cond guard' loc checkT)


verTypeAST ((Rept guard inv bound loc _)) = do guard' <- verTypeASTlist guard
                                               inv'   <- verTypeAST inv  
                                               bound' <- verTypeAST bound
                                               checkT <- verRept (map tag guard') (tag inv') (tag bound')
                                               return (Rept guard' inv' bound' loc checkT)
                                                      

verTypeAST ((ProcCall name args loc _)) = do args'  <- verTypeASTlist args  
                                             checkT <- verProcCall name (map tag args')  
                                             return (ProcCall name args' loc checkT)

  
verTypeAST ((FunBody loc exp _)) = do exp' <- verTypeAST (exp)
                                      return (FunBody loc exp' (tag exp'))


verTypeAST ((FCallExp loc name args _)) = do args' <- verTypeASTlist args 
                                             checkT <- verCallExp name (map tag args') 
                                             return (FCallExp loc name args' checkT)


verTypeAST ((DefFun name body bound _)) = do body'  <- verTypeAST body   
                                             bound' <- verTypeAST bound
                                             checkT <- verDefFun name (tag body') (tag bound') 
                                             return (DefFun name body' bound' checkT)
                                                    

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
                                                  return (Quant op var loc range' term' checkT)
                                                         


verTypeAST (EmptyAST) = return EmptyAST


verTypeASTlist []     = return []
verTypeASTlist (x:xs) = do r  <- verTypeAST x
                           rs <- verTypeASTlist xs
                           return (r:rs)
