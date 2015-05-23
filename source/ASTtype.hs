module ASTtype where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence as DS
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Text.Parsec.Pos     as P
import qualified Data.Text.Read      as TR
import qualified Control.Monad       as M
import qualified Data.Monoid         as DM
import qualified Data.Text           as T
import MyParseError                  as PE
import MyTypeError                   as PT
import ParserState                   as PS
import Data.Monoid
import SymbolTable
import Location
import VerTypes
import Token
import Type 
import AST


verTypeAST :: (AST ()) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (AST (Type.Type))
verTypeAST ((AST.Program name loc sc defs accs _)) = do defs' <- verTypeASTlist defs
                                                        accs' <- verTypeASTlist accs
                                                        let defsT = map tag defs'
                                                        let accsT = map tag accs'
                                                        return $ (AST.Program name loc sc defs' accs' (verProgram defsT accsT))


verTypeAST ((ID     loc cont _)) = return (ID     loc cont (verID cont))
verTypeAST ((Int    loc cont _)) = return (Int    loc cont (MyInt     ))
verTypeAST ((Float  loc cont _)) = return (Float  loc cont (MyFloat   ))
verTypeAST ((Bool   loc cont _)) = return (Bool   loc cont (MyBool    ))
verTypeAST ((Char   loc cont _)) = return (Char   loc cont (MyChar    ))
verTypeAST ((String loc cont _)) = return (String loc cont (MyString  ))


verTypeAST ((Constant loc True  max _)) = return (Constant loc True  max (MyInt  ))
verTypeAST ((Constant loc False max _)) = return (Constant loc False max (MyFloat))


verTypeAST ((Arithmetic t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  return (Arithmetic t loc lexpr' rexp' 
                                                         (verArithmetic $ verType (tag lexpr') (tag rexp')))


verTypeAST ((Relational t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  return (Relational t loc lexpr' rexp' 
                                                         (verRelational $ verType (tag lexpr') (tag rexp')))


verTypeAST ((Boolean    t loc lexpr rexp _)) = do lexpr' <- verTypeAST lexpr
                                                  rexp'  <- verTypeAST rexp  
                                                  return (Boolean t loc lexpr' rexp' 
                                                         (verBoolean $ verType (tag lexpr') (tag rexp')))


verTypeAST ((Convertion t loc exp _)) = do exp' <- verTypeAST (exp) 
                                           return (Convertion t loc exp' (verConvertion t))


verTypeAST ((Unary op loc exp _)) = do exp' <- verTypeAST (exp) 
                                       return (Unary op loc exp' (verUnary op (tag exp')))


verTypeAST ((Block accs loc _)) = do accs' <- verTypeASTlist accs
                                     let accsT = map tag accs'
                                     return (Block accs' loc (verBlock accsT))


verTypeAST ((Skip loc _))    = return (Skip  loc (MyEmpty))
verTypeAST ((Abort loc _))   = return (Abort loc (MyEmpty))
verTypeAST ((Ran var loc _)) = return (Ran var loc (verRandom var))


verTypeAST ((Write ln exp loc _)) = do exp' <- verTypeAST (exp) 
                                       return (Write ln exp' loc (verWrite (tag exp')))


verTypeAST ((ArrCall loc name args _)) = do args' <- verTypeASTlist args
                                            let argsT = map tag args'
                                            return (ArrCall loc name args' (verArray argsT name))   


verTypeAST ((Guard exp action loc _)) = do exp'    <- verTypeAST exp     
                                           action' <- verTypeAST action
                                           return (Guard exp' action' loc 
                                                  (verGuard (tag exp') (tag action')))


verTypeAST ((GuardExp exp action loc _)) = do exp'    <- verTypeAST exp     
                                              action' <- verTypeAST action
                                              return (GuardExp exp' action' loc 
                                                     (verGuard (tag exp') (tag action')))


verTypeAST ((States t loc exprs _)) = do exprs' <- verTypeASTlist exprs
                                         let exprsT = map tag exprs'
                                         return (States t loc exprs' (verState exprsT))    


verTypeAST ((GuardAction loc assert action _)) = do assert' <- verTypeAST assert  
                                                    action' <- verTypeAST action
                                                    return (GuardAction loc assert' action'
                                                           (verGuardAction (tag assert') (tag action')))


verTypeAST ((LAssign idlist explist loc _)) = do explist' <- verTypeASTlist explist  
                                                 let explistT = map tag explist'
                                                 return (LAssign [] explist' loc (verLAssign explistT idlist))


verTypeAST ((Cond guard loc _)) = do guard' <- verTypeASTlist guard  
                                     let guardT = map tag guard'
                                     return (Cond guard' loc (verCond guardT))


verTypeAST ((Rept guard inv bound loc _)) = do guard' <- verTypeASTlist guard
                                               inv'   <- verTypeAST inv  
                                               bound' <- verTypeAST bound
                                               let guardT = map tag guard' 
                                               return (Rept guard' inv' bound' loc
                                                      (verRept guardT (tag inv') (tag bound')))

verTypeAST ((ProcCall name args loc _)) = do args' <- verTypeASTlist args  
                                             let argsT = map tag args'
                                             return (ProcCall name args' loc (verProcCall argsT name))

  
verTypeAST ((FunBody loc exp _)) = do exp' <- verTypeAST (exp)  
                                      return (FunBody loc exp' (tag exp'))


verTypeAST ((FCallExp loc name args _)) = do args' <- verTypeASTlist args 
                                             let argsT = map tag args' 
                                             return (FCallExp loc name args' (verCallExp argsT name))


verTypeAST ((DefFun name body bound _)) = do body'  <- verTypeAST body   
                                             bound' <- verTypeAST bound
                                             return (DefFun name body' bound'
                                                    (verDefFun (tag body') (tag bound') name))


verTypeAST ((DefProc name accs pre post bound _)) = do accs'  <- verTypeASTlist accs 
                                                       pre'   <- verTypeAST pre  
                                                       post'  <- verTypeAST post 
                                                       bound' <- verTypeAST bound
                                                       let accsT = map tag accs' 
                                                       return (DefProc name accs' pre' post' bound'
                                                              (verDefProc accsT (tag pre') (tag post') (tag bound')))


verTypeAST ((Quant op var loc range term _)) = do range' <- verTypeAST range  
                                                  term'  <- verTypeAST term 
                                                  return (Quant op var loc range' term'
                                                         (verQuant op (tag range') (tag term')))


verTypeAST (EmptyAST) = return EmptyAST


verTypeASTlist []     = return []
verTypeASTlist (x:xs) = do r  <- verTypeAST x
                           rs <- verTypeASTlist xs
                           return (r:rs)
