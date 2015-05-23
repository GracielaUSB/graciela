module ASTtype where

import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Text.Parsec.Pos     as P
import qualified Data.Text.Read      as TR
import qualified Control.Monad       as M
import qualified Data.Monoid         as DM
import qualified Data.Text           as T
import MyParseError                  as PE
import ParserState                   as PS
import Data.Monoid
import SymbolTable
import Location
import VerTypes
import Token
import Type 
import AST


verTypeAST :: (AST ()) ->  (AST (Type.Type))
verTypeAST ((AST.Program name loc defs accs _)) = let defs' = verTypeASTlist defs
                                                      defsT = map tag defs'
                                                      accs' = verTypeASTlist accs
                                                      accsT = map tag accs'
                                                  in (AST.Program name loc defs' accs' (verProgram defsT accsT))



verTypeAST ((ID     loc cont _)) = (ID     loc cont (verID cont   ))
verTypeAST ((Int    loc cont _)) = (Int    loc cont (MyInt   ))
verTypeAST ((Float  loc cont _)) = (Float  loc cont (MyFloat ))
verTypeAST ((Bool   loc cont _)) = (Bool   loc cont (MyBool  ))
verTypeAST ((Char   loc cont _)) = (Char   loc cont (MyChar  ))
verTypeAST ((String loc cont _)) = (String loc cont (MyString))



verTypeAST ((Constant loc True  max _)) = (Constant loc True  max (MyInt  ))
verTypeAST ((Constant loc False max _)) = (Constant loc False max (MyFloat))
											


verTypeAST ((Arithmetic t loc lexpr rexp _)) = let l = verTypeAST (lexpr)
                                                   r = verTypeAST (rexp ) 
											   in (Arithmetic t loc l r (verArithmetic $ verType (tag l) (tag r)))



verTypeAST ((Relational t loc lexpr rexp _)) = let l = verTypeAST (lexpr)
                                                   r = verTypeAST (rexp ) 
                                               in (Relational t loc l r (verRelational $ verType (tag l) (tag r)))



verTypeAST ((Boolean    t loc lexpr rexp _)) = let l = verTypeAST (lexpr)
                                                   r = verTypeAST (rexp ) 
                                               in (Boolean    t loc l r (verBoolean    $ verType (tag l) (tag r)))



verTypeAST ((Convertion t loc exp _)) = let exp' = verTypeAST (exp) 
                                        in  (Convertion t loc exp' (verConvertion t))
 


verTypeAST ((Unary op loc exp _)) = let exp' = verTypeAST (exp) 
                                    in  (Unary op loc exp' (verUnary op (tag exp')))
 


verTypeAST ((Block accs loc _)) = let accs' = verTypeASTlist accs
                                      accsT = map tag accs'
                                  in (Block accs' loc (verBlock accsT))



verTypeAST ((Skip loc _))    = (Skip  loc (MyEmpty))
       


verTypeAST ((Abort loc _))   = (Abort loc (MyEmpty))



verTypeAST ((Ran var loc _)) = (Ran var loc (verRandom var))
     


verTypeAST ((Write ln exp loc _)) = let exp' = verTypeAST (exp) 
                                    in (Write ln exp' loc (verWrite (tag exp')))



verTypeAST ((ArrCall loc name args _)) = let args' = verTypeASTlist args
                                             argsT = map tag args'
                                         in (ArrCall loc name args' (verArray argsT name))   



verTypeAST ((Guard exp action loc _)) = let exp'    = verTypeAST (exp   )  
                                            action' = verTypeAST (action)
                                        in (Guard exp' action' loc (verGuard (tag exp') (tag action')))



verTypeAST ((GuardExp exp action loc _)) = let exp'    = verTypeAST (exp   )  
                                               action' = verTypeAST (action)
                                           in (GuardExp exp' action' loc (verGuard (tag exp') (tag action')))



verTypeAST ((States t loc exprs _)) = let exprs' = verTypeASTlist exprs
                                          exprsT = map tag exprs'
                                      in (States t loc exprs' (verState exprsT))    



verTypeAST ((GuardAction loc assert action _)) = let assert' = verTypeAST (assert)  
                                                     action' = verTypeAST (action)
                              in (GuardAction loc assert' action' (verGuardAction (tag assert') (tag action')))



verTypeAST ((LAssign idlist explist loc _)) = let explist' = verTypeASTlist explist  
                                                  explistT = map tag explist'
                                              in (LAssign [] explist' loc (verLAssign explistT idlist))



verTypeAST ((Cond guard loc _)) = let guard' = verTypeASTlist guard  
                                      guardT = map tag guard'
                                  in (Cond guard' loc (verCond guardT))



verTypeAST ((Rept guard inv bound loc _)) = let guard' = verTypeASTlist guard
                                                guardT = map tag guard' 
                                                inv'   = verTypeAST (inv  )
                                                bound' = verTypeAST (bound)
                                          in (Rept guard' inv' bound' loc (verRept guardT (tag inv') (tag bound')))

           

verTypeAST ((ProcCall name args loc _)) =  let args' = verTypeASTlist args  
                                               argsT = map tag args'
                                           in (ProcCall name args' loc (verProcCall argsT name))
  


verTypeAST ((FunBody loc exp _)) = let exp' = verTypeAST (exp)  
                                   in (FunBody loc exp' (tag exp'))



verTypeAST ((FCallExp loc name args _)) =  let args' = verTypeASTlist args 
                                               argsT = map tag args' 
                                           in (FCallExp loc name args' (verCallExp argsT name))
    


verTypeAST ((DefFun name body bound _)) = let body'  = verTypeAST (body )  
                                              bound' = verTypeAST (bound)
                            in (DefFun name body' bound' (verDefFun (tag body') (tag bound') name))



verTypeAST ((DefProc name accs pre post bound _)) = let accs'  = verTypeASTlist accs 
                                                        accsT  = map tag accs' 
                                                        pre'   = verTypeAST (pre  )
                                                        post'  = verTypeAST (post )
                                                        bound' = verTypeAST (bound)
               in (DefProc name accs' pre' post' bound' (verDefProc accsT (tag pre') (tag post') (tag bound')))



verTypeAST ((Quant op var loc range term _)) =  let range' = verTypeAST (range)  
                                                    term'  = verTypeAST (term )
                              in (Quant op var loc range' term' (verQuant op (tag range') (tag term')))



verTypeAST (EmptyAST) = EmptyAST


verTypeASTlist asts = map verTypeAST asts

