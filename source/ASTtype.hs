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



verTypeAST (Just (AST.Program name loc defs accs _)) = let defs' = verTypeASTlist defs
                                                           accs' = verTypeASTlist accs
                                                       in (AST.Program name loc defs' accs' (verProgram defs' accs'))



verTypeAST (Just (ID     loc cont _)) = (ID     loc cont (verID cont   ))
verTypeAST (Just (Int    loc cont _)) = (Int    loc cont (Just MyInt   ))
verTypeAST (Just (Float  loc cont _)) = (Float  loc cont (Just MyFloat ))
verTypeAST (Just (Bool   loc cont _)) = (Bool   loc cont (Just MyBool  ))
verTypeAST (Just (Char   loc cont _)) = (Char   loc cont (Just MyChar  ))
verTypeAST (Just (String loc cont _)) = (String loc cont (Just MyString))



verTypeAST (Just (Constant loc True  max _)) = (Constant loc True  max (Just MyInt  ))
verTypeAST (Just (Constant loc False max _)) = (Constant loc False max (Just MyFloat))
											


verTypeAST (Just (Arithmetic t loc lexpr rexp _)) = let l = verTypeAST (Just lexpr)
                                                        r = verTypeAST (Just rexp ) 
											        in (Arithmetic t loc l r (verArithmetic $ verType (tag l) (tag r)))



verTypeAST (Just (Relational t loc lexpr rexp _)) = let l = verTypeAST (Just lexpr)
                                                        r = verTypeAST (Just rexp ) 
                                                    in (Relational t loc l r (verRelational $ verType (tag l) (tag r)))



verTypeAST (Just (Boolean    t loc lexpr rexp _)) = let l = verTypeAST (Just lexpr)
                                                        r = verTypeAST (Just rexp ) 
                                                    in (Boolean    t loc l r (verBoolean    $ verType (tag l) (tag r)))



verTypeAST (Just (Convertion t loc exp _)) = let exp' = verTypeAST (Just exp) 
                                             in  (Convertion t loc exp' (verConvertion t))
 


verTypeAST (Just (Unary op loc exp _)) = let exp' = verTypeAST (Just exp) 
                                         in  (Unary op loc exp' (verUnary op (tag exp')))
 


verTypeAST (Just (Block action loc _)) = let action' = verTypeASTlist action
                                         in (Block action' loc (verInstructionList action'))



verTypeAST (Just (Skip loc _))    = (Skip  loc (Just MyEmpty))
       


verTypeAST (Just (Abort loc _))   = (Abort loc (Just MyEmpty))



verTypeAST (Just (Ran var loc _)) = (Ran var loc (verRandom var))
     


verTypeAST (Just (Write ln exp loc _)) = let exp' = verTypeAST (Just exp) 
                                         in (Write ln exp' loc (verInstruction (tag exp')))



verTypeAST (Just (ArrCall loc name args _)) = let args' = verTypeASTlist args
                                              in (ArrCall loc name args' (verArray args' name))   



verTypeAST (Just (Guard exp action loc _)) = let exp'    = verTypeAST (Just exp   )  
                                                 action' = verTypeAST (Just action)
                                             in (Guard exp' action' loc (verGuard (tag exp') (tag action')))



verTypeAST (Just (GuardExp exp action loc _)) = let exp'    = verTypeAST (Just exp   )  
                                                    action' = verTypeAST (Just action)
                                                    in (GuardExp exp' action' loc (verGuard (tag exp') (tag action')))



verTypeAST (Just (States t loc exprs _)) = let exprs' = verTypeASTlist exprs
                                           in (States t loc exprs' (verState exprs'))    



verTypeAST (Just (GuardAction loc assert action _)) = let assert' = verTypeAST (Just assert)  
                                                          action' = verTypeAST (Just action)
                               in (GuardAction loc assert' action' (verGuardAction (tag assert') (tag action')))



verTypeAST (Just (LAssign idlist explist loc _)) = let explist' = verTypeASTlist explist  
                                                   in (LAssign idlist explist' loc (verLAssign explist' idlist))



verTypeAST (Just (Cond guard loc _)) = let guard' = verTypeASTlist guard  
                                       in (Cond guard' loc (verCond guard'))



verTypeAST (Just (Rept guard inv bound loc _)) = let guard' = verTypeASTlist guard 
                                                     inv'   = verTypeAST (Just inv  )
                                                     bound' = verTypeAST (Just bound)
                                          in (Rept guard inv bound loc (verRept guard' (tag inv') (tag bound')))

           

verTypeAST (Just (ProcCall name args loc _)) =  let args' = verTypeASTlist args  
                                                in (ProcCall name args' loc (verProcCall args'))
  


verTypeAST (Just (FunBody loc exp _)) = let exp' = verTypeAST (Just exp)  
                                        in (FunBody loc exp' (verFunBody (tag exp')))



verTypeAST (Just (FCallExp loc name args _)) =  let args' = verTypeASTlist args  
                                             in (FCallExp loc name args' (verCallExp args' name))
    


verTypeAST (Just (DefFun name body bound _)) = let body'  = verTypeAST (Just body )  
                                                   bound' = verTypeAST (Just bound)
                            in (DefFun name body' bound' (verDefFun (tag body') (tag bound')))



verTypeAST (Just (DefProc name accs pre post bound _)) = let accs'  = verTypeASTlist accs 
                                                             pre'   = verTypeAST (Just pre  )
                                                             post'  = verTypeAST (Just post )
                                                             bound' = verTypeAST (Just bound)
               in (DefProc name accs pre' post' bound' (verDefProc accs' (tag pre') (tag post') (tag bound')))



verTypeAST (Just (Quant op var loc range term _)) =  let range' = verTypeAST (Just range)  
                                                         term'  = verTypeAST (Just term )
                              in (Quant op var loc range' term' (verQuant op (tag range') (tag term')))



verTypeAST (Just EmptyAST) = EmptyAST


verTypeASTlist asts = map verTypeAST (map Just asts) 

