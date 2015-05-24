module VerTypes where

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
import Token
import Type
import AST

verType MyError _  = MyError 
verType _  MyError = MyError 
verType x  y       = if (x == y) then x else MyError



verArithmetic MyInt   = MyInt 
verArithmetic MyFloat = MyFloat 
verArithmetic err	    = MyError 


verRelational MyError = MyError 
verRelational _       = MyBool


verBoolean MyBool     = MyBool     
verBoolean err        = MyError 



verConvertion ToInt    = MyInt   
verConvertion ToDouble = MyFloat 
verConvertion ToString = MyString
verConvertion ToChar   = MyChar  



verWrite  MyError          = MyError 
verWrite (MyArray     _ _) = MyError
verWrite (MyFunction  _ _) = MyError
verWrite (MyProcedure _  ) = MyError
verWrite  _                = MyEmpty



verUnary Minus   MyInt       = MyInt  
verUnary Minus   MyFloat     = MyFloat
verUnary Minus   err         = MyError 


verUnary Not     MyBool      = MyBool 
verUnary Not     err         = MyError 


verUnary Abs     MyInt        = MyInt  
verUnary Abs     MyFloat      = MyFloat
verUnary Abs     err          = MyError


verUnary Sqrt    MyInt        = MyInt  
verUnary Sqrt    MyFloat      = MyFloat
verUnary Sqrt    err          = MyError 


verUnary Length (MyArray t n) = MyInt   
verUnary Length  MyString     = MyString
verUnary Length  err          = MyError


--Para revisar algun tipo de una lista
--Mejorar poniendo los errores al estado desde aqui
checkListType _ False _ = False
checkListType x True  y = if (x == y) then True else False 


verGuardAction assert action = case ((MyBool == assert) && (MyEmpty == action)) of
                                   True  -> MyEmpty
                                   False -> MyError 



verGuard exp action = case ((MyBool == exp) && (MyEmpty == action)) of
                          True  -> MyEmpty 
                          False -> MyError 



verDefProc accs pre post bound = let func = checkListType MyEmpty
                                 in case ((foldl func True accs) && (MyBool == pre )  && 
                                                (MyInt == bound) && (MyBool == post)) of
                                        True  -> MyEmpty 
                                        False -> MyError



verBlock accs = let func = checkListType MyEmpty
                       in case (foldl func True accs) of
                              True  -> MyEmpty
                              False -> MyError

verProgram :: [Type] -> [Type] -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
verProgram defs accs = let func = checkListType MyEmpty
                       in case ((foldl func True defs) && (foldl func True accs)) of
                              True  -> return $ MyEmpty
                              False -> return $ MyError



verCond guards = let func = checkListType MyBool
                 in case (foldl func True guards) of
                        True  -> MyEmpty
                        False -> MyError



verState exprs = let func = checkListType MyBool 
                 in case (foldl func True exprs) of
                        True  -> MyEmpty 
                        False -> MyError

verRept guard inv bound = let func = checkListType MyBool
                          in case ((foldl func True guard) && (MyBool == inv) && (MyInt == bound)) of
                              True  -> MyEmpty 
                              False -> MyError



verQuant op range term  = let func = checkListType MyBool
                          in case ((verRango range) && not(MyError == term)) of
                              True  -> MyBool 
                              False -> MyError


--NECESITO TABLA
verCallExp  args name     = MyEmpty
verProcCall args name     = MyEmpty
verArray    args name     = MyEmpty
verLAssign explist idlist = MyEmpty
verID name                = MyEmpty
verDefFun body bound name = MyEmpty
verRandom var             = MyEmpty


verRango range            = True
