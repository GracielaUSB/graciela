module VerTypes where

import qualified Control.Monad.RWS.Strict as RWSS
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
import Token
import Type
import AST

verType (MyError err) ((MyError err')) = ((MyError (err ++ err')))
verType (MyError err) _  = (MyError err)
verType  _ (MyError err) = (MyError err)
verType (MyEmpty) _      = (MyEmpty      )
verType _ (MyEmpty)      = (MyEmpty      )
verType x y              = if (x == y) then x else ((MyError "March Type Error"))

verArithmetic (MyInt  )     = (MyInt      )
verArithmetic (MyFloat)     = (MyFloat    )
verArithmetic (MyError err) = (MyError err)
verArithmetic (MyEmpty)     = (MyEmpty    )
verArithmetic _				      = ((MyError "Arithmetic Error"))

verRelational (MyError err) = (MyError err)
verRelational (MyEmpty)     = (MyEmpty    )
verRelational _				      = (MyBool     )

verBoolean (MyBool     ) = (MyBool     )
verBoolean (MyError err) = (MyError err)
verBoolean (MyEmpty    ) = (MyEmpty    )
verBoolean _			       = ((MyError "Boolean Error"))

verConvertion ToInt    = (MyInt   )
verConvertion ToDouble = (MyFloat )
verConvertion ToString = (MyString)
verConvertion ToChar   = (MyChar  )

verInstruction (MyError err) = (MyError err)
verInstruction  _            = (MyEmpty)

verUnary Minus   (MyInt  )     = (MyInt  )
verUnary Minus   (MyFloat)     = (MyFloat)
verUnary Minus   (MyError err) = (MyError err)
verUnary Minus   (MyEmpty)     = (MyEmpty)
verUnary Minus   _             = ((MyError "Minus Error"))

verUnary Not     (MyBool)      = (MyBool )
verUnary Not     (MyError err) = (MyError err)
verUnary Not     (MyEmpty)     = (MyEmpty)
verUnary Not     _             = ((MyError "Not Error"))

verUnary Abs     (MyInt)       = (MyInt  )
verUnary Abs     (MyFloat)     = (MyFloat)
verUnary Abs     (MyError err) = (MyError err)
verUnary Abs     (MyEmpty)     = (MyEmpty)
verUnary Abs     _             = ((MyError "Abs Error"))

verUnary Sqrt    (MyInt)       = (MyInt  )
verUnary Sqrt    (MyFloat)     = (MyFloat)
verUnary Sqrt    (MyError err) = (MyError err)
verUnary Sqrt    (MyEmpty)     = (MyEmpty)
verUnary Sqrt    _             = ((MyError "Sqrt Error"))

verUnary Length  (MyArray t n) = (MyInt   )
verUnary Length  (MyString)    = (MyString)
verUnary Length  (MyError err) = (MyError err)
verUnary Length  (MyEmpty)     = (MyEmpty )
verUnary Length   _            = ((MyError "Length Error"))


--Para revisar algun tipo de una lista
--Mejorar poniendo los errores al estado desde aqui
checkListType _ False _ = False
checkListType x True  y = if (x == y) then True else False 


verGuardAction assert action = case ((verAssert assert) && (MyEmpty == action)) of
                                   True  -> (MyEmpty) 
                                   False -> ((MyError "GuardAction Error"))



verGuard exp action = case ((MyBool == exp) && (MyEmpty == action)) of
                          True  -> (MyEmpty) 
                          False -> ((MyError "Guard Error"))



verDefProc accs pre post bound = let func = checkListType (MyEmpty)
                                 in case ((foldl func True accs) && (verPre  pre ) && 
                                          (verBound bound))      && (verPost post) of
                                        True  -> (MyEmpty) 
                                        False -> ((MyError "DefProc Error"))

verBlock accs = let func = checkListType (MyEmpty)
                       in case (foldl func True accs) of
                              True  -> (MyEmpty) 
                              False -> (MyError "Block Error")

verProgram defs accs = let func = checkListType (MyEmpty)
                       in case ((foldl func True defs) && (foldl func True accs)) of
                              True  -> (MyEmpty) 
                              False -> (MyError "Program Error")

verCond guards = let func = checkListType (MyBool)  
                 in case (foldl func True guards) of
                        True  -> (MyEmpty) 
                        False -> (MyError "Guard Error")

verState exprs = let func = checkListType (MyBool)  
                 in case (foldl func True exprs) of
                        True  -> (MyEmpty) 
                        False -> (MyError "State Error")

verRept guard inv bound = let func = checkListType (MyBool)
                          in case ((foldl func True guard) && (verInv inv) && (verBound bound)) of
                              True  -> (MyEmpty) 
                              False -> (MyError "Rept Error")




verQuant op range term  = (MyEmpty)


--NECESITO TABLA
verCallExp  args name     = (MyEmpty)
verProcCall args name     = (MyEmpty)
verArray    args name     = (MyEmpty)
verLAssign explist idlist = (MyEmpty)
verID name                = (MyEmpty)
verDefFun body bound name = (MyEmpty)
verRandom var             = (MyEmpty)

--
verPre    pre    = True
verPost   post   = True
verInv    inv    = True
verBound  bound  = True
verAssert assert = True
