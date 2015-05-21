module VerTypes where

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



verType (Just (MyError err)) (Just (MyError err')) = (Just (MyError (err ++ err')))
verType (Just (MyError err)) _ = (Just (MyError err))
verType _ (Just (MyError err)) = (Just (MyError err))
verType (Just MyEmpty) _       = (Just MyEmpty      )
verType _ (Just MyEmpty)       = (Just MyEmpty      )
verType x y                    = if (x == y) then x else (Just (MyError "March Type Error"))



verArithmetic (Just MyInt  )       = (Just MyInt        )
verArithmetic (Just MyFloat)       = (Just MyFloat      )
verArithmetic (Just (MyError err)) = (Just (MyError err))
verArithmetic (Just MyEmpty)       = (Just MyEmpty      )
verArithmetic _				       = (Just (MyError "Arithmetic Error"))



verRelational (Just (MyError err)) = (Just (MyError err))
verRelational (Just MyEmpty)       = (Just MyEmpty      )
verRelational _				       = (Just MyBool       )



verBoolean (Just MyBool       ) = (Just MyBool       )
verBoolean (Just (MyError err)) = (Just (MyError err))
verBoolean (Just MyEmpty)       = (Just MyEmpty      )
verBoolean _			        = (Just (MyError "Boolean Error"))



verConvertion ToInt    = (Just MyInt   )
verConvertion ToDouble = (Just MyFloat )
verConvertion ToString = (Just MyString)
verConvertion ToChar   = (Just MyChar  )



verInstruction (Just (MyError err)) = (Just (MyError err))
verInstruction  _                   = (Just  MyEmpty     )



verUnary Minus   (Just  MyInt)        = (Just MyInt  )
verUnary Minus   (Just  MyFloat)      = (Just MyFloat)
verUnary Minus   (Just (MyError err)) = (Just (MyError err))
verUnary Minus   (Just  MyEmpty)      = (Just MyEmpty)
verUnary Minus   _                    = (Just (MyError "Minus Error"))

verUnary Not     (Just  MyBool)       = (Just MyBool)
verUnary Not     (Just (MyError err)) = (Just (MyError err))
verUnary Not     (Just  MyEmpty)      = (Just MyEmpty)
verUnary Not     _                    = (Just (MyError "Not Error"))

verUnary Abs     (Just  MyInt)        = (Just MyInt  )
verUnary Abs     (Just  MyFloat)      = (Just MyFloat)
verUnary Abs     (Just (MyError err)) = (Just (MyError err))
verUnary Abs     (Just  MyEmpty)      = (Just MyEmpty)
verUnary Abs     _                    = (Just (MyError "Abs Error"))

verUnary Sqrt    (Just  MyInt)        = (Just MyInt  )
verUnary Sqrt    (Just  MyFloat)      = (Just MyFloat)
verUnary Sqrt    (Just (MyError err)) = (Just (MyError err))
verUnary Sqrt    (Just  MyEmpty)      = (Just MyEmpty)
verUnary Sqrt    _                    = (Just (MyError "Sqrt Error"))

verUnary Length  (Just (MyArray t n)) = (Just MyInt   )
verUnary Length  (Just  MyString)     = (Just MyString)
verUnary Length  (Just (MyError err)) = (Just (MyError err))
verUnary Length  (Just  MyEmpty)      = (Just MyEmpty )
verUnary Length   _                   = (Just (MyError "Length Error"))


--Para revisar algun tipo de una lista
--Mejorar poniendo los errores al estado desde aqui
cheackListType _ False _ = False
cheackListType x True  y = if (x == y) then True else False 



verGuardAction assert action = case ((verAssert assert) && (Just MyEmpty == action)) of
                                   True  -> (Just MyEmpty) 
                                   False -> (Just (MyError "GuardAction Error"))



verGuard exp action = case ((Just MyBool == exp) && (Just MyEmpty == action)) of
                          True  -> (Just MyEmpty) 
                          False -> (Just (MyError "Guard Error"))



verDefProc accs pre post bound = let func = cheackListType (Just MyEmpty)
                                 in case ((foldl func True accs) && (verPre  pre ) && 
                                          (verBound bound))      && (verPost post) of
                                        True  -> (Just MyEmpty) 
                                        False -> (Just (MyError "DefProc Error"))



verBlock accs = let func = cheackListType (Just MyEmpty)
                       in case (foldl func True accs) of
                              True  -> (Just MyEmpty) 
                              False -> (Just (MyError "Block Error"))



verProgram defs accs = let func = cheackListType (Just MyEmpty)
                       in case ((foldl func True defs) && (foldl func True accs)) of
                              True  -> (Just MyEmpty) 
                              False -> (Just (MyError "Program Error"))



verCond guards = let func = cheackListType (Just MyBool)  
                 in case (foldl func True guards) of
                        True  -> (Just MyEmpty) 
                        False -> (Just (MyError "Guard Error"))



verState exprs = let func = cheackListType (Just MyBool)  
                 in case (foldl func True exprs) of
                        True  -> (Just MyEmpty) 
                        False -> (Just (MyError "State Error"))



verRept guard inv bound = let func = cheackListType (Just MyBool)
                          in case ((foldl func True guard) && (verInv inv) && (verBound bound)) of
                              True  -> (Just MyEmpty) 
                              False -> (Just (MyError "Rept Error"))




verQuant op range term  = (Just MyEmpty)


--NECESITO TABLA
verCallExp  args name     = (Just MyEmpty)
verProcCall args name     = (Just MyEmpty)
verArray    args name     = (Just MyEmpty)
verLAssign explist idlist = (Just MyEmpty)
verID name                = (Just MyEmpty)
verDefFun body bound name = (Just MyEmpty)
verRandom var             = (Just MyEmpty)

--
verPre    pre    = True
verPost   post   = True
verInv    inv    = True
verBound  bound  = True
verAssert assert = True