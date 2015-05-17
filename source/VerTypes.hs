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



 --------------------TERMINARRRR--------------
verDefProc accs pre post bound = (Just MyEmpty)
verGuardAction assert action   = (Just MyEmpty)
verProgram defs accs    = (Just MyEmpty)
verRandom var           = (Just MyEmpty)
verInstructionList accs = (Just MyEmpty)
verGuard exp action     = (Just MyEmpty)
verState exprs          = (Just MyEmpty)
verCond guard           = (Just MyEmpty)
verRept guard inv bound = (Just MyEmpty)
verFunBody exp          = (Just MyEmpty)
verProcCall args        = (Just MyEmpty)
verDefFun body  bound   = (Just MyEmpty)
verQuant op range term  = (Just MyEmpty)


--NECESITO TABLA
verCallExp args name      = (Just MyEmpty)
verArray   args name      = (Just MyEmpty)
verLAssign explist idlist = (Just MyEmpty)
verID name                = (Just MyEmpty)

