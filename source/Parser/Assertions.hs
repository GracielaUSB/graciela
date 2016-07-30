module Parser.Assertions
  ( assertions
  , precondition
  , postcondition
  , bound
  , assertion
  , invariant
  , repInvariant
  , coupInvariant
  )
  where
-------------------------------------------------------------------------------
import           AST
import           Contents
import           Graciela
import           MyParseError        as PE
import           Parser.Declarations
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Parser.State
import           SourcePos
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP
import           Control.Monad       (unless, void)
import           Text.Megaparsec     hiding (Token)
-------------------------------------------------------------------------------

assertions :: Graciela Token -> Graciela Token
           -> StateCond      -> Graciela Token
           -> Graciela AST
assertions initial final stateCond follow = do
    posFrom <- getPosition
    initial
    e <- expression
    final
    posTo <- getPosition
    return $ AST posFrom posTo GEmpty (States stateCond e)

precondition :: Graciela Token -> Graciela AST
precondition follow = assertions (match TokLeftPre) (match TokRightPre) Pre follow

postcondition :: Graciela Token -> Graciela AST
postcondition follow = assertions (match TokLeftPost) (match TokRightPost) Post follow

bound :: Graciela Token -> Graciela AST
bound follow = assertions (match TokLeftBound) (match TokRightBound) Bound follow

assertion :: Graciela Token -> Graciela AST
assertion follow = assertions (match TokLeftA) (match TokRightA) Assertion follow

invariant :: Graciela Token -> Graciela AST
invariant follow = assertions (match TokLeftInv) (match TokRightInv) Invariant follow

repInvariant :: Graciela AST
repInvariant = assertions (match TokLeftRep) (match TokRightRep) Representation
                          (match TokEnd <|> match TokProc <|> match TokLeftAcopl)

coupInvariant :: Graciela AST
coupInvariant = assertions (match TokLeftAcopl) (match TokRightAcopl) Coupling
                          (match TokEnd <|> match TokProc)
