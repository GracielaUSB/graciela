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
import           Text.Megaparsec
-------------------------------------------------------------------------------

assertions :: Graciela Token -> Graciela Token
           -> StateCond      -> Graciela Token
           -> Graciela (Maybe (AST Type) )
assertions initial final ty follow = do
    initial
    e <- expression
    final
    return $ AP.liftA2 (States ty pos) e (return GEmpty)

precondition :: Graciela Token -> Graciela (Maybe (AST Type) )
precondition follow = assertions (match TokLeftPre) (match TokRightPre) Pre follow

postcondition :: Graciela Token -> Graciela (Maybe (AST Type) )
postcondition follow = assertions (match TokLeftPost) (match TokRightPost) Post follow

bound :: Graciela Token -> Graciela (Maybe (AST Type) )
bound follow = assertions (match TokLeftBound) (match TokRightBound) Bound follow

assertion :: Graciela Token -> Graciela (Maybe (AST Type) )
assertion follow = assertions (match TokLeftA) (match TokRightA) Assertion follow

invariant :: Graciela Token -> Graciela (Maybe (AST Type) )
invariant follow = assertions (match TokLeftInv) (match TokRightInv) Invariant follow

repInvariant :: Graciela (Maybe (AST Type))
repInvariant = assertions (match TokLeftRep) (match TokRightRep) Representation
                          (parseEnd <|> parseProc <|> match TokLeftAcopl)

coupInvariant :: Graciela (Maybe (AST Type) )
coupInvariant = assertions (match TokLeftAcopl) (match TokRightAcopl) Couple
                          (parseEnd <|> parseProc)
