module Parser.Assertion
  ( assertion
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
import           AST.Expression

import           Graciela
import           MyParseError        as PE
import           Parser.Declaration
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Parser.State
import           Location
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP
import           Control.Monad       (unless, void)
import           Text.Megaparsec     hiding (Token)
-------------------------------------------------------------------------------

assert ::  Graciela Expression
assert  = do 
  expr <- expression 
  case expr of 
    Expression _ exprType _ -> return expr
    Expression loc _ _ -> do 
      genCustomError ("Las asserciones solo pueden tener expresiones booleanas")
      return $ BadExpression loc

precondition :: Graciela Expression
precondition = between (match TokLeftPre) (match TokRightPre) assert

postcondition :: Graciela Expression
postcondition = between (match TokLeftPost) (match TokRightPost)  assert

bound :: Graciela Expression
bound = between (match TokLeftBound) (match TokRightBound) assert

assertion :: Graciela Expression
assertion = between (match TokLeftA) (match TokRightA) assert

invariant :: Graciela Expression
invariant = between (match TokLeftInv) (match TokRightInv) assert

repInvariant :: Graciela Expression
repInvariant = between (match TokLeftRep) (match TokRightRep) assert

coupInvariant :: Graciela Expression
coupInvariant = between (match TokLeftAcopl) (match TokRightAcopl) assert
