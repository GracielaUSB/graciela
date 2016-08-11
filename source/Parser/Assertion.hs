{-# LANGUAGE NamedFieldPuns #-}
module Parser.Assertion
  ( assertion
  , bound
  , coupInvariant
  , invariant
  , postcondition
  , precondition
  , repInvariant
  ) where
-------------------------------------------------------------------------------
import           AST.Expression
import           AST.Type
import           Graciela
import           Location
import           Error       as PE
import           Parser.Declaration
import           Parser.Expression
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           Token
-------------------------------------------------------------------------------
import           Control.Monad       (unless, void)
import           Text.Megaparsec     (between)
-------------------------------------------------------------------------------


bound :: Graciela Expression
bound = between (match TokLeftBound) (match TokRightBound) bound'
  where
    bound' = do
      expr <- safeExpression
      case expr of
        Expression { expType } | expType =:= GInt -> return expr
        Expression { loc, expType } -> do
          putError loc $ BadBoundType expType
          return $ BadExpression loc
        badexpr@(BadExpression _) ->
          return badexpr

assert ::  Graciela Expression
assert  = do
  expr <- safeExpression
  case expr of
    Expression { expType } | expType =:= GBool -> return expr
    Expression { loc, expType } -> do
      putError loc $ BadAssertType expType
      return $ BadExpression loc
    badexpr@(BadExpression _) ->
      return badexpr


precondition :: Graciela Expression
precondition = between (match TokLeftPre) (withRecovery TokRightPre ) assert

postcondition :: Graciela Expression
postcondition = between (match TokLeftPost) (withRecovery TokRightPost ) assert

assertion :: Graciela Expression
assertion = between (match TokLeftA) (withRecovery TokRightA ) assert

invariant :: Graciela Expression
invariant = between (match TokLeftInv) (withRecovery TokRightInv ) assert

repInvariant :: Graciela Expression
repInvariant = between (match TokLeftRep) (withRecovery TokRightRep ) assert

coupInvariant :: Graciela Expression
coupInvariant = between (match TokLeftAcopl) (withRecovery TokRightAcopl ) assert
