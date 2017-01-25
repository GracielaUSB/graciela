{-# LANGUAGE NamedFieldPuns #-}
module Language.Graciela.Parser.Assertion
  ( assertion
  , bound
  , precond
  , postcond
  , invariant
  , repInv
  , coupInv
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Expression
import           Language.Graciela.AST.Type
import           Language.Graciela.Error              as PE
import           Language.Graciela.Location
import           Language.Graciela.Parser.Declaration
import           Language.Graciela.Parser.Expression
import           Language.Graciela.Parser.Monad
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Type
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens       ((%=))
import           Control.Monad      (unless, void)
import           Data.Sequence      ((|>))
import           Text.Megaparsec    (ParseError, between, lookAhead, manyTill,
                                     withRecovery)
--------------------------------------------------------------------------------

bound :: Parser (Maybe Expression)
bound = between (match TokLeftBound) (match' TokRightBound) (declarative bound')
  where
    bound' = do
      expr <- withRecovery recover expression
      case expr of
        Nothing -> pure Nothing
        Just Expression { loc = Location (from, _) , expType }
          | expType =:= GInt -> pure expr
          | otherwise -> do
            putError from $ BadBoundType expType
            pure Nothing

    recover :: ParseError TokenPos Error -> Parser (Maybe a)
    recover err = do
      errors %= (|> err)
      void . manyTill anyToken . lookAhead . match $ TokRightBound
      pure Nothing


assert :: Token -> Token -> Parser (Maybe Expression)
assert open close = between (match' open) (match' close) (declarative $ assert'' close)

assert' :: Token -> Token -> Parser (Maybe Expression)
assert' open close = between (match open) (match' close) (declarative $ assert'' close)
                           -- ^^^^^ this match is not obligatory
assert'' close = do
  expr <- withRecovery recover expression
  case expr of
    Nothing -> pure Nothing
    Just Expression { loc = Location (from, _), expType }
      | expType =:= GBool -> pure expr
      | otherwise -> do
        putError from $ BadAssertType expType
        pure Nothing
  where 
    recover :: ParseError TokenPos Error -> Parser (Maybe a)
    recover err = do
      errors %= (|> err)
      void . manyTill anyToken . lookAhead . match $ close
      pure Nothing

precond, postcond, assertion, invariant, repInv :: Parser (Maybe Expression)
precond   = assert  TokLeftPre   TokRightPre
postcond  = assert  TokLeftPost  TokRightPost
assertion = assert' TokLeftBrace TokRightBrace
invariant = assert  TokLeftInv   TokRightInv
repInv    = assert  TokLeftRep   TokRightRep
coupInv   = assert  TokLeftAcopl TokRightAcopl
