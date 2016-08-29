
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Recovery
  ( safeAssertion
  , safeExpression
  , safeIdentifier
  -- , Parser.Recovery.withRecovery
  , withRecoveryFollowedBy
  , prettyError
  ) where
--------------------------------------------------------------------------------
import           AST.Expression        (Expression (..))
import           Error
import           Location
import           Parser.Expression     (expression)
import           Parser.Monad          (Parser, anyToken, identifier, match,
                                        putError, unsafeGenCustomError)
import           Parser.State
import           Token
--------------------------------------------------------------------------------
import           Control.Lens          ((%=))
import           Data.List             (intercalate)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import           Data.Sequence         ((|>))
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (ErrorItem (Tokens), ParseError (..),
                                        ShowErrorComponent, ShowToken,
                                        getPosition, lookAhead, manyTill,
                                        parseErrorPretty, showErrorComponent,
                                        try, (<|>))
import           Text.Megaparsec       as MP (withRecovery)
import           Text.Megaparsec.Error (sourcePosStackPretty)
import qualified Text.Megaparsec.Prim  as Prim (Token)
--------------------------------------------------------------------------------

{- Try the assertion p. if fail then report Error e and return bad expression-}
safeAssertion :: Parser (Maybe Expression) -> Error -> Parser (Maybe Expression)
safeAssertion p e = try p <|> recover
  where
    recover = do
      pos <- getPosition
      putError pos e
      return Nothing

{- Parse an expression. If fail then report an error and return bad expression -}
safeExpression :: Parser (Maybe Expression)
safeExpression = MP.withRecovery recover expression
  where
    recover err = do
      -- let from :| [_] = errorPos err
      -- to <- getPosition
      errors %= (|> err)
      -- let loc = Location (from, to)
      return Nothing

{- Parse an identifier. If fail then report an error and return `Nothing` -}
safeIdentifier :: Parser (Maybe Text)
safeIdentifier = MP.withRecovery recover (Just <$> identifier)
  where
    recover err = do
      unsafeGenCustomError (parseErrorPretty err)
      return Nothing

{- Try parse the token. If fail then report an error -}
-- withRecovery :: Token -> Parser (Maybe Location)
-- withRecovery token = MP.withRecovery (recover [token]) (Just <$> match token)
--   where
--     recover expected err = do
--       let from :| _ = errorPos err
--       to <- getPosition
--       -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
--       let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos from to t)
--       let err' = err { errorExpected = f expected }
--       -- Print (put it in the error list)
--       errors %= (|> err')
--       return Nothing

{- Try parse the token. If fail then report an error and start a panic mode until follow is found -}
withRecoveryFollowedBy :: Token -> Parser (Maybe Token) -> Parser (Maybe Location)
withRecoveryFollowedBy token follow = MP.withRecovery (recover [token] follow) (Just <$> match token)
  where
    recover expected follow err = do
        let from :| [_] = errorPos err
        to <- getPosition
        -- if any follow token is especified, then trash many token until a follow is at the look ahead
        anyToken `manyTill` lookAhead follow
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos from to t)
        let err' = err { errorExpected = f expected }
        -- Print (put it in the error list)
        errors %= (|> err')
        return Nothing
