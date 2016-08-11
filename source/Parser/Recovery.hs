
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Recovery
  ( errorId
  , safeAssertion
  , safeExpression
  , safeIdentifier
  , Parser.Recovery.withRecovery
  , withRecoveryFollowedBy
  ) where
--------------------------------------------------------------------------------
import           AST.Expression         (Expression(..))
import           Parser.Expression      (expression)
import           Parser.Token           (identifier, match, anyToken)
import           Token
import           Location
import           Error
import           Graciela
--------------------------------------------------------------------------------
import           Control.Lens          ((%=)) 
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NE
import           Data.Set              (Set)
import           Data.Sequence         ((|>))
import qualified Data.Set              as Set
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (ErrorItem (Tokens), ParseError(..), 
                                        manyTill, lookAhead, (<|>), ShowToken,
                                        parseErrorPretty, getPosition, try,
                                        ShowErrorComponent,showErrorComponent)
import           Text.Megaparsec        as MP (withRecovery)
import qualified Text.Megaparsec.Prim  as Prim (Token)

errorId :: Text
errorId = pack "0#Error"

safeAssertion :: Graciela Expression -> Error -> Graciela Expression
safeAssertion p e = try p <|> recover
  where 
    recover = do 
      pos <- getPosition 
      let location = Location(pos,pos)
      putError location e
      return $ BadExpression location

safeExpression :: Graciela Expression 
safeExpression = MP.withRecovery recover expression
  where
    recover err = do
      let from :| [_] = errorPos err
      to <- getPosition
      errors %= (|> err)
      let loc = Location (from, to) 
      return $ BadExpression loc

safeIdentifier :: Graciela Text
safeIdentifier = MP.withRecovery recover identifier
  where 
    recover err = do
      genCustomError (parseErrorPretty err)
      return errorId

withRecovery :: Token -> Graciela Location 
withRecovery token = MP.withRecovery (recover [token]) (match token)   
  where 
    recover expected err = do 
        let from :| [_] = errorPos err
        to <- getPosition
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos from to t)
        let err' = err { errorExpected = f expected}
        -- Print (put it in the error list)
        errors %= (|> err')
        return $ Location (from, to) 

withRecoveryFollowedBy :: Token -> Graciela Token -> Graciela Location 
withRecoveryFollowedBy token follow = MP.withRecovery (recover [token] follow) (match token)   
  where 
    recover expected follow err = do 
        let from :| [_] = errorPos err
        to <- getPosition
        -- if any follow token is especified, then trash many token until a follow is at the look ahead
        anyToken `manyTill` lookAhead follow
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos from to t)
        let err' = err { errorExpected = f expected}

        errors %= (|> err')
        return $ Location (from, to) 