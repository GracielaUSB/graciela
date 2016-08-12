
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
  , prettyError
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
import           Data.List             (intercalate)
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
import           Text.Megaparsec.Error        (sourcePosStackPretty)
import qualified Text.Megaparsec.Prim  as Prim (Token)

{- Use when an identifier parse fails. Its an ilegal id in graciela -}
errorId :: Text
errorId = pack "0#Error"

{- Try the assertion p. if fail then report Error e and return bad expression-}
safeAssertion :: Graciela Expression -> Error -> Graciela Expression
safeAssertion p e = try p <|> recover
  where 
    recover = do 
      pos <- getPosition 
      let location = Location(pos,pos)
      putError location e
      return $ BadExpression location

{- Parse an expression. If fail then report an error and return bad expression -}
safeExpression :: Graciela Expression 
safeExpression = MP.withRecovery recover expression
  where
    recover err = do
      let from :| [_] = errorPos err
      to <- getPosition
      errors %= (|> err)
      let loc = Location (from, to) 
      return $ BadExpression loc

{- Parse an identifier. If fail then report an error and return `errorId` -}
safeIdentifier :: Graciela Text
safeIdentifier = MP.withRecovery recover identifier
  where 
    recover err = do
      genCustomError (parseErrorPretty err)
      return errorId

{- Try parse the token. If fail then report an error -}
withRecovery :: Token -> Graciela Location 
withRecovery token = MP.withRecovery (recover [token]) (match token)   
  where 
    recover expected err = do 
        let from :| [_] = errorPos err
        to <- getPosition
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos from to t)
        let err' = err { errorExpected = f expected }
        -- Print (put it in the error list)
        errors %= (|> err')
        return $ Location (from, to) 

{- Try parse the token. If fail then report an error and start a panic mode until follow is found -}
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
        let err' = err { errorExpected = f expected }
        -- Print (put it in the error list)
        errors %= (|> err')
        return $ Location (from, to) 



-- Modify the pretty print of errores
prettyError :: ( Ord t
               , ShowToken t
               , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
prettyError (ParseError pos us ps xs) =
  sourcePosStackPretty pos ++ ":\n" ++
  if Set.null us && Set.null ps && Set.null xs
    then "unknown parse error\n"
    else concat
      [ messageItemsPretty "\t Found unexpected: " us
      , messageItemsPretty "\t instead of: "  ps
      , unlines . fmap ("\t"++) $ (showErrorComponent <$> Set.toAscList xs) 
      ]


messageItemsPretty :: ShowErrorComponent a
  => String            -- ^ Prefix to prepend
  -> Set a             -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | Set.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . Set.toAscList . Set.map showErrorComponent
    in prefix ++ f ts ++ "\n"


orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x ++ " or " ++ y
orList xs       = intercalate ", " (NE.init xs) ++ ", or " ++ NE.last xs