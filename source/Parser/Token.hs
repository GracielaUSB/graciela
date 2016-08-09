{-|
Module      : Parser.Token
Description : Todos los lexemas del lenguaje
Copyright   : Graciela

Contiene los analizadores semánticos (parsers) básicos del compilador,
que funcionan como bloques para analizadores semánticos más complejos.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Token
  ( anyToken
  , satisfy
  , match
  , oneOf
  , parens
  , percents
  , brackets
  , beginEnd
  , identifier
  , boolLit
  , charLit
  , stringLit
  , integerLit
  , floatLit
  , errorId
  , identifierWithRecovery
  , Parser.Token.withRecovery
  , withRecoveryFollowedBy
  ) where
--------------------------------------------------------------------------------
import           Token
import           Location
import           MyParseError
import           Graciela
--------------------------------------------------------------------------------
import           Control.Monad         (when, void)
import           Control.Lens          ((%=))
import           Data.List             (intercalate) 
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NE
import           Data.Set              (Set)
import           Data.Sequence         ((|>))
import qualified Data.Set              as Set
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (ErrorItem (Tokens), ParseError(..), 
                                        token, between, manyTill, lookAhead, 
                                        parseErrorPretty, getPosition,
                                        ShowErrorComponent,showErrorComponent, 
                                        ShowToken)
import           Text.Megaparsec        as MP (withRecovery)
-- import           Text.Megaparsec.Error (sourcePosStackPretty)
import           Text.Megaparsec.Prim  (MonadParsec)
import qualified Text.Megaparsec.Prim  as Prim (Token)
--------------------------------------------------------------------------------

unex :: TokenPos -> (Set (ErrorItem TokenPos), Set a, Set b)
unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


match :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => Token -> m Location
match t = token test Nothing
  where
    test tp @ TokenPos { tok, start, end } =
      if t == tok
        then Right $ Location (start, end)
        else Left . unex $ tp


satisfy :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => (Token -> Bool) -> m Token
satisfy f = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if f tok
        then Right tok
        else Left . unex $ tp


oneOf :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => [Token] -> m Token
oneOf ts = satisfy (`elem` ts)


parens :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
       => m a -> m a
parens = between (match TokLeftPar) (match TokRightPar)


percents :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
percents = between (match TokLeftPercent) (match TokRightPercent)


brackets :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
brackets = between (match TokLeftBracket) (match TokRightBracket)


beginEnd :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
beginEnd = between (match TokBegin) (match TokEnd)


anyToken :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Token
anyToken = token test Nothing
  where
    test TokenPos {tok} = Right tok


identifier :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Text
identifier = token test Nothing
  where
    test    TokenPos {tok = TokId i} = Right i
    test tp@TokenPos {tok}           = Left . unex $ tp


boolLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Bool
boolLit = token test Nothing
  where
    test    TokenPos {tok = TokBool b} = Right b
    test tp@TokenPos {tok}             = Left . unex $ tp


charLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Char
charLit = token test Nothing
  where
    test    TokenPos {tok = TokChar c} = Right c
    test tp@TokenPos {tok}             = Left . unex $ tp


stringLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
          => m Text
stringLit = token test Nothing
  where
    test    TokenPos {tok = TokString s} = Right s
    test tp@TokenPos {tok}               = Left . unex $ tp


integerLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Integer
integerLit = token test Nothing
  where
    test    TokenPos {tok = TokInteger i} = Right i
    test tp@TokenPos {tok}                = Left . unex $ tp


floatLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Double
floatLit = token test Nothing
  where
    test    TokenPos {tok = TokFloat f} = Right f
    test tp@TokenPos {tok}              = Left . unex $ tp


errorId :: Text
errorId = pack "0#Error"

identifierWithRecovery :: Graciela Text
identifierWithRecovery = MP.withRecovery recover identifier
  where 
    recover err = do
      genCustomError (parseErrorPretty err)
      return errorId

withRecovery :: Token -> Graciela Location 
withRecovery token = MP.withRecovery (recover [token]) (match token)   
  where 
    recover expected err = do 
        pos <- getPosition
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos pos pos t)
        let err' = err { errorExpected = f expected}
        -- Print (put it in the error list)
        errors %= (|> err')
        return $ Location (gracielaDef,gracielaDef) 

withRecoveryFollowedBy :: Token -> Graciela Token -> Graciela Location 
withRecoveryFollowedBy token follow = MP.withRecovery (recover [token] follow) (match token)   
  where 
    recover expected follow err = do 
        pos <- getPosition
        -- if any follow token is especified, then trash many token until a follow is at the look ahead
        anyToken `manyTill` lookAhead follow
        -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
        let f = Set.singleton . Tokens . NE.fromList . fmap (\t -> TokenPos pos pos t)
        let err' = err { errorExpected = f expected}

        errors %= (|> err')
        return $ Location (gracielaDef,gracielaDef) 


-- Modify the pretty print of errores

-- prettyError :: ( Ord t
--                , ShowToken t
--                , ShowErrorComponent e )
--   => ParseError t e    -- ^ Parse error to render
--   -> String            -- ^ Result of rendering
-- prettyError (ParseError pos us ps xs) =
--   sourcePosStackPretty pos ++ ":\n" ++
--   if Set.null us && Set.null ps && Set.null xs
--     then "unknown parse error\n"
--     else concat
--       [ messageItemsPretty "\tunexpected " us
--       , messageItemsPretty "\texpecting "  ps
--       , unlines (showErrorComponent <$> Set.toAscList xs) ]


-- messageItemsPretty :: ShowErrorComponent a
--   => String            -- ^ Prefix to prepend
--   -> Set a             -- ^ Collection of messages
--   -> String            -- ^ Result of rendering
-- messageItemsPretty prefix ts
--   | Set.null ts = ""
--   | otherwise =
--     let f = orList . NE.fromList . Set.toAscList . Set.map showErrorComponent
--     in prefix ++ f ts ++ "\n"


-- orList :: NonEmpty String -> String
-- orList (x:|[])  = x
-- orList (x:|[y]) = x ++ " or " ++ y
-- orList xs       = intercalate ", " (NE.init xs) ++ ", or " ++ NE.last xs