{-|
Module      : Language.Graciela.Parser.Monad
Description : The parsing monad for Graciela
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

This is a modified ParsecT monad with a custom state, operating on a
stream of TokenPos.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

module Parser.Monad
  ( ParserT
  , Parser

  , MonadParser (..)

  , evalParserT
  , execParserT
  , runParserT
  , evalParser
  , execParser
  , runParser

  , match
  , anyToken
  , oneOf
  , noneOf

  , boolLit
  , charLit
  , integerLit
  , floatLit
  , stringLit
  , identifier
  , identifierAndLoc

  , parens
  , brackets
  , block
  , percents
  , beginEnd
  ) where
--------------------------------------------------------------------------------
import           Error
import           Location
import           Parser.Prim               ()
import           Parser.State              hiding (State)
import qualified Parser.State              as Parser (State)
import           Token                     (Token (..), TokenPos (..))
--------------------------------------------------------------------------------
import           Control.Applicative       (Alternative)
import           Control.Lens              (use, (%=))
import           Control.Monad             (MonadPlus, void)
import           Control.Monad.Identity    (Identity (..))
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT (..), evalStateT)
import           Data.Int                  (Int32)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE (fromList)
import           Data.Sequence             ((|>))
import qualified Data.Set                  as Set (empty, singleton)
import           Data.Text                 (Text)
import qualified Text.Megaparsec                      as Mega (runParserT)
import           Text.Megaparsec           (ErrorItem (..), ParseError (..),
                                            ParsecT, between, getPosition,
                                            lookAhead, manyTill, withRecovery,
                                            (<|>))
import           Text.Megaparsec.Prim      (MonadParsec (..))
--------------------------------------------------------------------------------

-- | Graciela Parser monad transformer.
newtype ParserT m a = ParserT
  { unParserT :: ParsecT Error [TokenPos] (StateT Parser.State m) a }
  deriving ( Functor, Applicative, Monad, MonadState Parser.State
           , MonadParsec Error [TokenPos], MonadPlus, Alternative )

-- | Graciela Parser monad.
type Parser = ParserT Identity
--------------------------------------------------------------------------------

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, and return a tuple with the final value and state.
runParserT  :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m (Maybe a, Parser.State)
runParserT p fp s input = runStateT flatten s
  where
    flatten = do
      x <- Mega.runParserT (unParserT p) fp input
      pure $ case x of
        Right (Just v) -> Just v
        _              -> Nothing

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final state.
evalParserT :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m (Maybe a)
evalParserT p fp s input = fst <$> runParserT p fp s input

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final value.
execParserT :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m Parser.State
execParserT  p fp s input = snd <$> runParserT p fp s input
--------------------------------------------------------------------------------

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, and return a tuple with the final value and state.
runParser  :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> (Maybe a, Parser.State)
runParser  p fp s input = runIdentity $ runParserT p fp s input
-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final state.
evalParser :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> Maybe a
evalParser p fp s input = runIdentity $ evalParserT p fp s input

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final value.
execParser :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> Parser.State
execParser p fp s input = runIdentity $ execParserT p fp s input
--------------------------------------------------------------------------------

class MonadParsec Error [TokenPos] p => MonadParser p where
  putError :: Location -> Error -> p ()
  reject :: p (Maybe a)
  safe :: p (Maybe a) -> p (Maybe a)
  push :: Token -> p ()
  pop :: p ()
  satisfy :: (Token -> Bool) -> p Token

instance Monad m => MonadParser (ParserT m) where
  putError = pPutError
  reject   = pReject
  safe     = pSafe
  push     = pPush
  pop      = pPop
  satisfy  = pSatisfy

instance MonadParser g => MonadParser (StateT s g) where
  putError l e = lift $ putError l e
  reject  = lift reject
  safe p = StateT $ \s -> do
    a' <- safe $ evalStateT p s
    return (a', s)
  push    = lift . push
  pop     = lift pop
  satisfy = lift . satisfy

pPutError :: Monad m => Location -> Error -> ParserT m ()
pPutError (Location (from, _)) e = ParserT $ do
  let
    err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
  errors %= (|> err)
pPutError _ _ = error "FIXME"

pReject :: Monad m => ParserT m (Maybe a)
pReject = pure Nothing

pSafe :: (Monad m)
      => ParserT m (Maybe a) -> ParserT m (Maybe a)
pSafe = withRecovery r
  where
    r e = do
      pos <- getPosition

      putError
        (Location (pos, undefined))
        (UnexpectedToken (errorUnexpected e))

      ts <- use recSet
      void $ noneOf ts `manyTill` (lookAhead (void $ oneOf ts) <|> eof)

      reject

pPush :: Monad m
      => Token -> ParserT m ()
pPush t = ParserT $ recSet %= (t:)

pPop :: Monad m
     => ParserT m ()
pPop = ParserT $ recSet %= tail

pSatisfy :: Monad m
         => (Token -> Bool) -> ParserT m Token
pSatisfy f = token test Nothing
  where
    test tp @ TokenPos { tok } =
      if f tok
        then Right tok
        else Left . unex $ tp
    unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])
--------------------------------------------------------------------------------

match :: MonadParser m
      => Token -> m Location
match t = do
  from <- getPosition
  void $ satisfy (== t)
  to <- getPosition
  pure $ Location (from, to)

anyToken :: MonadParser m => m Token
anyToken = satisfy (const True)

oneOf :: (Foldable f, MonadParser m)
      => f Token -> m Token
oneOf ts = satisfy (`elem` ts)

noneOf :: (Foldable f, MonadParser m)
      => f Token -> m Token
noneOf ts = satisfy (`notElem` ts)

--------------------------------------------------------------------------------
boolLit :: MonadParser m
        => m Bool
boolLit = unTokBool <$> satisfy bool
  where
    bool TokBool {} = True
    bool _          = False

charLit :: MonadParser m
        => m Char
charLit = unTokChar <$> satisfy char
  where
    char TokChar {} = True
    char _          = False

integerLit :: MonadParser m
           => m Int32
integerLit = unTokInteger <$> satisfy string
  where
    string TokInteger {} = True
    string _             = False

floatLit :: MonadParser m
         => m Double
floatLit = unTokFloat <$> satisfy float
  where
    float TokFloat {} = True
    float _           = False

stringLit :: MonadParser m
          => m Text
stringLit = unTokString <$> satisfy string
  where
    string TokString {} = True
    string _            = False

identifier :: MonadParser m
           => m Text
identifier = unTokId <$> satisfy ident
  where
    ident TokId {} = True
    ident _        = False

-- | Match an identifier and return both its name and location
identifierAndLoc :: MonadParser m
                 => m (Text, Location)
identifierAndLoc = do
  from <- getPosition
  name <- identifier
  to <- getPosition
  pure (name, Location(from,to))

--------------------------------------------------------------------------------
parens :: MonadParser m
       => m (Maybe a) -> m (Maybe a)
parens = (. safe) $ between
  (match TokLeftPar  <* push TokRightPar)
  (match TokRightPar <* pop)

brackets :: MonadParser m
         => m (Maybe a) -> m (Maybe a)
brackets = (. safe) $ between
  (match TokLeftBracket  <* push TokRightBracket)
  (match TokRightBracket <* pop)

block :: MonadParser m
      => m (Maybe a) -> m (Maybe a)
block = (. safe) $ between
  (match TokOpenBlock  <* push TokCloseBlock)
  (match TokCloseBlock <* pop)

percents :: MonadParser m
         => m (Maybe a) -> m (Maybe a)
percents = (. safe) $ between
  (match TokLeftPercent  <* push TokRightPercent)
  (match TokRightPercent <* pop)

beginEnd :: MonadParser m
         => m (Maybe a) -> m (Maybe a)
beginEnd = (. safe) $ between
  (match TokBegin <* push TokEnd)
  (match TokEnd   <* pop)

--------------------------------------------------------------------------------

-- insertType :: Text -> Type -> SourcePos -> Graciela ()
-- insertType name t loc =
--   typesTable %= Map.insert name (t, loc)
--
--
-- getType :: Text -> Graciela (Maybe Type)
-- getType name = do
--   types <- use typesTable
--   case Map.lookup name types of
--     Just (t, loc) -> return $ Just t
--     Nothing       -> return Nothing
--
-- --------------------------------------------------------------------------------
-- unsafeGenCustomError :: String -> Graciela ()
-- unsafeGenCustomError msg = ParserT $ do
--     pos <- getPosition
--     synErrorList %= (|> CustomError msg  (Location (pos,pos)))
