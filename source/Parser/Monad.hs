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
  , safeIdentifier
  , identifierAndLoc

  , parens
  , brackets
  -- , block
  , percents
  , beginEnd

  , some
  , many

  , endBy
  , endBy1
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1

  , unsafeGenCustomError
  ) where
--------------------------------------------------------------------------------
import           Error
import           Location
import           Parser.Prim                ()
import           Parser.State               hiding (State)
import qualified Parser.State               as Parser (State)
import           Token                      (Token (..), TokenPos (..))
import           Type                       (Type)
--------------------------------------------------------------------------------
import           Control.Applicative        (Alternative)
import           Control.Lens               (use, (%=))
import           Control.Monad              (MonadPlus, void)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT,
                                             throwE)
import           Control.Monad.Trans.State  (StateT (..), evalStateT)
import           Data.Int                   (Int32)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE (fromList)
import qualified Data.Map                   as Map (lookup)
import           Data.Sequence              (Seq, (<|), (|>))
import qualified Data.Sequence              as Seq (empty, singleton)
import qualified Data.Set                   as Set (empty, singleton)
import           Data.Text                  (Text)
import           Text.Megaparsec            (ErrorItem (..), ParseError (..),
                                             ParsecT, between, getPosition,
                                             lookAhead, manyTill, withRecovery,
                                             (<|>))
import qualified Text.Megaparsec            as Mega (runParserT)
import           Text.Megaparsec.Prim       (MonadParsec (..))
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

infixl 3 <!>
infixl 3 <!!>

class MonadParsec Error [TokenPos] p => MonadParser p where
  putError :: SourcePos -> Error -> p ()
  getType :: Text -> p (Maybe Type)
  followedBy :: p (Maybe a) -> p b -> p (Maybe a)
  satisfy :: (Token -> Bool) -> p Token
  match' :: Token -> p Location
  (<!>) :: p (Maybe a) ->  (SourcePos, Error) -> p (Maybe a)
  a <!> (p, e) = a <|> (putError p e *> pure Nothing)
  (<!!>) :: p a -> (SourcePos, Error) -> p (Maybe a)
  a <!!> b  = Just <$> a <!> b

instance Monad m => MonadParser (ParserT m) where
  putError     = pPutError
  getType      = pGetType
  followedBy   = pFollowedBy
  satisfy      = pSatisfy
  match'       = pMatch'

instance MonadParser g => MonadParser (StateT s g) where
  putError l e        = lift $ putError l e
  getType             = lift . getType
  followedBy p follow = withRecovery (pRecover follow) p
  satisfy             = lift . satisfy
  match'              = lift . match'

pPutError :: Monad m => SourcePos -> Error -> ParserT m ()
pPutError from e = ParserT $ do
  let
    err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
  errors %= (|> err)

pGetType :: (Monad m)
         => Text -> ParserT m (Maybe Type)
pGetType name = do
  types <- use typesTable
  case Map.lookup name types of
    Just (t, loc) -> return $ Just t
    Nothing       -> return Nothing

pFollowedBy :: (Monad m)
            => ParserT m (Maybe a) -> ParserT m b -> ParserT m (Maybe a)
pFollowedBy p follow =
  withRecovery (pRecover follow) (p <* lookAhead follow)


pRecover :: (MonadParser m)
         => m b
         -> ParseError TokenPos Error
         -> m (Maybe a)
pRecover follow e = do
  pos <- getPosition

  putError pos . UnexpectedToken $ errorUnexpected e

  void $ anyToken `manyTill` (void (lookAhead follow) <|> eof)

  pure Nothing


pSatisfy :: Monad m
         => (Token -> Bool) -> ParserT m Token
pSatisfy f = token test Nothing
  where
    test tp @ TokenPos { tok } =
      if f tok
        then Right tok
        else Left . unex $ tp
    unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


pMatch' :: Monad m
         => Token-> ParserT m Location
pMatch' t = withRecovery recover (match t)
  where
    recover e = do
      pos <- getPosition

      let loc = Location (pos, pos)

      putError pos . UnexpectedToken $ errorUnexpected e

      pure loc
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

safeIdentifier :: MonadParser m
               => m (Maybe Text)
safeIdentifier = withRecovery recover (Just <$> identifier)
  where
    recover e = do
      pos <- getPosition

      putError pos . UnknownError $
        "An identifier was expected but none was given."

      pure Nothing

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
       => m a -> m a
parens = between
  (match TokLeftPar )
  (match TokRightPar)

brackets :: MonadParser m
         => m a -> m a
brackets = between
  (match TokLeftBracket )
  (match TokRightBracket)

-- block :: MonadParser m
--       => m a -> m a
-- block = between
--   (match TokOpenBlock )
--   (match TokCloseBlock)

percents :: MonadParser m
         => m a -> m a
percents = between
  (match TokLeftPercent )
  (match TokRightPercent)

beginEnd :: MonadParser m
         => m a -> m a
beginEnd = between
  (match TokBegin)
  (match TokEnd  )

--------------------------------------------------------------------------------
-- | One or more.
some :: Alternative m => m a -> m (Seq a)
some v = some_v
  where
    many_v = some_v <|> pure Seq.empty
    some_v = fmap (<|) v <*> many_v
{-# INLINE some #-}

-- | Zero or more.
many :: Alternative m => m a -> m (Seq a)
many v = many_v
  where
    many_v = some_v <|> pure Seq.empty
    some_v = fmap (<|) v <*> many_v
{-# INLINE many #-}

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a sequence of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon
endBy :: Alternative m => m a -> m sep -> m (Seq a)
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a sequence of values returned by @p@.
endBy1 :: Alternative m => m a -> m sep -> m (Seq a)
endBy1 p sep = some (p <* sep)
{-# INLINE endBy1 #-}

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a sequence of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma
sepBy :: Alternative m => m a -> m sep -> m (Seq a)
sepBy p sep = sepBy1 p sep <|> pure Seq.empty
{-# INLINE sepBy #-}

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a sequence of values returned by @p@.
sepBy1 :: Alternative m => m a -> m sep -> m (Seq a)
sepBy1 p sep = (<|) <$> p <*> many (sep *> p)
{-# INLINE sepBy1 #-}

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a sequence of values
-- returned by @p@.
sepEndBy :: Alternative m => m a -> m sep -> m (Seq a)
sepEndBy p sep = sepEndBy1 p sep <|> pure Seq.empty
{-# INLINE sepEndBy #-}

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.
sepEndBy1 :: Alternative m => m a -> m sep -> m (Seq a)
sepEndBy1 p sep = (<|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure Seq.empty)
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
--------------------------------------------------------------------------------
unsafeGenCustomError :: String -> Parser ()
unsafeGenCustomError msg = do
    pos <- getPosition
    synErrorList %= (|> CustomError msg (Location (pos,pos)))
