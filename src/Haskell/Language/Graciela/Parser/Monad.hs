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

module Language.Graciela.Parser.Monad
  ( ParserT
  , Parser

  , MonadParser (..)

  , evalParserT
  , execParserT
  , runParserT
  , evalParser
  , execParser
  , runParser

  , satisfy
  , match
  , anyToken
  , oneOf
  , noneOf
  , followedBy

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

  , some'
  , many'
  , sepBy1'

  , declarative
  , coupleRelation
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Struct
import           Language.Graciela.AST.Type      (Type)
import           Language.Graciela.Common
import           Language.Graciela.Error
import           Language.Graciela.Parser.Config (Config (..), defaultConfig)
import           Language.Graciela.Parser.Prim   ()
import           Language.Graciela.Parser.State  hiding (State)
import qualified Language.Graciela.Parser.State  as Parser (State)
import           Language.Graciela.Token         (Token (..), TokenPos (..))
--------------------------------------------------------------------------------
import           Control.Applicative             (Alternative)
import           Control.Lens                    (use, view, (%=), (.=), (<<.=),
                                                  (<~), (^.), _1, _2)
import           Control.Monad                   (MonadPlus)
import           Control.Monad.Identity          (Identity (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader            (MonadReader (..), asks)
import           Control.Monad.State             (MonadState)
import           Control.Monad.Trans.Except      (ExceptT (..), catchE,
                                                  runExceptT, throwE)
import           Control.Monad.Trans.Reader      (ReaderT (..), runReaderT)
import           Control.Monad.Trans.State       (StateT (..), evalStateT)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE (fromList)
import qualified Data.Map.Strict                 as Map (empty, lookup)
import           Data.Sequence                   (Seq, (<|), (|>))
import qualified Data.Sequence                   as Seq (empty, singleton)
import qualified Data.Set                        as Set (empty, singleton)
import           Data.Text                       (Text)
import           Text.Megaparsec                 (ErrorItem (..),
                                                  ParseError (..), ParsecT,
                                                  between, getPosition,
                                                  lookAhead, manyTill,
                                                  withRecovery, (<|>))
import qualified Text.Megaparsec                 as Mega (runParserT)
import           Text.Megaparsec.Error           (parseErrorTextPretty)
import           Text.Megaparsec.Prim            (MonadParsec (..))
--------------------------------------------------------------------------------

-- | Graciela Parser monad transformer.
newtype ParserT m a = ParserT
  { unParserT :: ParsecT Error [TokenPos] (ReaderT Config (StateT Parser.State m)) a }
  deriving ( Functor, Applicative, Monad
           , MonadState Parser.State
           , MonadParsec Error [TokenPos]
           , MonadReader Config
           , MonadPlus, Alternative
           , MonadIO)

-- | Graciela Parser monad.
type Parser = ParserT IO
--------------------------------------------------------------------------------

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, and return a tuple with the final value and state.
runParserT  :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m (Either (ParseError TokenPos Error) a, Parser.State)
runParserT p fp s input = runStateT (runReaderT flatten cfg) s
    where
      cfg = defaultConfig (EnableTrace `elem` s^.pragmas) (MemoryOperations `elem` s^.pragmas)

      flatten = do
        definitions <~ asks nativeFunctions
        symbolTable <~ asks nativeSymbols

        x <- Mega.runParserT (unParserT p) fp input
        pure $ case x of
          Right (Just v) -> Right v
          Left e         -> Left  e

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final state.
evalParserT :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m (Either (ParseError TokenPos Error) a)
evalParserT p fp s input = view _1 <$> runParserT p fp s input

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final value.
execParserT :: Monad m
            => ParserT m (Maybe a)
            -> FilePath
            -> Parser.State
            -> [TokenPos]
            -> m Parser.State
execParserT  p fp s input = view _2 <$> runParserT p fp s input
--------------------------------------------------------------------------------

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, and return a tuple with the final value and state.
runParser  :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> IO (Either (ParseError TokenPos Error) a, Parser.State)
runParser  p fp s input = runParserT p fp s input
-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final state.
evalParser :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> IO (Either (ParseError TokenPos Error) a)
evalParser p fp s input = evalParserT p fp s input

-- | Evaluate a parser computation with the given filename, stream of tokens,
-- and initial state, discarding the final value.
execParser :: Parser (Maybe a)
           -> FilePath
           -> Parser.State
           -> [TokenPos]
           -> IO Parser.State
execParser p fp s input = execParserT p fp s input
--------------------------------------------------------------------------------

infixl 3 <!>
infixl 3 <!!>

class MonadParsec Error [TokenPos] p => MonadParser p where
  putError :: SourcePos -> Error -> p ()
  getType :: Text -> p (Maybe Type)
  getStruct :: Text -> p (Maybe Struct)
  satisfy' :: (Token -> Bool) -> p TokenPos
  match' :: Token -> p Location
  (<!>) :: p (Maybe a) ->  (SourcePos, Error) -> p (Maybe a)
  a <!> (p, e) = a <|> (putError p e *> pure Nothing)
  (<!!>) :: p a -> (SourcePos, Error) -> p (Maybe a)
  a <!!> b  = Just <$> a <!> b

instance Monad m => MonadParser (ParserT m) where
  putError     = pPutError
  getType      = pGetType
  getStruct    = pGetStruct
  satisfy'     = pSatisfy'
  match'       = pMatch'

instance MonadParser g => MonadParser (StateT s g) where
  putError l e        = lift $ putError l e
  getType             = lift . getType
  getStruct           = lift . getStruct
  satisfy'            = lift . satisfy'
  match'              = lift . match'

pPutError :: Monad m => SourcePos -> Error -> ParserT m ()
pPutError from e = ParserT $ do
  let
    err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
  errors %= (|> err)

pGetType :: (Monad m)
         => Text -> ParserT m (Maybe Type)
pGetType name = do
  types <- asks nativeTypes
  case name `Map.lookup` types of
    Just (t, loc) -> return $ Just t
    Nothing       -> return Nothing

pGetStruct :: (Monad m)
           => Text -> ParserT m (Maybe Struct)
pGetStruct name = Map.lookup name <$> use dataTypes

pRecover :: MonadParser m
         => m b
         -> ParseError TokenPos Error
         -> m (Maybe a)
pRecover follow e = do
  pos <- getPosition

  putError pos . UnknownError . init $ parseErrorTextPretty e

  void $ anyToken `manyTill` (void (lookAhead follow) <|> eof)

  pure Nothing

pSatisfy' :: Monad m
         => (Token -> Bool) -> ParserT m TokenPos
pSatisfy' f = token test Nothing
  where
    test tp @ TokenPos { tok } =
      if f tok
        then Right tp
        else Left . unex $ tp
    unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])

pMatch' :: Monad m
         => Token-> ParserT m Location
pMatch' t = withRecovery recover (match t)
  where
    recover e = do
      pos <- getPosition
      -- Modify the error, so it knows the expected token (there is obviously a better way, IDK right now)
      let
        from :| _ = errorPos e
        expected  = Set.singleton . Tokens . NE.fromList $ [TokenPos from from t]
        loc       = Location (pos, pos)

      errors %= (|> e { errorExpected = expected } )

      pure loc
--------------------------------------------------------------------------------

satisfy :: MonadParser m
         => (Token -> Bool) -> m Token
satisfy f = tok <$> satisfy' f

match :: MonadParser m
      => Token -> m Location
match t = do
  TokenPos { start, end } <- satisfy' (== t)
  pure $ Location (start, end)

anyToken :: MonadParser m => m Token
anyToken = satisfy (const True)

oneOf :: (Foldable f, MonadParser m)
      => f Token -> m Token
oneOf ts = satisfy (`elem` ts)

noneOf :: (Foldable f, MonadParser m)
      => f Token -> m Token
noneOf ts = satisfy (`notElem` ts)

followedBy :: (MonadParser m)
            => m (Maybe a) -> m b -> m (Maybe a)
followedBy p follow =
  withRecovery (pRecover follow) (p <* lookAhead follow)
--------------------------------------------------------------------------------

boolLit :: MonadParser m
        => m Bool
boolLit = unTokBool <$> satisfy bool
  where
    bool t@TokBool {} = True
    bool _            = False

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
  TokenPos { tok = TokId name, start, end } <- satisfy' ident
  pure (name, Location (start, end))
  where
    ident TokId {} = True
    ident _        = False
--------------------------------------------------------------------------------

parens :: MonadParser m
       => m a -> m a
parens = between
  (match  TokLeftPar )
  (match' TokRightPar)

brackets :: MonadParser m
         => m a -> m a
brackets = between
  (match  TokLeftBracket )
  (match' TokRightBracket)

-- block :: MonadParser m
--       => m a -> m a
-- block = between
--   (match TokOpenBlock )
--   (match TokCloseBlock)

percents :: MonadParser m
         => m a -> m a
percents = between
  (match  TokLeftPercent )
  (match' TokRightPercent)

beginEnd :: MonadParser m
         => m a -> m a
beginEnd = between
  (match  TokBegin)
  (match' TokEnd  )
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

-- | One or more, carrying a state.
some' :: (Monad m, Alternative m) => (a -> m a) -> a -> m a
some' v s = v s >>= many' v

-- | Zero or more, carrying a state
many' :: (Monad m, Alternative m) => (a -> m a) -> a -> m a
many' v s = some' v s <|> pure s

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

-- | @sepBy1' s p sep@ parses /one/ or more occurrences of @p s@, separated
-- by @sep@. Returns a sequence of values returned by @p@.
sepBy1' :: (Monad m, Alternative m) => (a -> m a) -> m sep -> a -> m a
sepBy1' p sep s = p s >>= many' (\t -> sep *> p t)

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

declarative :: (MonadParser m, MonadState Parser.State m)
          => m a -> m a
declarative p = (isDeclarative <<.= True) >>= \x -> (p <* (isDeclarative .= x))

coupleRelation :: (MonadParser m, MonadState Parser.State m)
          => m a -> m a
coupleRelation p = (doingCoupleRel <<.= True) >>= \x -> (p <* (isDeclarative .= x))
--------------------------------------------------------------------------------
