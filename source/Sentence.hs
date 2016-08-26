{-|
Module      : Sentence
Description : Concept testing for custom type structures with type variables
Copyright   : (c) Moisés Ackerman, 2016
License     : GPL-3
Maintainer  : moises+sentence@ackerman.space
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sentence
  ( Sentence (..)
  , word
  , var
  , fill
  , spans
  , stringToSentence
  , sf
  ) where
--------------------------------------------------------------------------------
import           Data.Char      (isPunctuation, isSpace)
import           Data.Semigroup (Semigroup (..))
--------------------------------------------------------------------------------
-- Data and Instances ------------------

-- | A datatype for Sentences which possibly have unassigned variables.
data Sentence
  = Full String
    -- ^ A Full Sentence has no word variables.
  | Unfull Int (String -> Sentence)
    -- ^ An Unfull Sentence has one or more word variables.

instance Show Sentence where
  show (Full s)     = s
    -- Just show the String.
  show (Unfull i f) = show $ f ("\\$" <> show i)
    -- Show the String which results from substituting all variables for $i.

instance Semigroup Sentence where
  Full x     <> Full y     = Full     (x <> y)
    -- Just append the strings
  x@(Full _) <> Unfull j y = Unfull j (\z -> x <> y z)
    -- The resulting Sentence has the left variable as its first one.
  Unfull i x <> y@(Full _) = Unfull i (\z -> x z <> y)
    -- The resulting Sentence has the right variable as its first one.
  Unfull i x <> Unfull j y
    | i == j = Unfull i (\z -> x z <> y z)
      -- Both variables are joined as a single one.
    | i <  j = Unfull i (\z -> x z <> Unfull j y)
      -- The lesser variable is the outer one in the resulting Sentence.
    | i >  j = Unfull j (\z -> Unfull i x <> y z)
      -- The lesser variable is the outer one in the resulting Sentence.
    | otherwise = error "Excluded third"
      -- Impossible case, but ghc-mod was complaining.

instance Monoid Sentence where
  mempty = Full ""
  mappend = (<>)

--------------------------------------------------------------------------------
-- Helper functions --------------------

-- | A word is a Sentence with no variables.
word :: String -> Sentence
word = Full

-- | A var is an Unfull Sentence which returns exactly what is passed to it.
-- (It's the same as @Unfull i (\x -> Full x)@ but avoiding the lambda)
var :: Int -> Sentence
var i = Unfull i Full

-- | I honestly thought this was in the Prelude. It basically splits a List
-- into spans which alternate between satisfying and not satisfying a
-- predicate. For example:
--
-- >>> spans even [1,3,4,5,6,2,8,9]
-- [[1,3],[4],[5],[6,2,8],[9]]
--
-- (I basically needed an alternative to 'words' which didn't eliminate
-- the spaces)
spans :: (a -> Bool) -> [a] -> [[a]]
spans _ [] = []
spans p xs@(x:_)
  | p x       = spansSpan  xs
  | otherwise = spansBreak xs
  where
    spansSpan [] = []
    spansSpan zs =
      let (yes, no) = span p zs
      in yes : spansBreak no
    spansBreak [] = []
    spansBreak zs =
      let (no, yes) = break p zs
      in no : spansSpan yes

-- | /Parses/ a String into a Sentence. That is, it takes words of the
-- form "'$' ++ number" and converts them into variables, and leaves the rest
-- of the words alone.
stringToSentence :: String -> Sentence
stringToSentence s =
  mconcat (aux <$> spans (\x -> isSpace x || isPunctuation x) s)
  where
    aux :: String -> Sentence
    aux w = case w of
      '$':n -> case reads n of
        [(i, "")] -> var i
        _         -> word w
      _     -> word w

-- | To fill a Sentence with a word is to replace the first available
-- variable with the given String. If the Sentence is Full, nothing happens.
fill :: Sentence -> String -> Sentence
fill s@(Full _)   _ = s
fill (Unfull _ f) s = f s

--------------------------------------------------------------------------------
-- Typeclass magic ---------------------

class SF t where
  -- ^ The class of Sentences and Sentence-returning-functions.
  sf' :: [String] -> Sentence -> t
    -- ^ A function which takes a list of words and a Sentence, and returns
    -- either a Sentence or a Sentence-returning-function where the words
    -- in the list have been suitably substituted into the original Sentence.

instance SF Sentence where
  -- ^ A Sentence naturally belongs in the class of Sentences and
  -- Sentence-returning-functions.
  sf' acc sentence = foldr (flip fill) sentence acc
    -- The accumulated words are simply substituted into the sentence using
    -- 'fill', in right-to-left order.

instance SF r => SF (String -> r) where
  -- ^ If `r` is a Sentence or a Sentence-returning-function, the function
  -- which takes a String and returns `r` is most definitely a
  -- Sentence-returning-function.
  sf' acc sentence w = sf' (w : acc) sentence
    -- Given a word, pass it to `r` in the accumulated list. Since `r` must
    -- eventually be a Sentence, the base case will be reached, and the
    -- accumulator emptied.

-- Using the typeclass magic -----------

-- | Takes a String, which is interpreted as a Sentence, and an arbitrary
-- number of words which are substituted into the Sentence using the SF
-- class and instances. For instance:
--
-- >>> sf "$1, $2!" "Hello" "World" :: Sentence
-- "Hello, World!"
--
-- The type annotation is necessary since otherwise there are infinitely
-- many possible types for the expression (think String -> r) and Haskell
-- has no idea which one to pick.

sf :: (SF t) => String -> t
sf s = sf' [] (stringToSentence s)
