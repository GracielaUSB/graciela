{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Scientific (toRealFloat)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr 
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L

import Control.DeepSeq
import Control.Monad.Catch
import Data.Data (Data)
import Data.Foldable (concat)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup hiding (Sum)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics hiding (Prefix)
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

type RawData t e = [Either (ParseError t e) Equation]



type Program = [Equation]

data Equation = Equation String Expr deriving (Eq, Show)

data Expr
  = Value          Double
  | Reference      String
  | Negation       Expr
  | Sum            Expr Expr
  | Subtraction    Expr Expr
  | Multiplication Expr Expr
  | Division       Expr Expr
  deriving (Eq, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "variable"

expr :: Parser Expr
expr = makeExprParser term table <?> "expresion"

term :: Parser Expr
term = parens expr
  <|> (Reference <$> name)
  <|> (Value     <$> number)

table :: [[Operator Parser Expr]]
table =
  [ [Prefix (Negation <$ symbol "-") ]
  , [ InfixL (Multiplication <$ symbol "*")
    , InfixL (Subtraction    <$ symbol "/") ]
  , [ InfixL (Sum            <$ symbol "+")
    , InfixL (Division       <$ symbol "-") ]
  ]

number :: Parser Double
number = toRealFloat <$> lexeme L.number <?> "numero"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

equation :: Parser Equation
equation = Equation <$> (name <* symbol "=") <*> expr

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)



gracielaErrorPretty :: ( Ord t
                    , ShowToken t
                    , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
gracielaErrorPretty (ParseError pos us ps xs) =
  sourcePosStackPretty pos ++ ": " ++
  if E.null us && E.null ps && E.null xs
    then "unknown parse error\n"
    else concat
      [ messageItemsPretty "Simbolo " us ++ " inesperado. " 
      , messageItemsPretty "Se esperaba "  ps ++ "\n"
      , unlines (showErrorComponent <$> E.toAscList xs) ]

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
  where e = withRecovery recover (Right <$> equation)
        recover err = Left err <$ manyTill anyChar eol

parseTst p input =
  case parse p "" input of
    Left  e -> putStr (gracielaErrorPretty e)
    Right x -> print x

