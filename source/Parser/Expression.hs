{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST
import           Graciela
import           Lexer
import           Limits
import           MyParseError              as PE
import           Parser.Token
import           ParserState               as PS
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad             (void)
import           Control.Monad.Trans.State (evalState)
import           Data.Functor              (($>))
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, unpack)
import           Text.Megaparsec           (between, runParser, runParserT,
                                            sepBy, some, try, (<|>))
import           Text.Megaparsec           hiding (Token)
import           Text.Megaparsec.Expr      (Operator (..), makeExprParser)
--------------------------------------------------------------------------------

import           Data.Tree



expression :: Graciela AST
expression = do 
    makeExprParser term operator
    p <- getPosition                    -- Dummy
    return $ AST p p GEmpty (Bool True) -- Dummy

expression' :: Graciela (Tree String)
expression' = makeExprParser term operator

quantification = percents q
  where
    q = do
      q <- quantifier
      (var, t) <- declaration
      match TokPipe
      range <- expression'
      match TokPipe
      expr <- expression'
      pure . Node "Quantification" $
        [ leaf . show $ q
        , leaf . unpack $  var
        , leaf . unpack $ t
        ]



declaration = do
  var <- identifier
  match TokColon
  t <- identifier
  pure (var, t)


quantifier :: Graciela QuantOp
quantifier =  (match TokExist  $> Exists)
          <|> (match TokMax    $> Maximum)
          <|> (match TokSigma  $> Summation)
          <|> (match TokMin    $> Minimum)
          <|> (match TokForall $> ForAll)
          <|> (match TokPi     $> Product)


leaf x = Node x []


call :: Graciela (Tree String)
call = do
  name <- identifier
  args <- parens (expression' `sepBy` match TokComma)
  return $
    Node ("call " <> unpack name) $
      case args of
        [] -> [leaf "no args"]
        _  -> (\(i, arg) -> Node ("arg " ++ show i) [arg]) <$> zip [0..] args


term :: Graciela (Tree String)
term =  parens expression'
    <|> try call
    <|> value
    <|> (leaf . show <$> boolLit)
    <|> (leaf . show <$> integerLit)
    <|> (leaf . show <$> floatLit)
    <|> (leaf . show <$> charLit)
    <|> quantification
  where
    value  = makeExprParser value' ops
    value' =  parens value
          <|> (leaf . show <$> identifier)
    ops :: [[ Operator Graciela (Tree String) ]]
    ops =
      [ {-Level 0-}
        [ Postfix (do
            e <- subindex
            return (\x -> Node "sub" [x,e]))
        ]
      , {-Level 1-}
        [Prefix (foldr1 (.) <$> some pointer)]
      ]
    subindex = brackets expression'

    pointer = match TokTimes $> \x -> Node "pointer to" [x]

operator :: [[ Operator Graciela (Tree String) ]]
operator =
  [ {-Level 2-}
    [ Prefix (match TokNot        $> \x -> Node "not" [x])
    , Prefix (match TokMinus      $> \x -> Node "minus" [x])
    ]
  , {-Level 3-}
    [ InfixR (match TokPower      $> \x y -> Node "^" [x,y])
    ]
  , {-Level 4-}
    [ InfixL (match TokTimes      $> \x y -> Node "*" [x,y])
    , InfixL (match TokDiv        $> \x y -> Node "/" [x,y])
    , InfixL (match TokMod        $> \x y -> Node "mod" [x,y])
    ]
  , {-Level 5-}
    [ InfixL (match TokPlus       $> \x y -> Node "+" [x,y])
    , InfixL (match TokMinus      $> \x y -> Node "-" [x,y])
    ]
  , {-Level 6-}
    [ InfixL (match TokMax        $> \x y -> Node "max" [x,y])
    , InfixL (match TokMin        $> \x y -> Node "min" [x,y])
    ]
  , {-Level 7-}
    [ InfixN (match TokLT         $> \x y -> Node "<" [x,y])
    , InfixN (match TokLE         $> \x y -> Node "<=" [x,y])
    , InfixN (match TokGT         $> \x y -> Node ">" [x,y])
    , InfixN (match TokGE         $> \x y -> Node ">=" [x,y])
    ]
  , {-Level 8-}
    [ InfixN (match TokAEQ        $> \x y -> Node "==" [x,y])
    , InfixN (match TokANE        $> \x y -> Node "!=" [x,y])
    ]
  , {-Level 9-}
    [ InfixR (match TokAnd        $> \x y -> Node "/\\" [x,y])
    ]
  , {-Level 10-}
    [ InfixR (match TokOr         $> \x y -> Node "\\/" [x,y])
    ]
  , {-Level 11-}
    [ InfixR (match TokImplies    $> \x y -> Node "==>" [x,y])
    , InfixL (match TokConsequent $> \x y -> Node "<==" [x,y])
    ]
  , {-Level 12-}
    [ InfixN (match TokBEQ        $> \x y -> Node "===" [x,y])
    , InfixN (match TokBNE        $> \x y -> Node "!==" [x,y])
    ]
  ]


concatLexPar :: Text -> IO ()
concatLexPar input = do
  let Right ets = runParser lexer "" input
  let Right r   = evalState (runParserT expression' "" ets) initialState
  putStrLn . drawTree $ r
