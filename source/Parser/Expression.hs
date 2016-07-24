module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST
import           Graciela
import           Lexer
import           Limits
import           Location
import           MyParseError              as PE
import           Parser.Token
import           ParserState               as PS
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad             (void)
import           Control.Monad.Trans.State (evalState)
import           Data.Functor              (($>))
import           Data.Text                 (Text, pack)
import           Text.Megaparsec           (between, runParser, runParserT,
                                            (<|>), some)
import           Text.Megaparsec.Expr      (Operator (..), makeExprParser)
--------------------------------------------------------------------------------

import Data.Tree


expression :: Graciela (Tree String)
expression = makeExprParser term operator


quantification = between (match TokLeftPercent) (match TokRightPercent) q
  where
    q = do
      void quantifier
      declaration
      match TokPipe
      expression
      match TokPipe
      expression


declaration = do
  void identifier
  match TokColon
  void identifier


quantifier :: Graciela OpQuant
quantifier =  (match TokExist  $> Exists)
          <|> (match TokMax    $> Maximum)
          <|> (match TokSigma  $> Summation)
          <|> (match TokMin    $> Minimum)
          <|> (match TokForall $> ForAll)
          <|> (match TokPi     $> Product)


parens :: Graciela a -> Graciela a
parens = between (match TokLeftPar) (match TokRightPar)


leaf x = Node x []


term :: Graciela (Tree String)
term =  parens expression
    <|> (leaf . show <$> boolLit )
    <|> value
    <|> (leaf . show <$> integerLit)
    <|> (leaf . show <$> floatLit)
    <|> (leaf . show <$> charLit)
    -- <|> quantification
  where
    value = makeExprParser value' ops
    value' =  parens value
          <|> (leaf . show <$> identifier)
    ops :: [[ Operator Graciela (Tree String) ]]
    ops =
      [ {-Level 0-}
        [ Prefix (foldr1 (.) <$> some pointer)
        , Postfix (do
            e <- subindex
            return (\x -> Node "sub" [x,e]))
        ]
      ]
    subindex =
      between (match TokLeftBracket) (match TokRightBracket) expression

    pointer = match TokTimes $> \x -> Node "pointer to" [x]

operator :: [[ Operator Graciela (Tree String) ]]
operator =
  [ {-Level 1-}
    [ Prefix (match TokNot        $> \x -> Node "not" [x])
    , Prefix (match TokMinus      $> \x -> Node "minus" [x])
    ]
  , {-Level 2-}
    [ InfixR (match TokPower      $> \x y -> Node "" [])
    ]
  , {-Level 3-}
    [ InfixL (match TokTimes      $> \x y -> Node "" [])
    , InfixL (match TokDiv        $> \x y -> Node "" [])
    , InfixL (match TokMod        $> \x y -> Node "" [])
    ]
  , {-Level 4-}
    [ InfixL (match TokPlus       $> \x y -> Node "" [])
    , InfixL (match TokMinus      $> \x y -> Node "" [])
    ]
  , {-Level 5-}
    [ InfixL (match TokMax        $> \x y -> Node "" [])
    , InfixL (match TokMin        $> \x y -> Node "" [])
    ]
  , {-Level 6-}
    [ InfixN (match TokLT         $> \x y -> Node "" [])
    , InfixN (match TokLE         $> \x y -> Node "" [])
    , InfixN (match TokGT         $> \x y -> Node "" [])
    , InfixN (match TokGE         $> \x y -> Node "" [])
    ]
  , {-Level 7-}
    [ InfixN (match TokAEQ        $> \x y -> Node "" [])
    , InfixN (match TokANE        $> \x y -> Node "" [])
    ]
  , {-Level 8-}
    [ InfixR (match TokAnd        $> \x y -> Node "" [])
    ]
  , {-Level 9-}
    [ InfixR (match TokOr         $> \x y -> Node "" [])
    ]
  , {-Level 10-}
    [ InfixR (match TokImplies    $> \x y -> Node "" [])
    , InfixL (match TokConsequent $> \x y -> Node "" [])
    ]
  , {-Level 11-}
    [ InfixN (match TokBEQ        $> \x y -> Node "" [])
    , InfixN (match TokBNE        $> \x y -> Node "" [])
    ]
  ]


concatLexPar :: Text -> IO ()
concatLexPar input = do
  let Right ets = runParser lexer "" input
  let Right r   = evalState (runParserT expression "" ets) initialState
  putStrLn . drawTree $ r
