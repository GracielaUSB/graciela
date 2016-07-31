{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST.Expression            (Expression, Object)
import qualified AST.Expression            as E
import qualified AST.Object                as O
import           Graciela
import           Lexer
import           Limits
import           Location
import           MyParseError              as PE
import           Parser.Token
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Control.Monad             (void)
import           Control.Monad.Trans.State (evalState)
import           Data.Functor              (($>))
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, unpack)
import           Text.Megaparsec           (between, getPosition, runParser,
                                            runParserT, sepBy, sepBy1, some,
                                            try, (<|>))
import           Text.Megaparsec.Expr      (Operator (..), makeExprParser)
--------------------------------------------------------------------------------

object :: Graciela Object
object = makeExprParser object' ops
  where
    object' = parens object <|> variable

    variable = do
      from <- getPosition
      name <- identifier
      to   <- getPosition
      pure O.Object
        { O.loc     = Location (from, to)
        , O.objType = GInt
        , O.obj'    = O.Variable
          { O.name
          }
        }

    ops :: [[ Operator Graciela Object ]]
    ops =
      [ {-Level 0-}
        [ Postfix (foldr1 (flip (.)) <$> some (subindex <|> field))
        ]
      , {-Level 2-}
        [ Prefix  (foldr1 (.) <$> some pointer)
        ]
      ]

    subindex :: Graciela (Object -> Object)
    subindex = do
      e <- brackets expression
      to <- getPosition
      pure $ \o @ O.Object { O.loc = Location (from, _) } ->
        O.Object
          { O.loc = Location (from, to)
          , O.objType = GInt
          , O.obj' = O.Index o e
          }

    field :: Graciela (Object -> Object)
    field = do
      match TokDot
      fieldName <- identifier
      to <- getPosition
      pure $ \o @ O.Object { O.loc = Location (from, _) } ->
        O.Object
          { O.loc = Location (from, to)
          , O.objType = GInt
          , O.obj' = O.Member o fieldName
          }

    pointer :: Graciela (Object -> Object)
    pointer = do
      from <- getPosition
      match TokTimes $> \o @ O.Object { O.loc = Location (_, to) } ->
        O.Object
          { O.loc = Location (from, to)
          , O.objType = GInt
          , O.obj' = O.Deref o
          }














expression :: Graciela Expression
expression = makeExprParser term operator
-- expression = do
--   from <- getPosition
--   e <- expression'
--   to <- getPosition
--   return e
--   where
--     expression' = makeExprParser term operator


-- quantification :: Graciela AST
-- quantification = do
--   from <- getPosition
--   match TokLeftPercent
--
--   q <- quantifier
--   (var, t) <- declaration
--   match TokPipe
--   range <- expression'
--   match TokPipe
--   expr <- expression'
--
--   match TokRightPercent
--   to <- getPosition
--
--   return AST
--     { posFrom
--     , posTo
--     , astType = GBool
--     , ast' = Quantification
--       { qOp      = q
--       , qVar     = var
--       , qVarType = t
--       , qRange   = range
--       , qCond    = EmptyAST
--       , qBody    = expr
--       }
--     }
--
--   where
--     quantifier =  (match TokExist  $> Exists)
--               <|> (match TokMax    $> Maximum)
--               <|> (match TokSigma  $> Summation)
--               <|> (match TokMin    $> Minimum)
--               <|> (match TokForall $> ForAll)
--               <|> (match TokPi     $> Product)
--               <|> (match TokCount  $> Count)
--
--     declaration = do
--       var <- identifier
--       match TokColon
--       t <- identifier
--       pure (var, t)
--
--
--
-- call :: Graciela AST
-- call = do
--   posFrom <- getPosition
--   name <- identifier
--   args <- parens (expression' `sepBy` match TokComma)
--   posTo <- getPosition
--   return AST
--     { posFrom
--     , posTo
--     , astType = GBool
--     , ast' = FunctionCall
--       { fname = identifier
--       , astST = empty from
--       , args  = args
--       }
--     }



-- if' :: Graciela AST -> Graciela AST
-- if' element = between (match TokIf) (match TokFi) contents
--   where
--     contents = Node "If" <$> line `sepBy1` match TokSepGuards
--     line = do
--       expr <- expression
--       match TokArrow
--       elem' <- element
--       return $ Node "Guard"
--         [ Node "Condition" [expr]
--         , Node "Result" [elem']
--         ]





term :: Graciela Expression
term =  -- parens expression
    -- <|> try call
     variable <$> object
    <|> wrap boolLit    E.BoolLit  GBool
    <|> wrap integerLit E.IntLit   GInt
    <|> wrap floatLit   E.FloatLit GFloat
    <|> wrap charLit    E.CharLit  GChar
    -- <|> quantification
    -- <|> if' expression
  where
    variable :: Object -> Expression
    variable o @ O.Object { O.loc, O.objType } =
      E.Expression
        { E.loc
        , E.expType = objType
        , E.exp' = E.Obj o
        }

    wrap :: Graciela a -> (a -> E.Expression') -> Type -> Graciela Expression
    wrap litp expr t = do
      from <- getPosition
      lit <- litp
      to <- getPosition
      pure E.Expression
        { E.loc     = Location (from, to)
        , E.expType = t
        , E.exp'    = expr lit
        }

ii :: (Type, Type)
ii =  (GInt, GInt)
ff :: (Type, Type)
ff =  (GFloat, GFloat)
bb :: (Type, Type)
bb =  (GBool, GBool)

iii :: (Type, Type, Type)
iii =  (GInt, GInt, GInt)
fff :: (Type, Type, Type)
fff =  (GFloat, GFloat, GFloat)
bbb :: (Type, Type, Type)
bbb =  (GBool, GBool, GBool)
iib :: (Type, Type, Type)
iib =  (GInt, GInt, GBool)
ffb :: (Type, Type, Type)
ffb =  (GFloat, GFloat, GBool)


bchecked :: [(Type, Type, Type)]
         -> E.BinaryOperator
         -> Expression -> Expression -> Expression
bchecked ts binOp
  lexpr @ E.Expression { E.expType = ltype }
  rexpr @ E.Expression { E.expType = rtype }
  = foldr aux bad ts
      where
        aux (left, right, ret) E.BadExpression {}
          | ltype == left && rtype == right =
            E.Expression
              { E.loc = location
              , E.expType = ret
              , E.exp' = E.Binary
                { E.binOp
                , E.lexpr
                , E.rexpr
                }
              }
          | otherwise = bad
        aux _ goodExpr = goodExpr
        location = Location (E.from lexpr, E.to rexpr)
        bad = E.BadExpression { E.loc = location }
bchecked _ _ l r =
  E.BadExpression { E.loc = Location (E.from l, E.to r) }


operator :: [[ Operator Graciela Expression ]]
operator =
  [ {-Level 2-}
  --   [ Prefix (match TokNot        $> \x -> Node "not" [x])
  --   , Prefix (match TokMinus      $> \x -> Node "minus" [x])
  --   ]
  -- , {-Level 3-}
  --   [ InfixR (match TokPower      $> \x y -> Node "^" [x,y])
  --   ]
  -- , {-Level 4-}
  --   [ InfixL (match TokTimes      $> \x y -> Node "*" [x,y])
  --   , InfixL (match TokDiv        $> \x y -> Node "/" [x,y])
  --   , InfixL (match TokMod        $> \x y -> Node "mod" [x,y])
  --   ]
  -- , {-Level 5-}
    [ InfixL (match TokPlus       $> bchecked [iii,fff] E.Plus)
    , InfixL (match TokMinus      $> bchecked [iii,fff] E.BMinus)
    ]
  -- , {-Level 6-}
  --   [ InfixL (match TokMax        $> \x y -> Node "max" [x,y])
  --   , InfixL (match TokMin        $> \x y -> Node "min" [x,y])
  --   ]
  -- , {-Level 7-}
  --   [ InfixN (match TokLT         $> \x y -> Node "<" [x,y])
  --   , InfixN (match TokLE         $> \x y -> Node "<=" [x,y])
  --   , InfixN (match TokGT         $> \x y -> Node ">" [x,y])
  --   , InfixN (match TokGE         $> \x y -> Node ">=" [x,y])
  --   ]
  -- , {-Level 8-}
  --   [ InfixN (match TokAEQ        $> \x y -> Node "==" [x,y])
  --   , InfixN (match TokANE        $> \x y -> Node "!=" [x,y])
  --   ]
  -- , {-Level 9-}
  --   [ InfixR (match TokAnd        $> \x y -> Node "/\\" [x,y])
  --   ]
  -- , {-Level 10-}
  --   [ InfixR (match TokOr         $> \x y -> Node "\\/" [x,y])
  --   ]
  -- , {-Level 11-}
  --   [ InfixR (match TokImplies    $> \x y -> Node "==>" [x,y])
  --   , InfixL (match TokConsequent $> \x y -> Node "<==" [x,y])
  --   ]
  -- , {-Level 12-}
  --   [ InfixN (match TokBEQ        $> \x y -> Node "===" [x,y])
  --   , InfixN (match TokBNE        $> \x y -> Node "!==" [x,y])
  --   ]
  ]


concatLexPar :: Text -> IO ()
concatLexPar input = do
  let Right ets = runParser lexer "" input
  let Right r   = evalState (runParserT expression "" ets) initialState
  putStrLn . drawTree . toTree $ r
