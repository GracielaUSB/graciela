{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST.Expression            hiding (inner, loc)
import qualified AST.Expression            as E (inner, loc)
import           AST.Object                hiding (inner, loc, name)
import qualified AST.Object                as O (inner, loc, name)
import           Entry                     (Entry' (..), Entry'' (..),
                                            Value (..), info, varType)
import           Graciela
import           Lexer
import           Limits
import           Location
import           MyParseError              as PE
import           Parser.ExprM              (Operator (..), makeExprParser)
import           Parser.Token
import           SymbolTable               (closeScope, insertSymbol, lookup,
                                            openScope)
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Control.Exception         (assert)
import           Control.Lens              (makeLenses, use, (%=), (<&>), (^.))
import           Control.Monad             (void, when, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, get, gets,
                                            modify, put, runState)
import           Data.Functor              (($>))
import qualified Data.Map                  as Map (lookup)
import           Data.Monoid               ((<>))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, singleton)
import           Data.Text                 (Text, pack, unpack)
import           Prelude                   hiding (Ordering (..), lookup)
import           Text.Megaparsec           (between, getPosition, runParser,
                                            runParserT, sepBy, sepBy1, some,
                                            try, (<|>))
--------------------------------------------------------------------------------
import           Control.Lens              ((&~))
import           Debug.Trace

expression :: Graciela Expression
expression = evalStateT expr []

data ProtoRange
  = ProtoVar                  -- ^ The associated expression is the
                              -- variable of interest
  | ProtoQRange  QRange       -- ^ A valid QRange has been formed
  | ProtoLow     Expression   -- ^ A lower bound has been found
  | ProtoHigh    Expression   -- ^ An upper bound has been found
  | ProtoNothing              -- ^ No manner of range has been formed

newtype Taint = Taint { getTaint :: Bool } -- A MetaExpr is `tainted` when it
                                           -- contains a quantified (dummy)
                                           -- variable. This means it cannot
                                           -- be used as a range limit.
  deriving (Eq, Ord, Show)

instance Monoid Taint where
  mempty = Taint False
  Taint x `mappend` Taint y = Taint (x || y)

type MetaExpr = (Expression, ProtoRange, Taint)

type GracielaRange = StateT [ Text ] Graciela


expr :: GracielaRange Expression
expr = (\(e,_,_) -> e) <$> metaexpr


metaexpr :: GracielaRange MetaExpr
metaexpr = makeExprParser term operator


term :: GracielaRange MetaExpr
term =  parens metaexpr
 -- <|> try call -- TODO: function calling, depends on ST design
    <|> variable
    <|> (get >>= bool)
    <|> basicLit integerLit       IntLit        GInt
    <|> basicLit floatLit         FloatLit      GFloat
    <|> basicLit charLit          CharLit       GChar
    <|> setLit   TokEmptySet      EmptySet      GSet
    <|> setLit   TokEmptyMultiset EmptyMultiset GMultiset
    <|> quantification
    <|> ifExp

  where
    bool :: [Text] -> GracielaRange MetaExpr
    bool [] = basicLit boolLit BoolLit GBool
    bool _  = do
      from <- getPosition
      lit <- boolLit
      to <- getPosition

      let loc = Location (from, to)

      pure $ if lit
        then let expr = Expression
                  { E.loc
                  , expType = GBool
                  , exp' = BoolLit lit }
        in (expr, ProtoNothing, Taint False)

        else let expr = Expression
                  { E.loc
                  , expType = GBool
                  , exp' = ESkip }
          in (expr, ProtoQRange EmptyRange, Taint False)


    basicLit :: Graciela a -> (a -> Expression') -> Type
             -> GracielaRange MetaExpr
    basicLit litp expr' t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition

      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = t
          , exp'    = expr' lit }

      pure (expr, ProtoNothing, Taint False)

    setLit :: Token -> Expression' -> (Type -> Type)
           -> GracielaRange MetaExpr
    setLit tok e t = do
      from <- getPosition
      match tok
      to <- getPosition

      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = t GAny
          , exp'    = e }

      pure (expr, ProtoNothing, Taint False)


variable :: GracielaRange MetaExpr
variable = do
  from <- getPosition
  name <- identifier
  to <- getPosition

  st <- lift (use symbolTable)

  let loc = Location (from, to)

  case name `lookup` st of

    Left _ -> do
      lift . syntaxError . (`CustomError` loc) $
        "Variable `" <> unpack name <> "` not defined in this scope."

      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right entry -> case entry^.info of

      Var { _varType } -> do
        let expr = Expression
              { E.loc
              , expType = _varType
              , exp' = Obj
                { theObj = Object
                  { O.loc
                  , objType = _varType
                  , obj' = Variable
                    { O.name }}}}

        rangevars <- get

        let protorange = case rangevars of
              [] -> ProtoNothing
              _  -> if name == head rangevars
                then ProtoVar
                else ProtoNothing

        let taint = case rangevars of
              [] -> Taint False
              _  -> if name `elem` rangevars
                then Taint True
                else Taint False

        pure (expr, protorange, taint)

      Const { _constType, _constValue } ->
        let
          exp' = case _constValue of
            B b -> BoolLit  b
            C c -> CharLit  c
            I i -> IntLit   i
            F f -> FloatLit f
            None -> error "internal error: unassigned constant"
          expr = Expression
            { E.loc
            , expType = _constType
            , exp' }
        in pure (expr, ProtoNothing, Taint False)

      _ -> pure (BadExpression { E.loc }, ProtoNothing, Taint False)


quantification :: GracielaRange MetaExpr
quantification = do
  from <- getPosition
  match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, t) <- declaration
  match TokPipe

  modify (var:)
  (cond, protorange, taint0) <- metaexpr
  let rloc = E.loc cond
  modify tail

  case cond of
    Expression {} ->
      case protorange of
        ProtoVar ->
          lift . syntaxError . (`CustomError` rloc) $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoNothing       ->
          lift . syntaxError . (`CustomError` rloc) $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoLow         _ ->
          lift . syntaxError $ (`CustomError` rloc)
            "Bad quantification range. No upper bound was given."
        ProtoHigh        _ ->
          lift . syntaxError $ (`CustomError` rloc)
            "Bad quantification range. No lower bound was given."
        ProtoQRange qrange ->
          pure ()
    BadExpression {} ->
      pure ()

  match TokPipe

  (body, _, taint1) <- metaexpr

  case body of
    Expression { expType = bType, E.loc = bloc } ->
      when (bType /= GError && not (bType =:= allowedBType)) .
        lift . syntaxError . (`CustomError` bloc) $
          "Bad quantification body. Body must be " <> show allowedBType <> "."
    _ -> pure ()


  match TokRightPercent
  to <- getPosition
  lift $ symbolTable %= closeScope to

  let loc = Location (from, to)

  let taint = taint0 <> taint1

  let expr =
        case body of
          BadExpression {} ->
            BadExpression { E.loc }
          Expression { expType = bType } ->
            case protorange of
              ProtoQRange qRange -> if bType =:= allowedBType
                then Expression
                  { E.loc
                  , expType = GBool
                  , exp' = Quantification
                    { qOp      = q
                    , qVar     = var
                    , qVarType = t
                    , qRange
                    , qCond    = cond
                    , qBody    = body }}
                else BadExpression
                  { E.loc }
              _ -> BadExpression
                { E.loc }

  pure (expr, ProtoNothing, taint)

  where
    numeric = GOneOf [GBool, GChar, GInt, GFloat]
    quantifier =  (match TokForall $> (ForAll,    GBool))
              <|> (match TokExist  $> (Exists,    GBool))
              <|> (match TokPi     $> (Product,   numeric))
              <|> (match TokSigma  $> (Summation, numeric))
              <|> (match TokMax    $> (Maximum,   numeric))
              <|> (match TokMin    $> (Minimum,   numeric))
              <|> (match TokCount  $> (Count,     GBool))

    quantifiableTypes = GOneOf [ GInt, GChar ]

    declaration = do
      from <- getPosition

      var <- identifier
      match TokColon
      tname <- identifier

      to <- getPosition

      let loc = Location (from, to)

      typeEntry <- Map.lookup tname <$> lift (use typesTable)

      case typeEntry of
        Nothing    -> do
          lift . syntaxError $
            CustomError ("type `" <> unpack tname <> "` does not exist" ) loc
          pure (var, GError)

        Just (t,_) ->
          if t =:= quantifiableTypes
            then do
              lift $ symbolTable %= insertSymbol var Entry
                { _entryName = var
                , _loc = Location (from, to)
                , _info = Var
                  { _varType  = t
                  , _varValue = Nothing }}
              pure (var, t)
            else do
              lift . syntaxError $
                CustomError
                  ("type `" <> unpack tname <> "` is not quantifiable" ) loc
              pure (var, GError)


ifExp :: GracielaRange MetaExpr
ifExp = do
  from <- getPosition
  match TokIf

  -- The StateT on top of Graciela allows us to manage
  -- a state local to this level of abstraction.
  if' <- evalStateT guards (Just (GAny, Seq.empty, Taint False))

  match TokFi
  if' from <$> getPosition

  where
    guards = do
      -- We run `line` for each "a -> b" pair in the If metaexpr,
      -- and then we extract the final set of guards
      line `sepBy1` match TokSepGuards
      st <- get


      pure $ \from to ->
        let
          -- Taint propagates usually
          taint = case st of
            Nothing      -> Taint False
            Just (_,_,t) -> t

          -- If there were errors, we return a BadExpression, otherwise,
          -- it's the conditional we were looking for
          expr = case st of
            Nothing ->
              BadExpression
                { E.loc = Location (from, to) }
            Just (t, gs, _) ->
              Expression
                { E.loc = Location (from, to)
                , expType = t
                , exp' = EConditional
                  { eguards = gs }}
        in
          (expr, ProtoNothing, taint)

    line = do
      st <- get
      -- We first save the state of the previous guards

      (lhs,_,taint0) <- lift metaexpr
      -- We take the left hand side of the guard,
      -- and three things could have happened,

      case lhs of
        Expression { expType = GBool } ->
          -- 1. We have a good boolean expression, which is ideal
          pure ()

        Expression { E.loc } -> do
          -- 2. We have a good expression which isn't boolean, so we
          -- report the error and clear the previous guards
          put Nothing
          lift . lift . syntaxError $
            CustomError "bad left side in conditional expression" loc

        BadExpression {} ->
          -- 3. We have a bad expression, which means there was an error
          -- before which we can only propagate
          put Nothing

      match TokArrow

      (rhs,_,taint1) <- lift metaexpr
      -- We take the right hand side of the guard,
      -- and four things could have happened,

      case rhs of
        Expression { E.loc = rloc, expType } ->
          case st of
            Just (currT, currGs, currTaint) ->
              -- There had been no errors before this guard and either
              if expType =:= currT
                then
                  -- 1. We have a good expression whose type matches the
                  -- type of previous guards. (In the case of the first guard,
                  -- the match is done against GAny, i.e., any type will
                  -- match). In this case, we add this guard to the list
                  -- and we propagate taint.
                  put $ Just
                    ( expType
                    , currGs |> (lhs, rhs)
                    , taint0 <> taint1 <> currTaint )

                else do
                  -- 2. We have a good expression whose type didn't match, so
                  -- we report the error and clear the previous guards.
                  put Nothing
                  lift . lift . syntaxError $
                    CustomError "bad right side in conditional expression" rloc

            Nothing ->
              -- 3. There had been an error before this guard, in which case
              -- we can't tell what the type is supposed to be, so we just
              -- propagate the error.
              pure ()

        BadExpression {} ->
          -- 4. We have a bad expression, which means there was an error
          -- deeper in the expression which we can only propagate
          put Nothing


operator :: [[ Operator GracielaRange MetaExpr ]]
operator =
  [ {-Level 0-}
    [ Postfix (foldr1 (>=>) <$> some subindex) ]
 -- , Postfix (foldr1 (>=>) <$> some field) ] -- TODO: Field access, depends on ST design
  , {-Level 1-}
    [ Prefix  (foldr1 (>=>) <$> some deref) ]
  , {-Level 2-}
    [ Prefix (match TokNot        <&> unary booleanU    Not   )
    , Prefix (match TokMinus      <&> unary arithmeticU UMinus)
    , Prefix (match TokAbs        <&> unary arithmeticU Abs   )
    , Prefix (match TokSqrt       <&> unary arithmeticU Sqrt  ) ]
  , {-Level 3-}
    [ InfixR (match TokPower      <&> binary arithmetic Power) ]
  , {-Level 4-}
    [ InfixL (match TokTimes      <&> binary arithmetic Times)
    , InfixL (match TokDiv        <&> binary arithmetic Div  )
    , InfixL (match TokMod        <&> binary arithmetic Mod  ) ]
  , {-Level 5-}
    [ InfixL (match TokPlus       <&> binary arithmetic Plus  )
    , InfixL (match TokMinus      <&> binary arithmetic BMinus) ]
  , {-Level 6-}
    [ InfixL (match TokMax        <&> binary arithmetic Max)
    , InfixL (match TokMin        <&> binary arithmetic Min) ]
  , {-Level 7-}
    [ InfixN (match TokElem       <&> membership)
    , InfixN (match TokNotElem    <&> binary elemSet NotElem)
    , InfixN (match TokLT         <&> comparison LT)
    , InfixN (match TokLE         <&> comparison LE)
    , InfixN (match TokGT         <&> comparison GT)
    , InfixN (match TokGE         <&> comparison GE) ]
  , {-Level 8-}
    [ InfixN (match TokAEQ        <&> pointRange)
    , InfixN (match TokANE        <&> binary equality ANE) ]
  , {-Level 9-}
    [ InfixR (match TokAnd        <&> conjunction) ]
  , {-Level 10-}
    [ InfixR (match TokOr         <&> binary boolean Or) ]
  , {-Level 11-}
    [ InfixR (match TokImplies    <&> binary boolean Implies   )
    , InfixL (match TokConsequent <&> binary boolean Consequent) ]
  , {-Level 12-}
    [ InfixN (match TokBEQ        <&> binary boolean BEQ)
    , InfixN (match TokBNE        <&> binary boolean BNE) ]
  ]


subindex :: GracielaRange (MetaExpr -> GracielaRange MetaExpr)
subindex = do
  from' <- getPosition
  (sub, _, taint0) <- brackets metaexpr
  to <- getPosition

  case sub of
    BadExpression {} -> pure $ badSubindex to

    Expression { expType } ->
      case expType of
        GInt ->
          pure $ \(expr, _, taint1) -> case expr of
            Expression
              { E.loc = Location (from, _)
              , expType = GArray { arrayType }
              , exp' = Obj o } ->
                let
                  taint = taint0 <> taint1
                  expr = Expression
                    { E.loc = Location (from, to)
                    , expType = arrayType
                    , exp' = Obj
                      { theObj = Object
                        { O.loc = Location (from, to)
                        , objType = arrayType
                        , obj' = Index
                          { O.inner = o
                          , index = sub }}}}
                in pure (expr, ProtoNothing, taint)

            BadExpression { E.loc = Location (from, _) } ->
              let loc = Location (from, to)
              in pure (BadExpression { E.loc }, ProtoNothing, Taint False)

            e -> do
              let Location (from, _) = E.loc e
              let loc = Location (from, to)

              lift . syntaxError $
                CustomError "Cannot subindex non-array." loc

              pure (BadExpression { E.loc }, ProtoNothing, Taint False)

        _ -> do
          let subloc = Location (from', to)
          lift . syntaxError $
            CustomError "Bad subindex. Must be integer expression." subloc
          pure $ badSubindex to

    where
      badSubindex to (e,_,_) = do
        let Location (from, _) = E.loc e
        let loc = Location (from, to)
        pure (BadExpression { E.loc }, ProtoNothing, Taint False)


deref :: GracielaRange (MetaExpr -> GracielaRange MetaExpr)
deref = do
  from <- getPosition
  match TokTimes

  pure $ \(expr, _, taint) -> case expr of
    Expression
      { E.loc = Location (_, to)
      , expType = GPointer pointerType
      , exp' = Obj o } ->
        let expr = Expression
              { E.loc = Location (from, to)
              , expType = pointerType
              , exp' = Obj
                { theObj = Object
                  { O.loc = Location (from, to)
                  , objType = pointerType
                  , obj' = Deref
                    { O.inner = o }}}}
        in pure (expr, ProtoNothing, taint)

    BadExpression { E.loc = Location (_, to) } ->
      let loc = Location (from, to)
      in pure (BadExpression { E.loc }, ProtoNothing, Taint False)


    e -> do
      let Location (_, to) = E.loc e
      let loc = Location (from, to)

      lift . syntaxError $
        CustomError "Cannot deref non-pointer." loc

      pure (BadExpression { E.loc }, ProtoNothing, Taint False)


type UnaryOpType = Type -> Either String Type

arithmeticU :: UnaryOpType
arithmeticU GInt   = Right GInt
arithmeticU GChar  = Right GChar
arithmeticU GFloat = Right GFloat
arithmeticU _      = Left $
  show GInt   <> ", " <>
  show GChar  <> ", or " <>
  show GFloat

booleanU :: UnaryOpType
booleanU GBool = Right GBool
booleanU _     = Left $ show GBool


unary :: UnaryOpType -> UnaryOperator -> Location
      -> MetaExpr -> GracielaRange MetaExpr
unary tf unOp
  opLoc @ (Location (from,_))
  (i @ Expression { expType = itype }, _, taint)
  = case tf itype of
    Left expected -> do
      let loc = Location (from, to i)
      lift . syntaxError $ CustomError
        ("Operator `" <> show unOp <> "` at " <> show opLoc <> " expected an \
          \expression of type " <> expected <> ", but received " <>
          show itype <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, taint)
    Right ret ->
      let expr = Expression
            { E.loc = Location (from, to i)
            , expType = ret
            , exp' = Unary
              { unOp
              , E.inner = i }}
      in pure (expr, ProtoNothing, taint)

unary _ _ (Location (from,_)) (i,_,taint) =
  let loc = Location (from, to i)
  in pure (BadExpression { E.loc }, ProtoNothing, taint)


type BinaryOpType = Type -> Type -> Either String Type

arithmetic :: BinaryOpType
arithmetic GInt   GInt   = Right GInt
arithmetic GChar  GChar  = Right GChar
arithmetic GFloat GFloat = Right GFloat
arithmetic _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

equality :: BinaryOpType
equality GInt   GInt   = Right GBool
equality GChar  GChar  = Right GBool
equality GFloat GFloat = Right GBool
equality _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

elemSet :: BinaryOpType
elemSet t1 (GSet t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GSet t2)
elemSet t1 (GMultiset t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GMultiset t2)
elemSet _ _ = Left $
  show (GUnsafeName "t", GSet      (GUnsafeName "t")) <> ", or " <>
  show (GUnsafeName "t", GMultiset (GUnsafeName "t"))

boolean :: BinaryOpType
boolean GBool GBool = Right GBool
boolean _     _     = Left $
  show (GBool, GBool)


binary :: BinaryOpType -> BinaryOperator -> Location
       -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
binary tf binOp opLoc
  (l @ Expression { expType = ltype }, _, ltaint)
  (r @ Expression { expType = rtype }, _, rtaint)
  = case tf ltype rtype of

    Left expected -> do
      let loc = Location (from l, to r)
      lift . syntaxError $ CustomError
        ("Operator `" <> show binOp <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right ret ->
      let
        taint = ltaint <> rtaint
        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = ret
          , exp' = Binary
            { binOp
            , lexpr = l
            , rexpr = r }}
      in pure (expr, ProtoNothing, taint)

binary _ _ _ (l,_,_) (r,_,_) =
  let expr = BadExpression { E.loc = Location (from l, to r) }
  in pure (expr, ProtoNothing, Taint False)


membership :: Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
membership opLoc
  (l @ Expression { expType = ltype }, ProtoVar, ltaint)
  (r @ Expression { expType = rtype }, _, Taint False)
  = case elemSet ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      lift . syntaxError $ CustomError
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = GBool
          , exp' = ESkip }
      in pure (expr, ProtoQRange (SetRange r), Taint False)

    Right _ -> error "impossible"

membership opLoc l r = binary elemSet Elem opLoc l r


comparison :: BinaryOperator -> Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
comparison binOp opLoc
  (l @ Expression { expType = ltype }, _, Taint False)
  (r @ Expression { expType = rtype }, ProtoVar, _)
  = case equality ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        lift . syntaxError $ CustomError
          ("Operator `" <> show binOp <> "` at " <> show opLoc <> " expected\
            \ two expressions of types " <> expected <> ", but received " <>
            show (ltype, rtype) <> ".") loc
        pure (BadExpression { E.loc }, ProtoNothing, Taint False)

      Right GBool ->
        let
          range = case binOp of
            LT -> ProtoLow  (succ' l)
            LE -> ProtoLow  l
            GT -> ProtoHigh (pred' l)
            GE -> ProtoHigh l
          expr = Expression
            { E.loc   = Location (from l, to r)
            , expType = GBool
            , exp'    = ESkip }
        in pure (expr, range, Taint True)

      Right _ ->
        error "internal error: impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, exp' } =
      Expression { E.loc, expType, exp' = Unary op e }

comparison binOp opLoc
  (l @ Expression { expType = ltype }, ProtoVar, _)
  (r @ Expression { expType = rtype }, _, Taint False)
  = case equality ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        lift . syntaxError $ CustomError
          ("Operator `" <> show binOp <> "` at " <> show opLoc <> " expected\
            \ two expressions of types " <> expected <> ", but received " <>
            show (ltype, rtype) <> ".") loc
        pure (BadExpression { E.loc }, ProtoNothing, Taint False)

      Right GBool ->
        let
          range = case binOp of
            LT -> ProtoHigh (pred' r)
            LE -> ProtoHigh r
            GT -> ProtoLow  (succ' r)
            GE -> ProtoLow  r
          expr = Expression
            { E.loc   = Location (from l, to r)
            , expType = GBool
            , exp'    = ESkip }
        in pure (expr, range, Taint True)

      Right _ ->
        error "internal error: impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, exp' } =
      Expression { E.loc, expType, exp' = Unary op e }

comparison binOp opLoc l r = binary equality binOp opLoc l r


pointRange :: Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
pointRange opLoc
  (l @ Expression { expType = ltype }, ProtoVar, ltaint)
  (r @ Expression { expType = rtype }, _, Taint False)
  = case equality ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      lift . syntaxError $ CustomError
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = GBool
          , exp' = ESkip }
      in pure (expr, ProtoQRange (PointRange r), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc
  (l @ Expression { expType = rtype }, _, Taint False)
  (r @ Expression { expType = ltype }, ProtoVar, ltaint)
  = case equality ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      lift . syntaxError $ CustomError
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = GBool
          , exp' = ESkip }
      in pure (expr, ProtoQRange (PointRange l), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc l r = binary elemSet Elem opLoc l r


conjunction :: Location
            -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
conjunction opLoc
  (l @ Expression { expType = ltype, exp' = lexp' }, lproto, ltaint)
  (r @ Expression { expType = rtype, exp' = rexp' }, rproto, rtaint)
  = if ltype == rtype && ltype == GBool
    then do
      varname <- gets head
      let
        loc = Location (from l, to r)
        taint = ltaint <> rtaint
        (exp', range) = case (lproto, rproto) of
          (ProtoVar, proto) -> error "internal error: boolean ProtoVar"
          (proto, ProtoVar) -> error "internal error: boolean ProtoVar"

          (q @ (ProtoQRange EmptyRange), _) ->
            (ESkip, q)
          (_, q @ (ProtoQRange EmptyRange)) ->
            (ESkip, q)

          (ProtoNothing, proto) ->
            (lexp', proto)
          (proto, ProtoNothing) ->
            (rexp', proto)

          (ProtoLow low, ProtoHigh high) ->
            (ESkip, ProtoQRange (ExpRange low high))
          (ProtoHigh high, ProtoLow low) ->
            (ESkip, ProtoQRange (ExpRange low high))
          (ProtoLow  llow,  ProtoLow  rlow ) ->
            (ESkip, ProtoLow  (joinProtos Max llow rlow))
          (ProtoHigh lhigh, ProtoHigh rhigh) ->
            (ESkip, ProtoHigh (joinProtos Min lhigh rhigh))

          (ProtoQRange l @ ExpRange {}, ProtoQRange r @ ExpRange {}) ->
            (ESkip, joinExpRanges l r)

          (ProtoQRange l @ ExpRange {}, r @ ProtoLow {}) ->
            (ESkip, joinExpRangeProto l r)
          (ProtoQRange l @ ExpRange {}, r @ ProtoHigh {}) ->
            (ESkip, joinExpRangeProto l r)
          (l @ ProtoLow {} , ProtoQRange r @ ExpRange {}) ->
            (ESkip, joinExpRangeProto r l)
          (l @ ProtoHigh {}, ProtoQRange r @ ExpRange {}) ->
            (ESkip, joinExpRangeProto r l)

          (point @ (ProtoQRange PointRange {}), proto) ->
            (rebuild varname proto, point)
          (proto, point @ (ProtoQRange PointRange {})) ->
            (rebuild varname proto, point)

          (set @ (ProtoQRange SetRange {}), proto) ->
            (rebuild varname proto, set)
          (proto, set @ (ProtoQRange SetRange {})) ->
            (rebuild varname proto, set)


      pure (Expression { E.loc, expType = GBool, exp' }, range, taint)

    else do
      let loc = Location (from l, to r)
      lift . syntaxError $ CustomError
        ("Operator `" <> show And <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> show (GBool, GBool) <>
          ", but received " <> show (ltype, rtype) <> ".") loc
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    where
      joinProtos binOp l r = Expression
        { E.loc   = Location (from l, to r)
        , expType = expType l
        , exp' = Binary binOp l r }

      joinExpRanges l ExpRange { low, high }
        = let ProtoQRange l' = joinExpRangeProto l (ProtoLow low)
          in  joinExpRangeProto l' (ProtoHigh high)

      joinExpRangeProto
        ExpRange { low = elow, high }
        (ProtoLow plow)
        = let
            t = expType elow
            low = Expression
              { E.loc = Location
                (from elow `min` from plow, to elow `max` to plow)
              , expType = t
              , exp' = Binary
                { binOp = Max
                , lexpr = elow
                , rexpr = plow }}

          in ProtoQRange ExpRange { low, high }

      joinExpRangeProto
        ExpRange { low, high = ehigh }
        (ProtoHigh phigh)
        = let
            t = expType low
            high = Expression
              { E.loc = Location
                (from ehigh `min` from phigh, to ehigh `max` to phigh)
              , expType = t
              , exp' = Binary
                { binOp = Min
                , lexpr = ehigh
                , rexpr = phigh }}

          in ProtoQRange ExpRange { low, high }

      rebuild varname (ProtoQRange (SetRange e)) =
        let
          t = case expType e of
            GSet a      -> a
            GMultiset a -> a
          lexpr = obj varname t
        in Binary
          { binOp = Elem
          , lexpr
          , rexpr = e }

      rebuild varname (ProtoQRange (PointRange rexpr)) =
        let
          t = expType rexpr
          lexpr = obj varname t
        in Binary
          { binOp = AEQ
          , lexpr
          , rexpr }

      rebuild varname (ProtoQRange (ExpRange l h)) =
        let
          lexpr' = rebuild varname (ProtoLow l)
          rexpr' = rebuild varname (ProtoHigh h)
        in Binary
          { binOp = And
          , lexpr = Expression
            { E.loc = Rearranged
            , expType = GBool
            , exp' = lexpr' }
          , rexpr = Expression
            { E.loc = Rearranged
            , expType = GBool
            , exp' = rexpr' }}

      rebuild varname (ProtoLow l) =
        let
          t = expType l
          rexpr = obj varname t
        in Binary
          { binOp = LT
          , lexpr = l
          , rexpr }

      rebuild varname (ProtoHigh h) =
        let
          t = expType l
          lexpr = obj varname t
        in Binary
          { binOp = LT
          , lexpr
          , rexpr = h }

      obj name expType =
        Expression
          { E.loc = Rearranged
          , expType
          , exp' = Obj
            { theObj = Object
              { O.loc = Rearranged
              , objType = expType
              , obj' = Variable
                { O.name }}}}

conjunction opLoc (l,_,_) (r,_,_) =
  let expr = BadExpression { E.loc = Location (from l, to r) }
  in pure (expr, ProtoNothing, Taint False)


--------------------------------------------------------------------------------
testExpr :: String -> IO ()
testExpr strinput = do
  let input = pack strinput
  let Right ets = runParser lexer "" input
  let init' = initialState &~ do
        symbolTable %= openScope (SourcePos "" (unsafePos 4) (unsafePos 10))
        symbolTable %= insertSymbol (pack "a") Entry
          { _entryName  = pack "a"
          , _loc        = Location (SourcePos "" (unsafePos 2) (unsafePos 2), SourcePos "" (unsafePos 2) (unsafePos 20))
          , _info       = Var
            { _varType  = GArray 10 GInt
            , _varValue = Nothing }}
        symbolTable %= insertSymbol (pack "b") Entry
          { _entryName  = pack "b"
          , _loc        = Location (SourcePos "" (unsafePos 3) (unsafePos 3), SourcePos "" (unsafePos 3) (unsafePos 20))
          , _info       = Var
            { _varType  = GArray 10 (GArray 10 GInt)
            , _varValue = Nothing }}
        symbolTable %= insertSymbol (pack "c") Entry
          { _entryName  = pack "c"
          , _loc        = Location (SourcePos "" (unsafePos 4) (unsafePos 4), SourcePos "" (unsafePos 4) (unsafePos 20))
          , _info       = Var
            { _varType  = GPointer GInt
            , _varValue = Nothing }}
  let (r, s) = runState (runParserT expression "" ets) init'
  case r of
    Right r' -> do
      putStrLn . drawTree . toTree $ r'
      case r' of
        Expression { expType } -> print expType
        _ -> pure ()
      mapM_ print (s ^. synErrorList)
    Left _ -> mapM_ print (s ^. synErrorList)
