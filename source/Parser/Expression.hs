{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST.Expression            hiding (inner, loc)
import qualified AST.Expression            as E (inner, loc)
import           AST.Object                hiding (inner, loc, name)
import qualified AST.Object                as O (inner, loc, name)
import           Entry                     (Entry' (..), Entry'' (..), info)
import           Error                     (Error (..))
import           Lexer
import           Location
import           Parser.ExprM              (Operator (..), makeExprParser)
import           Parser.Monad
import qualified Parser.Operator           as Op
import           Parser.State              hiding (State)
import           SymbolTable               (closeScope, insertSymbol, lookup,
                                            openScope)
import           Token
import           Treelike
import           Type                      (ArgMode (..), Type (..), (=:=))
--------------------------------------------------------------------------------
import           Control.Lens              (use, (%=), (&~), (<&>), (^.))
import           Control.Monad             (unless, void, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, get,
                                            gets, modify, put)
import           Data.Functor              (($>))
import qualified Data.Map.Strict           as Map (lookup)
import           Data.Monoid               ((<>))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, singleton)
import           Data.Text                 (Text, pack, unpack)
import           Prelude                   hiding (Ordering (..), lex, lookup)
import           Text.Megaparsec           (getPosition, parseErrorPretty,
                                            sepBy1, some, (<|>))
--------------------------------------------------------------------------------
import           Debug.Trace
import           Text.Megaparsec.Pos       (unsafePos)

expression :: Parser (Maybe Expression)
expression = evalStateT expr []


data ProtoRange
  = ProtoVar                  -- ^ The associated expression is the
                              -- variable of interest
  | ProtoQRange  QRange       -- ^ A valid QRange has been formed
  | ProtoLow     Expression   -- ^ A lower bound has been found
  | ProtoHigh    Expression   -- ^ An upper bound has been found
  | ProtoNothing              -- ^ No manner of range has been formed
  deriving (Show)

newtype Taint = Taint Bool -- A MetaExpr is `tainted` when it
                           -- contains a quantified (dummy)
                           -- variable. This means it cannot
                           -- be used as a range limit.
  deriving (Eq, Ord, Show)

instance Monoid Taint where
  mempty = Taint False
  Taint x `mappend` Taint y = Taint (x || y)

type MetaExpr = (Expression, ProtoRange, Taint)

type ParserExp = StateT [ Text ] Parser


expr :: ParserExp (Maybe Expression)
expr = pure . ((\(e,_,_) -> e) <$>) =<< metaexpr


metaexpr :: ParserExp (Maybe MetaExpr)
metaexpr = makeExprParser term operator

term :: ParserExp (Maybe MetaExpr)
term =  parens metaexpr
 -- <|> try call -- TODO: function calling, depends on ST design
    <|> variable
    <|> bool
    <|> basicLit integerLit       IntV          GInt
    <|> basicLit floatLit         FloatV        GFloat
    <|> basicLit charLit          CharV         GChar
    <|> setLit   TokEmptySet      EmptySet      GSet
    <|> setLit   TokEmptyMultiset EmptyMultiset GMultiset
    <|> quantification
    <|> ifExp

  where

    bool :: ParserExp (Maybe MetaExpr)
    bool = do
      from <- getPosition
      lit <- boolLit
      to <- getPosition

      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = GBool
          , exp'    = Value . BoolV $ lit }
        range = if lit
          then ProtoNothing
          else ProtoQRange EmptyRange

      pure . Just $ (expr, range, Taint False)

    basicLit :: Parser a -> (a -> Value) -> Type
             -> ParserExp (Maybe MetaExpr)
    basicLit litp val t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition

      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = t
          , exp'    = Value . val $ lit }

      pure . Just $ (expr, ProtoNothing, Taint False)

    setLit :: Token -> Expression' -> (Type -> Type)
           -> ParserExp (Maybe MetaExpr)
    setLit tok e t = do
      from <- getPosition
      void $ match tok
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = t GAny
          , exp'     = e }

      pure . Just $ (expr, ProtoNothing, Taint False)


variable :: ParserExp (Maybe MetaExpr)
variable = do
  from <- getPosition
  name <- identifier
  to <- getPosition

  st <- lift (use symbolTable)

  let loc = Location (from, to)

  case name `lookup` st of

    Left _ -> do
      putError loc . UnknownError $
        "Variable `" <> unpack name <> "` not defined in this scope."

      pure Nothing

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

        pure . Just $ (expr, protorange, taint)

      Const { _constType, _constValue } ->
        let
          expr = Expression
            { E.loc
            , expType = _constType
            , exp' = Value _constValue }

        in pure . Just $ (expr, ProtoNothing, Taint False)

      Argument { _argMode, _argType } | _argMode == In || _argMode == InOut ->
        let
          expr = Expression
            { E.loc
            , expType  = _argType
            , exp'     = Obj
              { theObj = Object
                { O.loc
                , objType = _argType
                , obj'    = Variable
                  { O.name }}}}

        in pure . Just $ (expr, ProtoNothing, Taint False)

      _ -> pure Nothing


quantification :: ParserExp (Maybe MetaExpr)
quantification = do
  from <- getPosition
  void $ match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, mvart) <- declaration
  void $ match TokPipe

  modify (var:)
  range <- metaexpr
  modify tail

  case range of
    Just (cond, protorange, taint0) -> do
      let rloc = E.loc cond
      case protorange of
        ProtoVar ->
          putError rloc . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoNothing       ->
          putError rloc . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoLow         _ ->
          putError rloc . UnknownError $
            "Bad quantification range. No upper bound was given."
        ProtoHigh        _ ->
          putError rloc . UnknownError $
            "Bad quantification range. No lower bound was given."
        ProtoQRange qrange ->
          pure ()
    Nothing -> pure ()

  void $ match TokPipe

  body <- metaexpr

  case body of
    Just (Expression { expType = bodyType, E.loc = bloc }, _, taint1) ->
      unless (bodyType =:= allowedBType) .
        putError bloc . UnknownError $
          "Bad quantification body. Body must be " <> show allowedBType <> "."
    Nothing -> pure ()

  void $ match TokRightPercent
  to <- getPosition
  lift $ symbolTable %= closeScope to

  case mvart of
    Nothing -> pure Nothing
    Just t -> case body of
      Just (theBody @ Expression { expType = bodyType }, _, taint1) ->
        case range of
          Just (cond, protorange, taint0) ->
            case protorange of
              ProtoQRange qRange -> case bodyType <> allowedBType of
                GUndef  -> pure Nothing
                newType ->
                  let
                    loc = Location (from, to)
                    taint = taint0 <> taint1
                    expr = Expression
                      { E.loc
                      , expType = newType
                      , exp' = Quantification
                        { qOp      = q
                        , qVar     = var
                        , qVarType = t
                        , qRange
                        , qCond    = cond
                        , qBody    = theBody }}
                  in pure . Just $ (expr, ProtoNothing, taint)
              _ -> pure Nothing
          Nothing -> pure Nothing
      Nothing -> pure Nothing

  where
    numeric = GOneOf [ GChar, GInt, GFloat ]

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
      void $ match TokColon
      tname <- identifier

      to <- getPosition

      let loc = Location (from, to)

      typeEntry <- Map.lookup tname <$> lift (use typesTable)

      case typeEntry of
        Nothing -> do
          putError loc . UnknownError $ ("type `" <> unpack tname <> "` does not exist" )
          pure (var, Nothing)

        Just (t,_) ->
          if t =:= quantifiableTypes
            then do
              lift $ symbolTable %= insertSymbol var Entry
                { _entryName = var
                , _loc = Location (from, to)
                , _info = Var
                  { _varType  = t
                  , _varValue = Nothing }}
              pure (var, Just t)
            else do
              putError loc . UnknownError $
                "type `" <> unpack tname <> "` is not quantifiable"
              pure (var, Nothing)


data IfBuilder
  = IfGuards     (Seq (Expression, Expression)) (Maybe Expression)
  | IfExp        Expression
  | IfNothing
  deriving (Show)

instance Monoid IfBuilder where
  mempty = IfGuards Seq.empty Nothing
  IfNothing `mappend` _ = IfNothing
  _ `mappend` IfNothing = IfNothing

  (IfExp e) `mappend` _ = IfExp e

  (IfGuards gs0 Nothing) `mappend` (IfGuards gs1 t) =
    IfGuards (gs0 <> gs1) t
  (IfGuards gs Nothing)  `mappend` (IfExp e) =
    IfGuards gs (Just e)

  igt@(IfGuards _ (Just _)) `mappend` _ = igt


data IfState = IfState
  { ifType    :: Type
  , ifBuilder :: IfBuilder
  , ifTaint   :: Taint }

initialIfState = IfState
  { ifType    = GAny
  , ifBuilder = IfGuards Seq.empty Nothing
  , ifTaint   = Taint False }


ifExp :: ParserExp (Maybe MetaExpr)
ifExp = do
  from <- getPosition
  void $ match TokIf

  -- The StateT on top of ParserExp allows us to manage
  -- a state local to this level of abstraction.
  IfState { ifType, ifBuilder, ifTaint } <- execStateT guards initialIfState

  void $ match TokFi

  to <- getPosition

  let loc = Location (from, to)

  case ifBuilder of
    IfNothing -> pure Nothing
    IfExp e ->
      pure . Just $ (e { E.loc, expType = ifType }, ProtoNothing, ifTaint)
    IfGuards gs t ->
      let
        expr = Expression
          { E.loc
          , expType = ifType
          , exp' = EConditional gs t }
      in pure . Just $ (expr, ProtoNothing, ifTaint)

  where
    guards =
      -- We run `line` for each "a -> b" pair in the If metaexpr,
      -- and then we extract the final set of guards
      line `sepBy1` match TokSepGuards

    line = (get >>= lhs) <* match TokArrow >>= rhs

    lhs st = do
      left <- lift metaexpr
      -- We take the left hand side of the guard,
      -- and three things could have happened,

      case left of
        Just (l, _, taint0) ->
          case l of
            e @ Expression { expType = GBool, E.loc } ->
            -- 1. We have a good boolean expression, which is ideal
              pure (Just e, st, taint0)

            Expression { E.loc } -> do
              -- 2. We have a good expression which isn't boolean, so we
              -- report the error and clear the previous guards
              lift . putError loc . UnknownError $
                "bad left side in conditional expression"
              pure (Nothing, st, taint0)
        Nothing -> pure (Nothing, st, Taint False)

        -- badEXPRESSION { E.loc } ->
        -- -- 3. We have a bad expression, which means there was an error
        -- -- before which we can only propagate
        --   pure (Nothing, st, taint0)


    rhs (ml, st@IfState { ifType, ifBuilder, ifTaint }, taint0) = do
      right <- lift metaexpr
      -- We take the right hand side of the guard,
      -- and 7 things could have happened,

      case ifBuilder of
        IfNothing ->
          -- 1. An error occurred in a previous line, there's nothing to do
          -- about this one.
          pure ()

        _ ->
          -- No errors have occured in previous lines, and
          case right of
            Nothing -> put st { ifType = GUndef, ifBuilder = IfNothing }

            Just (r, _, taint1) ->
              case r of
                -- badexpression {} ->
                --   -- 1. The rhs is a badexpression, which means there was
                --   -- an error deep in it which we can only propagate
                --   put st { ifType = GUndef, ifBuilder = IfNothing }

                Expression { E.loc = rloc, expType } ->
                  case expType <> ifType of
                    GUndef -> do
                      -- 2. The rhs type doesn't match previous lines
                      lift . putError rloc . UnknownError $
                        "bad right side in conditional expression"
                      put st { ifType = GUndef, ifBuilder = IfNothing }

                    newType ->
                      -- The rhs is perfect in syntax and type
                      case ml of
                        Nothing ->
                          -- 3. But the lhs was bad, so we clear everything
                          put st { ifType = GUndef, ifBuilder = IfNothing }

                        Just l
                          | exp' l == Value (BoolV True) ->
                            -- 4. The lhs has the true value, so we only keep
                            -- the final value of the ifExp is this rhs.

                            put st
                              { ifType = expType
                              , ifBuilder = ifBuilder <> IfExp r
                              , ifTaint = taint1 }

                          | exp' l == Value (BoolV False) ->
                            -- 5. We have a good rhs that must be ignored because
                            -- its lhs was false. Its type does affect the ifExp.
                            put st { ifType = expType }

                          | otherwise ->
                            -- 6. We have a good rhs whose type matches the
                            -- type of previous guards, while the lhs value will be
                            -- known only at runtime, so we leave this guard
                            -- expressed as a tuple. (In the case of the first
                            -- guard, the match is done against GAny, i.e., any
                            -- type will match).
                            put IfState
                              { ifType    = expType
                              , ifBuilder =
                                ifBuilder <> IfGuards (Seq.singleton (l, r)) Nothing
                              , ifTaint   = ifTaint <> taint0 <> taint1 }


operator :: [[ Operator ParserExp (Maybe MetaExpr) ]]
operator =
  [ {-Level 0-}
    [ Postfix (foldr1 (>=>) <$> some subindex) ]
 -- , Postfix (foldr1 (>=>) <$> some field) ] -- TODO: Field access, depends on ST design
  , {-Level 1-}
    [ Prefix  (foldr1 (>=>) <$> some deref) ]
  , {-Level 2-}
    [ Prefix (match TokNot        <&> unary Op.not    )
    , Prefix (match TokMinus      <&> unary Op.uMinus )
    , Prefix (match TokAbs        <&> unary Op.abs    )
    , Prefix (match TokSqrt       <&> unary Op.sqrt   ) ]
  , {-Level 3-}
    [ InfixR (match TokPower      <&> binary Op.power ) ]
  , {-Level 4-}
    [ InfixL (match TokTimes      <&> binary Op.times )
    , InfixL (match TokDiv        <&> binary Op.div   )
    , InfixL (match TokMod        <&> binary Op.mod   ) ]
  , {-Level 5-}
    [ InfixL (match TokPlus       <&> binary Op.plus   )
    , InfixL (match TokMinus      <&> binary Op.bMinus ) ]
  , {-Level 6-}
    [ InfixL (match TokMax        <&> binary Op.max    )
    , InfixL (match TokMin        <&> binary Op.min    ) ]
  , {-Level 7-}
    [ InfixN (match TokElem       <&> membership        )
    , InfixN (match TokNotElem    <&> binary Op.notElem )
    , InfixN (match TokLT         <&> comparison Op.lt  )
    , InfixN (match TokLE         <&> comparison Op.le  )
    , InfixN (match TokGT         <&> comparison Op.gt  )
    , InfixN (match TokGE         <&> comparison Op.ge  ) ]
  , {-Level 8-}
    [ InfixN (match TokAEQ        <&> pointRange    )
    , InfixN (match TokANE        <&> binary Op.ane ) ]
  , {-Level 9-}
    [ InfixR (match TokAnd        <&> conjunction ) ]
  , {-Level 10-}
    [ InfixR (match TokOr         <&> binary Op.or ) ]
  , {-Level 11-}
    [ InfixR (match TokImplies    <&> binary Op.implies    )
    , InfixL (match TokConsequent <&> binary Op.consequent ) ]
  , {-Level 12-}
    [ InfixN (match TokBEQ        <&> binary Op.beq )
    , InfixN (match TokBNE        <&> binary Op.bne ) ]
  ]


subindex :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
subindex = do
  from' <- getPosition
  subind <- brackets metaexpr
  to <- getPosition

  case subind of
    Nothing -> pure (\_ -> pure Nothing) -- FIXME
    Just (sub, _, taint0) ->
      case sub of
        -- badexpression {} -> pure $ badSubindex to

        Expression { expType } ->
          case expType of
            GInt ->
              pure $ \case
                Nothing -> pure Nothing
                Just (expr, _, taint1) -> case expr of
                  Expression
                    { E.loc = Location (from, _)
                    , expType = GArray { innerType }
                    , exp' = Obj o } ->
                      let
                        taint = taint0 <> taint1
                        expr = Expression
                          { E.loc = Location (from, to)
                          , expType = innerType
                          , exp' = Obj
                            { theObj = Object
                              { O.loc = Location (from, to)
                              , objType = innerType
                              , obj' = Index
                                { O.inner = o
                                , index = sub }}}}
                      in pure . Just $ (expr, ProtoNothing, taint)

                  -- badexpression { E.loc = Location (from, _) } ->
                  --   let loc = Location (from, to)
                  --   in pure (badexpression { E.loc }, ProtoNothing, Taint False)

                  e -> do
                    let Location (from, _) = E.loc e
                    let loc = Location (from, to)

                    putError loc . UnknownError $ "Cannot subindex non-array."

                    pure Nothing

            _ -> do --FIXME
              let subloc = Location (from', to)
              putError subloc . UnknownError $
                "Bad subindex. Must be integer expression."
              pure (\_ -> pure Nothing)


deref :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
deref = do
  from <- getPosition
  void $ match TokTimes

  pure $ \case
    Nothing -> pure Nothing

    Just (expr, _, taint) -> case expr of
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
          in pure . Just $ (expr, ProtoNothing, taint)

      -- badexpression { E.loc = Location (_, to) } ->
      --   let loc = Location (from, to)
      --   in pure (badexpression { E.loc }, ProtoNothing, Taint False)


      e -> do
        let Location (_, to) = E.loc e
        let loc = Location (from, to)

        putError loc . UnknownError $ "Cannot deref non-pointer."

        pure Nothing

unary :: Op.Un -> Location
      -> (Maybe MetaExpr) -> ParserExp (Maybe MetaExpr)
unary unOp
  opLoc @ (Location (from,_))
  (Just (i @ Expression { expType = itype, exp' }, _, taint))
  = case Op.unType unOp itype of
    Left expected -> do
      let loc = Location (from, to i)
      putError loc . UnknownError $
        "Operator `" <> show (Op.unSymbol unOp) <> "` at " <> show opLoc <>
        " expected an expression of type " <> expected <>
        ", but received " <> show itype <> "."
      pure Nothing

    Right ret -> do
      let
        exp'' = case exp' of
          Value v -> Value . Op.unFunc unOp $ v
          _ -> Unary
            { unOp = Op.unSymbol unOp
            , E.inner = i }

        expr = Expression
          { E.loc = Location (from, to i)
          , expType = ret
          , exp' = exp'' }
      pure . Just $ (expr, ProtoNothing, taint)

unary _ _ _ = pure Nothing


binary :: Op.Bin -> Location
       -> (Maybe MetaExpr) -> (Maybe MetaExpr) -> ParserExp (Maybe MetaExpr)
binary binOp opLoc
  (Just (l @ Expression { expType = ltype, exp' = lexp }, _, ltaint))
  (Just (r @ Expression { expType = rtype, exp' = rexp }, _, rtaint))
  = case Op.binType binOp ltype rtype of

    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <> show opLoc <>
          " expected two expressions of types " <> expected <>
          ", but received " <> show (ltype, rtype) <> ".")
      pure Nothing

    Right ret ->
      let
        taint = ltaint <> rtaint

        exp' = case (lexp, rexp) of
          (Value v, Value w) ->
            Value $ Op.binFunc binOp v w
          _ -> Binary
            { binOp = Op.binSymbol binOp
            , lexpr = l
            , rexpr = r }

        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = ret
          , exp' }

      in pure . Just $ (expr, ProtoNothing, taint)

binary _ _ _ _ = pure Nothing


membership :: Location
           -> (Maybe MetaExpr) -> (Maybe MetaExpr)
           -> ParserExp (Maybe MetaExpr)
membership opLoc
  (Just (l @ Expression { expType = ltype }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype }, _, Taint False))
  = case Op.binType Op.elem ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure . Just $ (expr, ProtoQRange (SetRange r), Taint False)

    Right _ -> error "internal error: impossible membership type"

membership opLoc l r = binary Op.elem opLoc l r


comparison :: Op.Bin -> Location
           -> (Maybe MetaExpr) -> (Maybe MetaExpr) -> ParserExp (Maybe MetaExpr)
comparison binOp opLoc
  (Just (l @ Expression { expType = ltype }, _, Taint False))
  (Just (r @ Expression { expType = rtype }, ProtoVar, _))
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError loc . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> " expected two expressions of types " <> expected <>
            ", but received " <> show (ltype, rtype) <> ".")
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoLow  (succ' l)
            LE -> ProtoLow  l
            GT -> ProtoHigh (pred' l)
            GE -> ProtoHigh l
            _  -> error "internal error: impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , exp'     = eSkip }
        in pure . Just $ (expr, range, Taint True)

      Right _ ->
        error "internal error: impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, exp' } =
      Expression { E.loc, expType, exp' = Unary op e }

comparison binOp opLoc
  (Just (l @ Expression { expType = ltype }, ProtoVar, _))
  (Just (r @ Expression { expType = rtype }, _, Taint False))
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError loc . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> " expected two expressions of types " <> expected <>
            ", but received " <> show (ltype, rtype) <> ".")
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoHigh (pred' r)
            LE -> ProtoHigh r
            GT -> ProtoLow  (succ' r)
            GE -> ProtoLow  r
            _  -> error "internal error: impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , exp'     = eSkip }
        in pure . Just $ (expr, range, Taint True)

      Right _ ->
        error "internal error: impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, exp' } =
      Expression { E.loc, expType, exp' = Unary op e }

comparison binOp opLoc l r = binary binOp opLoc l r


pointRange :: Location
           -> (Maybe MetaExpr) -> (Maybe MetaExpr)
           -> ParserExp (Maybe MetaExpr)
pointRange opLoc
  (Just (l @ Expression { expType = ltype }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype }, _, Taint False))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure . Just $ (expr, ProtoQRange (PointRange r), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc
  (Just (l @ Expression { expType = rtype }, _, Taint False))
  (Just (r @ Expression { expType = ltype }, ProtoVar, ltaint))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ", but received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure . Just $ (expr, ProtoQRange (PointRange l), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc l r = binary Op.aeq opLoc l r


conjunction :: Location
            -> (Maybe MetaExpr) -> (Maybe MetaExpr)
            -> ParserExp (Maybe MetaExpr)
conjunction _ Nothing _ = pure Nothing
conjunction _ _ Nothing = pure Nothing
conjunction opLoc
  (Just (l @ Expression { expType = ltype, exp' = lexp' }, lproto, ltaint))
  (Just (r @ Expression { expType = rtype, exp' = rexp' }, rproto, rtaint))
  = case Op.binType Op.and ltype rtype of
    Right GBool -> do
      varname <- gets head
      traceShowM l
      traceShowM r
      let
        loc = Location (from l, to r)
        taint = ltaint <> rtaint
        (exp', range) = case (lproto, rproto) of
          (ProtoVar, _) -> error "internal error: boolean ProtoVar"
          (_, ProtoVar) -> error "internal error: boolean ProtoVar"

          (q @ (ProtoQRange EmptyRange), _) ->
            (eSkip, q)
          (_, q @ (ProtoQRange EmptyRange)) ->
            (eSkip, q)

          (ProtoNothing, ProtoNothing) ->
            case (lexp', rexp') of
              (Value v, Value w) ->
                (Value (Op.binFunc Op.and v w), ProtoNothing)
              _ ->
                let
                  expr = Binary
                    { binOp = And
                    , lexpr = l
                    , rexpr = r }
                in (expr, ProtoNothing)

          (ProtoNothing, proto) ->
            (lexp', proto)
          (proto, ProtoNothing) ->
            (rexp', proto)

          (ProtoLow low, ProtoHigh high) ->
            (eSkip, ProtoQRange (ExpRange low high))
          (ProtoHigh high, ProtoLow low) ->
            (eSkip, ProtoQRange (ExpRange low high))
          (ProtoLow  llow,  ProtoLow  rlow ) ->
            (eSkip, ProtoLow  (joinProtos Max llow rlow))
          (ProtoHigh lhigh, ProtoHigh rhigh) ->
            (eSkip, ProtoHigh (joinProtos Min lhigh rhigh))

          (ProtoQRange l @ ExpRange {}, ProtoQRange r @ ExpRange {}) ->
            (eSkip, joinExpRanges l r)

          (ProtoQRange l @ ExpRange {}, r @ ProtoLow {}) ->
            (eSkip, joinExpRangeProto l r)
          (ProtoQRange l @ ExpRange {}, r @ ProtoHigh {}) ->
            (eSkip, joinExpRangeProto l r)
          (l @ ProtoLow {} , ProtoQRange r @ ExpRange {}) ->
            (eSkip, joinExpRangeProto r l)
          (l @ ProtoHigh {}, ProtoQRange r @ ExpRange {}) ->
            (eSkip, joinExpRangeProto r l)

          (point @ (ProtoQRange PointRange {}), proto) ->
            (rebuild varname proto, point)
          (proto, point @ (ProtoQRange PointRange {})) ->
            (rebuild varname proto, point)

          (set @ (ProtoQRange SetRange {}), proto) ->
            (rebuild varname proto, set)
          (proto, set @ (ProtoQRange SetRange {})) ->
            (rebuild varname proto, set)

        expr =
          Expression
            { E.loc
            , expType = GBool
            , exp' }

      pure . Just $ (expr, range, taint)

    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        "Operator `" <> show And <> "` at " <> show opLoc <> " expected two\
        \ expressions of types " <> expected <> ", but received " <>
        show (ltype, rtype) <> "."
      pure Nothing

    Right _ -> error "internal error: Bad andOp type"

    where
      joinProtos binOp l r = Expression
        { E.loc    = Location (from l, to r)
        , expType  = expType l
        , exp'     = Binary binOp l r }

      joinExpRanges l@ExpRange {} ExpRange { low, high }
        = let ProtoQRange l' = joinExpRangeProto l (ProtoLow low)
          in  joinExpRangeProto l' (ProtoHigh high)

      joinExpRanges _ _ = error "internal error: can only join two ExpRanges"

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

      joinExpRangeProto _ _ = error
        "internal error: can only join ExpRanges to Proto{Low|High}"

      rebuild varname (ProtoQRange (SetRange e)) =
        let
          t = case expType e of
            GSet a      -> a
            GMultiset a -> a
            _           -> error "internal error: impossible set type"
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

      rebuild _ ProtoVar = error
        "internal error: can't rebuild range variable"
      rebuild _ ProtoNothing = error
        "internal error: can't rebuild a non-range"
      rebuild _ (ProtoQRange EmptyRange) = error
        "internal error: can't rebuild an empty range"

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

--------------------------------------------------------------------------------
testExpr :: Parser (Maybe Expression) ->  String -> IO ()
testExpr myparser strinput = do
  let
    input = pack strinput
    ts = lex "" input
    init' = initialState "" &~ do
      symbolTable %= openScope (SourcePos "" (unsafePos 4) (unsafePos 10))
      symbolTable %= insertSymbol (pack "a") Entry
        { _entryName  = pack "a"
        , _loc        = Location (SourcePos "" (unsafePos 2) (unsafePos 2), SourcePos "" (unsafePos 2) (unsafePos 20))
        , _info       = Var
          { _varType  = GInt
          , _varValue = Nothing }}
      symbolTable %= insertSymbol (pack "b") Entry
        { _entryName  = pack "b"
        , _loc        = Location (SourcePos "" (unsafePos 3) (unsafePos 3), SourcePos "" (unsafePos 3) (unsafePos 20))
        , _info       = Var
          { _varType  = GInt
          , _varValue = Nothing }}
      symbolTable %= insertSymbol (pack "c") Entry
        { _entryName  = pack "c"
        , _loc        = Location (SourcePos "" (unsafePos 4) (unsafePos 4), SourcePos "" (unsafePos 4) (unsafePos 20))
        , _info       = Var
          { _varType  = GPointer GInt
          , _varValue = Nothing }}
      symbolTable %= insertSymbol (pack "d") Entry
        { _entryName  = pack "d"
        , _loc        = Location (SourcePos "" (unsafePos 4) (unsafePos 4), SourcePos "" (unsafePos 4) (unsafePos 20))
        , _info       = Var
          { _varType  = GArray 10 (GArray 10 GInt)
          , _varValue = Nothing }}
      symbolTable %= insertSymbol (pack "pii") Entry
        { _entryName  = pack "pii"
        , _loc        = Location (SourcePos "" (unsafePos 4) (unsafePos 4), SourcePos "" (unsafePos 4) (unsafePos 20))
        , _info       = Const
          { _constType  = GFloat
          , _constValue = FloatV 3.14 }}

    (r, s) = runParser myparser "" init' ts

  case r of
    Just x -> do
      putStrLn . drawTree . toTree $ x
      print $ expType x
    _ -> mapM_ (putStrLn . parseErrorPretty) (s ^. errors)
  -- case r of
  --   Right xx -> putStrLn . drawTree . toTree $ xx
  --   Left xx -> mapM_ (putStrLn . parseErrorPretty) (s ^. errors)


  -- case r of
  --   Right r' -> do
  --     putStrLn . drawTree . toTree $ r'
  --     case r' of
  --       Expression { expType } -> print expType
  --       _ -> pure ()
  --   _ -> pure ()
  -- mapM_ (putStrLn . prettyError) (s ^. errors)

-- testParser :: Show a => Parser a -> String -> IO ()
-- testParser myparser strinput = do
--   let input = pack strinput
--   let Right ets = runParser lexer "" input
--   let (r,s) = runState (runParserT myparser "" ets) initialState
--   case r of
--     Right r' -> print r'
--     _ -> print "oops"
