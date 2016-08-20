{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression
  ( expression
  , string
  ) where
--------------------------------------------------------------------------------
import           AST.Expression            hiding (inner, loc)
import qualified AST.Expression            as E (inner, loc)
import           AST.Object                hiding (inner, loc, name)
import qualified AST.Object                as O (inner, loc, name)
import           Entry                     (Entry' (..), Entry'' (..), info,
                                            varType)
import           Error                     (Error (..), prettyError)
import           Graciela hiding (putError)
import qualified Graciela as G (putError)
import           Lexer
import           Limits
import           Location
import           Parser.ExprM              (Operator (..), makeExprParser)
import qualified Parser.Operator           as Op
import           Parser.Token
import           SymbolTable               (closeScope, insertSymbol, lookup,
                                            openScope)
import           Token
import           Treelike
import           Type                      (ArgMode (..), Type (..), (=:=))
--------------------------------------------------------------------------------
import           Control.Applicative       (Alternative)
import           Control.Lens              (makeLenses, use, (%=), (&~), (<&>),
                                            (^.))
import           Control.Monad             (foldM, unless, void, when, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, get,
                                            gets, modify, put, runState)
import           Data.Char                 (chr, ord)
import           Data.Fixed                (mod')
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
import           Debug.Trace

string :: Graciela Expression
string = do
  from <- getPosition
  str  <- stringLit
  to   <- getPosition
  let location = Location(from,to)
  pure (Expression location GString StringLit{theString = unpack str})


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


putError :: Location -> Error -> GracielaRange ()
putError l err = lift $ G.putError l err


expr :: GracielaRange Expression
expr = (\(e,_,_) -> e) <$> metaexpr


metaexpr :: GracielaRange MetaExpr
metaexpr = makeExprParser term operator

term :: GracielaRange MetaExpr
term =  parens metaexpr
 -- <|> try call -- TODO: function calling, depends on ST design
    <|> variable
    <|> bool
    <|> nullptr
    <|> basicLit integerLit       IntV          GInt
    <|> basicLit floatLit         FloatV        GFloat
    <|> basicLit charLit          CharV         GChar
    <|> setLit   TokEmptySet      EmptySet      GSet
    <|> setLit   TokEmptyMultiset EmptyMultiset GMultiset
    <|> quantification
    <|> ifExp

  where
    bool :: GracielaRange MetaExpr
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

      pure (expr, range, Taint False)

    nullptr :: GracielaRange MetaExpr
    nullptr = do
      from <- getPosition
      match TokNull
      to <- getPosition
      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = GPointer GAny
          , exp'    = NullPtr }
        
      pure (expr, ProtoNothing, Taint False)

    basicLit :: Graciela a -> (a -> Value) -> Type
             -> GracielaRange MetaExpr
    basicLit litp val t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition

      let
        expr = Expression
          { E.loc   = Location (from, to)
          , expType = t
          , exp'    = Value . val $ lit }

      pure (expr, ProtoNothing, Taint False)

    setLit :: Token -> Expression' -> (Type -> Type)
           -> GracielaRange MetaExpr
    setLit tok e t = do
      from <- getPosition
      match tok
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = t GAny
          , exp'     = e }

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
      putError loc . UnknownError $
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
          expr = Expression
            { E.loc
            , expType = _constType
            , exp' = Value _constValue }

        in pure (expr, ProtoNothing, Taint False)

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

        in pure (expr, ProtoNothing, Taint False)

      _ -> pure (BadExpression { E.loc }, ProtoNothing, Taint False)


quantification :: GracielaRange MetaExpr
quantification = do
  from <- getPosition
  match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, mvart) <- declaration
  match TokPipe

  modify (var:)
  (cond, protorange, taint0) <- metaexpr
  let rloc = E.loc cond
  modify tail

  case cond of
    Expression {} ->
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
    BadExpression {} ->
      pure ()

  match TokPipe

  (body, _, taint1) <- metaexpr

  case body of
    Expression { expType = bodyType, E.loc = bloc } ->
      unless (bodyType =:= allowedBType) .
        putError bloc . UnknownError $
          "Bad quantification body. Body must be " <> show allowedBType <> "."
    _ -> pure ()

  match TokRightPercent
  to <- getPosition
  lift $ symbolTable %= closeScope to

  let
    loc = Location (from, to)
    taint = taint0 <> taint1
    expr = case mvart of
      Nothing ->
        BadExpression { E.loc }

      Just t -> case body of
        BadExpression {} ->
          BadExpression { E.loc }

        Expression { expType = bodyType } ->
          case protorange of
            ProtoQRange qRange -> if bodyType =:= allowedBType
              then Expression
                { E.loc
                , expType = expType body
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
      match TokColon
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
  = IfGuards (Seq (Expression, Expression))
  | IfExp Expression
  | IfNothing
  deriving (Show)

instance Monoid IfBuilder where
  mempty = IfGuards Seq.empty
  IfNothing      `mappend` _              = IfNothing
  _              `mappend` IfNothing      = IfNothing
  (IfExp e)      `mappend` _              = IfExp e
  _              `mappend` (IfExp e)      = IfExp e
  (IfGuards gs0) `mappend` (IfGuards gs1) = IfGuards (gs0 <> gs1)


data IfState = IfState
  { ifType    :: Type
  , ifBuilder :: IfBuilder
  , ifTaint   :: Taint }

initialIfState = IfState
  { ifType    = GAny
  , ifBuilder = IfGuards Seq.empty
  , ifTaint   = Taint False }


ifExp :: GracielaRange MetaExpr
ifExp = do
  from <- getPosition
  match TokIf

  -- The StateT on top of GracielaRange allows us to manage
  -- a state local to this level of abstraction.
  IfState { ifType, ifBuilder, ifTaint } <- execStateT guards initialIfState

  match TokFi

  lift (use errors) >>= traceShowM

  to <- getPosition

  let
    loc = Location (from, to)

    expr = case ifBuilder of
      IfNothing -> BadExpression { E.loc }
      IfExp e -> e { E.loc, expType = ifType }
      IfGuards gs -> Expression
        { E.loc
        , expType = ifType
        , exp' = EConditional gs }

  pure (expr, ProtoNothing, ifTaint)

  where
    guards =
      -- We run `line` for each "a -> b" pair in the If metaexpr,
      -- and then we extract the final set of guards
      line `sepBy1` match TokSepGuards

    line = (get >>= lhs) <* match TokArrow >>= rhs

    lhs st = do
      (l, _, taint0) <- lift metaexpr
      -- We take the left hand side of the guard,
      -- and three things could have happened,

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

        BadExpression { E.loc } ->
        -- 3. We have a bad expression, which means there was an error
        -- before which we can only propagate
          pure (Nothing, st, taint0)


    rhs (ml, st@IfState { ifType, ifBuilder, ifTaint }, taint0) = do
      (r, _, taint1) <- lift metaexpr
      -- We take the right hand side of the guard,
      -- and 7 things could have happened,

      case ifBuilder of
        IfNothing ->
          -- 1. An error occurred in a previous line, there's nothing to do
          -- about this one.
          pure ()

        _ ->
          -- No errors have occured in previous lines, and

          case r of
            BadExpression {} ->
              -- 1. The rhs is a BadExpression, which means there was
              -- an error deep in it which we can only propagate
              put st { ifType = GUndef, ifBuilder = IfNothing }

            Expression { E.loc = rloc, expType } ->
              if expType =:= ifType
                then
                  -- The rhs is perfect in syntax and type
                  case ml of
                    Nothing ->
                      -- 2. But the lhs was bad, so we clear everything
                      put st { ifType = GUndef, ifBuilder = IfNothing }

                    Just l
                      | exp' l == Value (BoolV True) ->
                        -- 3. The lhs has the true value, so we only keep
                        -- the final value of the ifExp is this rhs.

                        put st
                          { ifType = expType
                          , ifBuilder = ifBuilder <> IfExp r
                          , ifTaint = taint1 }

                      | exp' l == Value (BoolV False) ->
                        -- 4. We have a good rhs that must be ignored because
                        -- its lhs was false. Its type does affect the ifExp.
                        put st { ifType = expType }

                      | otherwise ->
                        -- 5. We have a good rhs whose type matches the
                        -- type of previous guards, while the lhs value will be
                        -- known only at runtime, so we leave this guard
                        -- expressed as a tuple. (In the case of the first
                        -- guard, the match is done against GAny, i.e., any
                        -- type will match).
                        put IfState
                          { ifType    = expType
                          , ifBuilder =
                            ifBuilder <> IfGuards (Seq.singleton (l, r))
                          , ifTaint   = ifTaint <> taint0 <> taint1 }

                else do
                  -- 6. The rhs type doesn't match previous lines
                  lift . putError rloc . UnknownError $
                    "bad right side in conditional expression"
                  put st { ifType = GUndef, ifBuilder = IfNothing }


operator :: [[ Operator GracielaRange MetaExpr ]]
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

              putError loc . UnknownError $ "Cannot subindex non-array."

              pure (BadExpression { E.loc }, ProtoNothing, Taint False)

        _ -> do
          let subloc = Location (from', to)
          putError subloc . UnknownError $ "Bad subindex. Must be integer expression."
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

      putError loc . UnknownError $ "Cannot deref non-pointer."

      pure (BadExpression { E.loc }, ProtoNothing, Taint False)


unary :: Op.Un -> Location
      -> MetaExpr -> GracielaRange MetaExpr
unary unOp
  opLoc @ (Location (from,_))
  (i @ Expression { expType = itype, exp' }, _, taint)
  = case Op.unType unOp itype of
    Left expected -> do
      let loc = Location (from, to i)
      putError loc . UnknownError $
        "Operator `" <> show (Op.unSymbol unOp) <> "` at " <> show opLoc <>
        " expected an expression of type " <> expected <>
        ",\n\tbut received " <> show itype <> "."
      pure (BadExpression { E.loc }, ProtoNothing, taint)
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
      pure (expr, ProtoNothing, taint)

unary _ (Location (from,_)) (i,_,taint) =
  let loc = Location (from, to i)
  in pure (BadExpression { E.loc }, ProtoNothing, taint)


binary :: Op.Bin -> Location
       -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
binary binOp opLoc
  (l @ Expression { expType = ltype, exp' = lexp }, _, ltaint)
  (r @ Expression { expType = rtype, exp' = rexp }, _, rtaint)
  = case Op.binType binOp ltype rtype of

    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <> show opLoc <>
          "\n\texpected two expressions of types " <> expected <>
          ",\n\tbut received " <> show (ltype, rtype) <> ".")
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

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

      in pure (expr, ProtoNothing, taint)

binary _ _ (l,_,_) (r,_,_) =
  let expr = BadExpression { E.loc = Location (from l, to r) }
  in pure (expr, ProtoNothing, Taint False)


membership :: Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
membership opLoc
  (l @ Expression { expType = ltype }, ProtoVar, ltaint)
  (r @ Expression { expType = rtype }, _, Taint False)
  = case Op.binType Op.elem ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure (expr, ProtoQRange (SetRange r), Taint False)

    Right _ -> error "internal error: impossible membership type"

membership opLoc l r = binary Op.elem opLoc l r


comparison :: Op.Bin -> Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
comparison binOp opLoc
  (l @ Expression { expType = ltype }, _, Taint False)
  (r @ Expression { expType = rtype }, ProtoVar, _)
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError loc . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> "\n\texpected two expressions of types " <> expected <>
            ",\n\tbut received " <> show (ltype, rtype) <> ".")
        pure (BadExpression { E.loc }, ProtoNothing, Taint False)

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoLow  (succ' l)
            LE -> ProtoLow  l
            GT -> ProtoHigh (pred' l)
            GE -> ProtoHigh l
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , exp'     = eSkip }
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
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError loc . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> "\n\texpected two expressions of types " <> expected <>
            ",\n\tbut received " <> show (ltype, rtype) <> ".")
        pure (BadExpression { E.loc }, ProtoNothing, Taint False)

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoHigh (pred' r)
            LE -> ProtoHigh r
            GT -> ProtoLow  (succ' r)
            GE -> ProtoLow  r
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , exp'     = eSkip }
        in pure (expr, range, Taint True)

      Right _ ->
        error "internal error: impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, exp' } =
      Expression { E.loc, expType, exp' = Unary op e }

comparison binOp opLoc l r = binary binOp opLoc l r


pointRange :: Location
           -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
pointRange opLoc
  (l @ Expression { expType = ltype }, ProtoVar, ltaint)
  (r @ Expression { expType = rtype }, _, Taint False)
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure (expr, ProtoQRange (PointRange r), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc
  (l @ Expression { expType = rtype }, _, Taint False)
  (r @ Expression { expType = ltype }, ProtoVar, ltaint)
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , exp'     = eSkip }
      in pure (expr, ProtoQRange (PointRange l), Taint False)

    Right _ -> error "internal error: impossible type equality"

pointRange opLoc l r = binary Op.aeq opLoc l r


conjunction :: Location
            -> MetaExpr -> MetaExpr -> GracielaRange MetaExpr
conjunction opLoc
  (l @ Expression { expType = ltype, exp' = lexp' }, lproto, ltaint)
  (r @ Expression { expType = rtype, exp' = rexp' }, rproto, rtaint)
  = case Op.binType Op.and ltype rtype of
    Right GBool -> do
      varname <- gets head
      let
        loc = Location (from l, to r)
        taint = ltaint <> rtaint
        (exp', range) = case (lexp', rexp') of
          (Value v, Value w) -> (Value (Op.binFunc Op.and v w), ProtoNothing)
          _ -> case (lproto, rproto) of
            (ProtoVar, proto) -> error "internal error: boolean ProtoVar"
            (proto, ProtoVar) -> error "internal error: boolean ProtoVar"

            (q @ (ProtoQRange EmptyRange), _) ->
              (eSkip, q)
            (_, q @ (ProtoQRange EmptyRange)) ->
              (eSkip, q)

            (ProtoNothing, ProtoNothing) ->
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

      pure (expr, range, taint)

    Left expected -> do
      let loc = Location (from l, to r)
      putError loc . UnknownError $
        "Operator `" <> show And <> "` at " <> show opLoc <> " expected two\
        \ expressions of types " <> expected <> ",\n\tbut received " <>
        show (ltype, rtype) <> "."
      pure (BadExpression { E.loc }, ProtoNothing, Taint False)

    Right _ -> error "internal error: Bad andOp type"

    where
      joinProtos binOp l r = Expression
        { E.loc    = Location (from l, to r)
        , expType  = expType l
        , exp'     = Binary binOp l r }

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
testExpr :: Graciela Expression ->  String -> IO ()
testExpr myparser strinput = do
  let
    input = pack strinput
    Right ets = runParser lexer "" input
    init' = initialState &~ do
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
        symbolTable %= insertSymbol (pack "pii") Entry
          { _entryName  = pack "pii"
          , _loc        = Location (SourcePos "" (unsafePos 4) (unsafePos 4), SourcePos "" (unsafePos 4) (unsafePos 20))
          , _info       = Const
            { _constType  = GFloat
            , _constValue = FloatV 3.14 }}
    (r, s) = runState (runParserT myparser "" ets) init'

  case r of
    Right r' -> do
      putStrLn . drawTree . toTree $ r'
      case r' of
        Expression { expType } -> print expType
        _ -> pure ()
    _ -> pure ()
  mapM_ (putStrLn . prettyError) (s ^. errors)

testParser :: Show a => Graciela a -> String -> IO ()
testParser myparser strinput = do
  let input = pack strinput
  let Right ets = runParser lexer "" input
  let (r,s) = runState (runParserT myparser "" ets) initialState
  case r of
    Right r' -> print r'
    _ -> print "oops"
