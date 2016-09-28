{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}

module Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression            hiding (inner, loc)
import qualified AST.Expression            as E (inner, loc)
import           AST.Object                hiding (inner, loc, name)
import qualified AST.Object                as O (inner, loc, name)
import           AST.Struct                (Struct (..), fillTypes)
import           AST.Type                  (ArgMode (..), Expression, Object,
                                            QRange, Type (..), fillType, hasDT,
                                            (=:=))
import           Entry                     (Entry (..), Entry' (..), info)
import           Error                     (Error (..))
import           Error                     (internal)
import           Lexer
import           Location
import           Parser.Config
import           Parser.ExprM              (Operator (..), makeExprParser)
import           Parser.Monad
import qualified Parser.Operator           as Op
import           Parser.State              hiding (State)
import           SymbolTable               (closeScope, defocus, emptyGlobal,
                                            insertSymbol, lookup, openScope)
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens              (elements, use, view, (%%=), (%=),
                                            (%~), (&), (&~), (.=), (<&>), (^.),
                                            _1, _3, _Just)
import           Control.Monad             (foldM, unless, void, when, (>=>))
import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, get,
                                            gets, modify, put)
import qualified Data.Array                as Array (listArray)
import           Data.Foldable             (foldl')
import           Data.Functor              (($>))
import qualified Data.Map.Strict           as Map (insert, lookup, size)
import           Data.Maybe                (catMaybes, fromJust)
import           Data.Monoid               (First (..))
import           Data.Semigroup            (Semigroup (..))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, fromList, singleton,
                                                   zip)
import           Data.Text                 (Text, pack, unpack)
import           Prelude                   hiding (Ordering (..), lex, lookup)
import           Text.Megaparsec           (between, getPosition, lookAhead,
                                            manyTill, optional,
                                            parseErrorPretty, try, (<|>))
--------------------------------------------------------------------------------
import           Debug.Trace

expression :: Parser (Maybe Expression)
expression =
  evalStateT expr []

data ProtoRange
  = ProtoVar                  -- ^ The associated expression is the
                              -- variable of interest
  | ProtoQRange  QRange       -- ^ A valid QRange has been formed
  | ProtoLow     Expression   -- ^ A lower bound has been found
  | ProtoHigh    Expression   -- ^ An upper bound has been found
  | ProtoNothing              -- ^ No manner of range has been formed
  deriving (Eq, Show)

newtype Taint = Taint Bool -- A MetaExpr is `tainted` when it
                           -- contains a quantified (dummy)
                           -- variable. This means it cannot
                           -- be used as a range limit.
  deriving (Eq, Ord, Show)

instance Semigroup Taint where
  Taint x <> Taint y = Taint (x || y)

instance Monoid Taint where
  mempty = Taint False
  mappend = (<>)

type MetaExpr = (Expression, ProtoRange, Taint)

type ParserExp = StateT [ Text ] Parser


expr :: ParserExp (Maybe Expression)
expr = pure . (view _1 <$>) =<< metaexpr


metaexpr :: ParserExp (Maybe MetaExpr)
metaexpr =
  makeExprParser term operator


term :: ParserExp (Maybe MetaExpr)
term =  parens metaexpr
    <|> callOrVariable
    <|> bool
    <|> nullptr
    <|> basicLit integerLit IntV   GInt
    <|> basicLit floatLit   FloatV GFloat
    <|> basicLit charLit    CharV  GChar
    <|> emptySet
    <|> collection
    <|> string
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
          { E.loc    = Location (from, to)
          , expType  = GBool
          , expConst = True
          , exp'     = Value . BoolV $ lit }
        range = if lit
          then ProtoNothing
          else ProtoQRange EmptyRange

      pure $ Just (expr, range, Taint False)

    nullptr :: ParserExp (Maybe MetaExpr)
    nullptr = do
      from <- getPosition
      match TokNull
      to <- getPosition
      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GPointer GAny
          , expConst = False
          , exp'     = NullPtr }

      pure $ Just (expr, ProtoNothing, Taint False)

    basicLit :: Parser a -> (a -> Value) -> Type
             -> ParserExp (Maybe MetaExpr)
    basicLit litp val t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = t
          , expConst = True
          , exp'     = Value . val $ lit }

      pure $ Just (expr, ProtoNothing, Taint False)

    emptySet :: ParserExp (Maybe MetaExpr)
    emptySet = do
      loc <- match TokEmptySet
      let
        expr = Expression
          { E.loc
          , expType  = GSet GAny
          , expConst = True
          , exp'     = Collection Set Nothing Seq.empty }
      pure $ Just (expr, ProtoNothing, Taint False)

    string = do
      from <- getPosition
      text <- stringLit
      to   <- getPosition

      strId <- lift $ stringIds %%= \m -> case text `Map.lookup` m of
        Just i  -> (i, m)
        Nothing -> let i = Map.size m in (i, Map.insert text i m)

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GString
          , expConst = True
          , exp'     = StringLit strId }

      pure $ Just (expr, ProtoNothing, Taint False)


collection :: ParserExp (Maybe MetaExpr)
collection = do
  (colKind, Location (from,_)) <-  (Set,)      <$> match TokLeftBrace
                               <|> (Multiset,) <$> match TokLeftBag
                               <|> (Sequence,) <$> match TokLeftSeq

  mvrc <- optional (varAndRange from)
  melems <- elems $ case colKind of
    Set      -> TokRightBrace
    Multiset -> TokRightBag
    Sequence -> TokRightSeq

  Location (_, to) <- match' $ case colKind of
    Set      -> TokRightBrace
    Multiset -> TokRightBag
    Sequence -> TokRightSeq

  case mvrc of
    Just _ -> lift $ symbolTable %= closeScope to
    _      -> pure ()

  case mvrc of
    -- A variable was given but an error occurred, either syntax or semantic
    Just Nothing -> pure Nothing

    -- A variable was not given, only (maybe) elems
    Nothing -> case melems of
      -- There were errors in the elems
      Nothing -> pure Nothing
      -- All elems are ok
      Just (els, t) ->
        let
          e = Expression
            { E.loc   = Location (from, to)
            , expType = (case colKind of
              Set      -> GSet
              Multiset -> GMultiset
              Sequence -> GSeq
              ) t
            , expConst = False
            , exp' = Collection
              { colKind
              , colVar   = Nothing
              , colElems = els }}
        in pure $ Just (e, ProtoNothing, Taint False)

    -- A variable was given without errors
    Just (Just (var, varTy, range, cond)) -> case melems of
      -- But there were errors in the elems
      Nothing -> pure Nothing
      -- And the elems are okay
      Just (els, t) -> if null els
        -- The element list is empty
        then do
          putError from . UnknownError $
            "Set comprehension without elements."
          pure Nothing

        -- The element list is not empty
        else
          let
            e = Expression
              { E.loc   = Location (from, to)
              , expType = (case colKind of
                Set      -> GSet
                Multiset -> GMultiset
                Sequence -> GSeq
                ) t
              , expConst = False
              , exp' = Collection
                { colKind
                , colVar   = Just (var, varTy, range, cond)
                , colElems = els }}
          in pure $ Just (e, ProtoNothing, Taint False)

  where
    varAndRange :: SourcePos -> ParserExp (Maybe (Text, Type, QRange, Expression))
    varAndRange from = do
      void . lookAhead . try $ identifier *> match TokColon

      lift $ symbolTable %= openScope from
      (var, varTy) <- declaration

      void $ match' TokPipe

      range <- case varTy of
        GUndef -> do
          anyToken `manyTill` match' TokPipe
          pure Nothing
        _ -> do
          modify (var :)
          range <- metaexpr
          void $ match' TokPipe
          modify tail
          pure range

      case range of
        Nothing -> pure Nothing
        Just (cond, protorange, taint0) -> do
          let
            Location (rfrom, _) = E.loc cond

          case protorange of
            ProtoVar -> do
              putError rfrom . UnknownError $
                "Bad collection range. Range must be a boolean expression \
                \in Conjunctive Normal Form where the variable `" <> unpack var <>
                "` is bounded."
              pure Nothing
            ProtoNothing -> do
              putError rfrom . UnknownError $
                "Bad collection range. Range must be a boolean expression \
                \in Conjunctive Normal Form where the variable `" <> unpack var <>
                "` is bounded."
              pure Nothing
            ProtoLow _ -> do
              putError rfrom . UnknownError $
                "Bad collection range. No upper bound was given."
              pure Nothing
            ProtoHigh _ -> do
              putError rfrom . UnknownError $
                "Bad collection range. No lower bound was given."
              pure Nothing
            ProtoQRange qrange ->
              pure $ Just (var, varTy, qrange, cond)

    quantifiableTypes, allowedCollTypes :: Type
    quantifiableTypes = GOneOf [ GInt, GChar ]
    allowedCollTypes  = GOneOf
      [ GInt
      , GChar
      , GTuple $ Seq.fromList
        [ GOneOf [GInt, GChar]
        , GOneOf [GInt, GChar, GFloat]] ]

    declaration = do
      from <- getPosition

      var <- identifier
      void $ match TokColon
      tname <- identifier

      to <- getPosition

      let loc = Location (from, to)

      typeEntry <- Map.lookup tname <$> lift (asks nativeTypes)

      case typeEntry of
        Nothing -> do
          putError from . UnknownError $ ("type `" <> unpack tname <> "` does not exist" )
          pure (var, GUndef)

        Just (t, _) ->
          if t =:= quantifiableTypes
            then do
              lift $ symbolTable %= insertSymbol var Entry
                { _entryName = var
                , _loc = Location (from, to)
                , _info = Var
                  { _varType  = t
                  , _varValue = Nothing
                  , _varConst = True }}
              pure (var, t)
            else do
              putError from . UnknownError $
                "type `" <> unpack tname <> "` is not quantifiable."
              pure (var, GUndef)

    elems :: Token -> ParserExp (Maybe (Seq Expression, Type))
    elems close =  lookAhead (match close) *> pure (Just (Seq.empty, GAny))
               <|> (elem' close `sepBy1'` match TokComma) (Just (Seq.empty, allowedCollTypes))

    elem' :: Token
          -> Maybe (Seq Expression, Type)
          -> ParserExp (Maybe (Seq Expression, Type))
    elem' close acc = do
      pos <- getPosition
      el <- metaexpr `followedBy` oneOf ([close, TokComma] :: [Token])
      case el of
        Nothing ->
          pure Nothing
        Just (e, _, _) -> case acc of
          Nothing -> pure Nothing
          Just (els, t) -> case t <> expType e of
            GUndef -> do
              putError pos . UnknownError $
                "Unexpected expression of type `" <> show (expType e) <> "`,\
                \expected instead an expression of type `" <> show t <> "`."
              pure Nothing
            newType -> pure $ Just (els |> e, newType)


callOrVariable :: ParserExp (Maybe MetaExpr)
callOrVariable = do
  (name, loc) <- identifierAndLoc
  tok <- lookAhead anyToken
  case tok of
    TokLeftPar -> call name loc
    _          -> variable name loc


variable :: Text -> Location -> ParserExp (Maybe MetaExpr)
variable name (Location (from, to)) = do
  st <- lift (use symbolTable)

  let loc = Location (from, to)

  maybeStruct <- lift (use currentStruct)

  abstractSt <- case maybeStruct of
    Just (GDataType _ (Just abstName) _, _, _) -> do
      adt <- getStruct abstName
      case adt of
        Just abst -> do
          pure $ structSt abst
        _ -> pure emptyGlobal
    _ -> pure emptyGlobal


  let
    entry = case name `lookup` st of
      Left _ -> name `lookup` abstractSt
      x      -> x

  case entry of

    Left _ -> do
      putError from . UnknownError $
        "Variable `" <> unpack name <> "` not defined in this scope."
      pure Nothing

    Right entry -> case entry^.info of
      Var { _varType, _varConst } -> do
        rangevars <- get

        let expr = Expression
              { E.loc
              , expType = _varType
              , expConst = _varConst || name `elem` rangevars
              , exp' = Obj
                { theObj = Object
                  { O.loc
                  , objType = _varType
                  , obj' = Variable
                    { O.name = name
                    , mode = Nothing }}}}

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

        pure $ Just (expr, protorange, taint)

      SelfVar { _selfType } -> do
        struct <- lift $ use currentStruct
        let
          expr = case struct of
            Just (GDataType structName' abstract t, mapTypes, _) ->
              case name `Map.lookup` mapTypes of
                Just (i, _, _) -> Expression
                  { loc
                  , expType  = _selfType
                  , expConst = False
                  , exp'     = Obj
                    { theObj = Object
                      { loc
                      , objType = _selfType
                      , obj' = Member
                        { field = i
                        , fieldName = name
                        , inner = Object
                          { loc
                          , objType = GDataType structName' abstract t
                          , obj' = Variable
                            { O.name = pack "self"
                            , mode = Nothing }}}}}}

                Nothing -> error $ "Internal error: Data Type variable `" <>
                            unpack name <>"` not found"
            Nothing -> error "Internal error: Data Type not found"

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

        pure $ Just (expr, protorange, taint)

      -- Const { _constType, _constValue } ->
      --   let
      --     expr = Expression
      --       { E.loc
      --       , expType  = _constType
      --       , expConst = True
      --       , exp' = Value _constValue }
      --
      --   in pure $ Just (expr, ProtoNothing, Taint False)

      Argument { _argMode, _argType } ->
        let
          expr = Expression
            { E.loc
            , expType  = _argType
            , expConst = case _argMode of
              In -> True
              _  -> False
            , exp'     = Obj
              { theObj = Object
                { O.loc
                , objType = _argType
                , obj'    = Variable
                  { O.name = name
                  , mode   = Just _argMode }}}}

        in pure $ Just (expr, ProtoNothing, Taint False)


call :: Text -> Location -> ParserExp (Maybe MetaExpr)
call fName (Location (from,_)) = do
  args <- between (match TokLeftPar) (match' TokRightPar) $
    metaexpr `sepBy` match TokComma

  to <- getPosition
  let loc = Location (from, to)

  defs <- lift (use definitions)
  case fName `Map.lookup` defs of
    Just Definition { defLoc, def' = FunctionDef { funcParams, funcRetType, funcRecursive }} -> do
      let
        nArgs   = length args
        nParams = length funcParams
        Location (pos, _) = defLoc
      if nArgs == nParams
        then do
          args' <- foldM (checkType fName pos)
            (Just (Seq.empty, Taint False, True))
            (Seq.zip args funcParams)
          pure $ case args' of
            Nothing -> Nothing
            Just (fArgs, taint, const') ->
              let
                expr = Expression
                  { E.loc
                  , expType  = funcRetType
                  , expConst = const'
                  , exp' = FunctionCall
                    { fName
                    , fArgs
                    , fRecursiveCall = False
                    , fRecursiveFunc = funcRecursive
                    , fStructArgs = Nothing }}
              in Just (expr, ProtoNothing, taint)

        else do
          putError from BadFuncNumberOfArgs
            { fName
            , fPos = pos
            , nParams
            , nArgs }
          pure Nothing

    Just Definition { def' = GracielaFunc { signatures, casts } } ->
      case sequence args of
        Nothing -> pure Nothing
        Just args' -> do
          let
            aux (es, ts, c0, t0) (e@Expression { expConst, expType }, _, t1) =
              (es |> e, ts |> expType, c0 && expConst, t0 <> t1)
            (fArgs', types, const', taint) =
              foldl' aux (Seq.empty, Seq.empty, True, Taint False) args'
            i64cast e@Expression { E.loc, expType, expConst, exp' } =
              e { exp' = I64Cast e, expType = I64 }
            fArgs = fArgs' & elements (`elem` casts) %~ i64cast
            SourcePos _ line col = from
            pos' = Expression loc GInt True . Value . IntV . fromIntegral . unPos <$>
              [ line, col ]

          case signatures types of
            Right (funcRetType, fName, canAbort) ->
              let
                expr = Expression
                  { E.loc
                  , expType  = funcRetType
                  , expConst = const'
                  , exp' = FunctionCall
                    { fName
                    , fArgs = fArgs <> if canAbort
                      then pos'
                      else []
                    , fRecursiveCall = False
                    , fRecursiveFunc = False
                    , fStructArgs = Nothing }}
              in pure $ Just (expr, ProtoNothing, taint)

            Left message -> do
              putError from message
              pure Nothing

    Just Definition { defLoc, def' = ProcedureDef {} } -> do
      putError from . UnknownError $
        "Cannot call procedure `" <> unpack fName <> "`, defined at " <>
        show defLoc <> " as an expression; a function was expected."
      pure Nothing

    Nothing -> do
      let
        nArgs = length args
        f = case hasDTType args of

          Nothing -> do
            let
              args' = sequence args
            case args' of
              Nothing -> do
                putError from . UnknownError $ "Calling function `" <>
                  unpack fName <>"` with bad arguments"
                pure Nothing
              Just args'' -> do
                putError from . UndefinedFunction fName $ (\(e,_,_) -> e) <$> args''
                pure Nothing

          Just (GFullDataType name typeArgs') -> do
            lift (use dataTypes) >>= \dts -> case name `Map.lookup` dts of
              Nothing -> internal "impossible call to struct function"
              Just Struct { structProcs } -> do
                case fName `Map.lookup` structProcs of
                  Just Definition{ def'=FunctionDef{ funcParams, funcRetType, funcRecursive }} -> do
                    cs <- lift $ use currentStruct
                    let
                      nParams = length funcParams
                      typeArgs = case cs of
                          Nothing -> typeArgs'
                          Just (GDataType _ _ dtArgs,_,_) ->
                            fmap (fillType dtArgs) typeArgs'

                    when (nArgs /= nParams) . putError from . UnknownError $
                      "Calling function `" <> unpack fName <> "` with a bad number of arguments."

                    args' <- foldM (checkType' typeArgs fName from)
                        (Just (Seq.empty, Taint False, True))
                        (Seq.zip args funcParams )
                    pure $ case args' of
                      Nothing -> Nothing
                      Just (fArgs, taint, const') -> do
                        let
                          expr = Expression
                            { E.loc
                            , expType = fillType typeArgs' funcRetType
                            , expConst = const'
                            , exp' = FunctionCall
                              { fName
                              , fArgs
                              , fRecursiveCall = False
                              , fRecursiveFunc = funcRecursive
                              , fStructArgs    = Just (name, typeArgs) }}

                        Just (expr, ProtoNothing, taint)

                  _ -> do
                    putError from . UnknownError $
                      "Data Type `" <> unpack name <>
                      "` does not have a function called `" <>
                      unpack fName <> "`"
                    return Nothing

          Just t@(GDataType name _ _) -> do
            Just (GDataType {typeArgs}, _, structProcs) <- lift $ use currentStruct
            case fName `Map.lookup` structProcs of
              Just Definition {def' =
                FunctionDef{ funcParams, funcRetType, funcRecursive }} -> do
                let
                  nParams = length funcParams

                when (nParams /= nArgs) . putError from . UnknownError $
                    "Calling procedure `" <> unpack fName <>
                    "` with a bad number of arguments."

                args' <- foldM (checkType' typeArgs fName from)
                  (Just (Seq.empty, Taint False, True))
                  (Seq.zip args funcParams )

                pure $ case args' of
                  Nothing -> Nothing
                  Just (fArgs, taint, const') -> do
                    let
                      expr = Expression
                        { E.loc
                        , expType = funcRetType
                        , expConst = const'
                        , exp' = FunctionCall
                          { fName
                          , fArgs
                          , fRecursiveCall = False
                          , fRecursiveFunc = funcRecursive
                          , fStructArgs    = Just (name, typeArgs)}}

                    Just (expr, ProtoNothing, taint)

              Nothing -> do
                putError from . UnknownError $
                  "Data Type `" <> unpack name <>
                  "` does not have a procedure called `" <>
                  unpack fName <> "`"
                return Nothing

      -- If the function is not defined, it's possible that we're
      -- dealing with a recursive call. The information of a function
      -- that is being defined is stored temporarily at the
      -- Parser.State `currentFunc`.
      currentFunction <- lift (use currentFunc)
      case currentFunction of
        Just cr@CurrentRoutine {}
          | cr^.crName == fName && cr^.crRecAllowed -> do
            let
              nArgs = length args
              nParams = length (cr^.crParams)

            if nArgs == nParams
              then do
                args' <- foldM (checkType fName (cr^.crPos))
                  (Just (Seq.empty, Taint False, True))
                  (Seq.zip args (cr^.crParams))

                lift $ (currentFunc . _Just . crRecursive) .= True

                fStructArgs <- do
                  cs'      <- lift $ use currentStruct
                  typeArgs <- lift $ use typeVars
                  case cs' of
                    Nothing -> pure Nothing
                    Just (GDataType name _ _, _, _) -> pure $ Just
                      ( name
                      , Array.listArray (0, length typeArgs - 1) $
                        zipWith GTypeVar [0..] typeArgs)

                pure $ case args' of
                  Nothing -> Nothing
                  Just (fArgs, taint, const') ->
                    let
                      expr = Expression
                        { E.loc
                        , expType  = cr^.crType
                        , expConst = const'
                        , exp' = FunctionCall
                          { fName
                          , fArgs
                          , fRecursiveCall = True
                          , fRecursiveFunc = True
                          , fStructArgs }}
                    in Just (expr, ProtoNothing, taint)
              else do
                putError from BadFuncNumberOfArgs
                  { fName
                  , fPos  = cr^.crPos
                  , nParams
                  , nArgs }
                pure Nothing
          | cr^.crName == fName && not (cr^.crRecAllowed) -> do
            putError from . UnknownError $
              "Function `" <> unpack fName <> "` cannot call itself \
              \recursively because no bound was given for it."
            pure Nothing
          | otherwise -> f

        Nothing -> f

  where
    hasDTType = getFirst . foldMap aux
    aux (Just (Expression { expType },_,_)) = First $ hasDT expType
    aux Nothing                             = First Nothing
    checkType = checkType' (Array.listArray (0,-1) [])

    checkType' _ _ _ _ (Nothing, _) = pure Nothing
    checkType' typeArgs fName fPos acc
      (Just (e@Expression { E.loc, expType,expConst, exp' }, _, taint), (name, pType)) = do
        let
          Location (from, _) = loc
          pType' = fillType typeArgs pType
        if  pType' =:= expType
          then do
            let
              type' = case expType of
                GPointer GAny -> pType'
                _             -> expType

            pure $ add e{expType = type'} taint expConst <$> acc
          else do
            putError from $
              BadFunctionArgumentType name fName fPos pType expType
            pure Nothing
    add e taint1 const1 (es, taint0, const0) =
      (es |> e, taint0 <> taint1, const0 && const1)


quantification :: ParserExp (Maybe MetaExpr)
quantification = do
  from <- getPosition
  void $ match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, mvart) <- declaration
  void $ match TokPipe

  modify (var:)
  range <- {-safe-} metaexpr
  void $ match TokPipe
  modify tail

  case range of
    Nothing -> pure ()
    Just (cond, protorange, taint0) -> do
      let
        Location (rfrom, _) = E.loc cond

      case protorange of
        ProtoVar ->
          putError rfrom . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoNothing       ->
          putError rfrom . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoLow         _ ->
          putError rfrom . UnknownError $
            "Bad quantification range. No upper bound was given."
        ProtoHigh        _ ->
          putError rfrom . UnknownError $
            "Bad quantification range. No lower bound was given."
        ProtoQRange qrange ->
          pure ()

  body <- {-safe-} metaexpr

  case body of
    Nothing -> pure ()
    Just (Expression { expType = bodyType, E.loc = Location (bfrom, _) }, _, taint1) ->
      unless (bodyType =:= allowedBType) .
        putError bfrom . UnknownError $
          "Bad quantification body. Body must be " <> show allowedBType <> "."

  void $ match TokRightPercent

  to <- getPosition
  lift $ symbolTable %= closeScope to

  case mvart of
    Nothing -> pure Nothing
    Just t -> case body of
      Nothing -> pure Nothing
      Just (theBody @ Expression { expType = bodyType, expConst = bodyConst }, _, taint1) ->
        case range of
          Nothing -> pure Nothing
          Just (cond@Expression {expConst = condConst}, protorange, taint0) ->
            case protorange of
              ProtoQRange qRange -> case bodyType <> allowedBType of
                GUndef  -> pure Nothing
                newType ->
                  let
                    loc = Location (from, to)
                    taint = taint0 <> taint1
                    expr = Expression
                      { E.loc
                      , expType = case q of
                        Count -> GInt
                        _     -> newType
                      , expConst = condConst && bodyConst
                      , exp' = Quantification
                        { qOp      = q
                        , qVar     = var
                        , qVarType = t
                        , qRange
                        , qCond    = cond
                        , qBody    = theBody }}
                  in pure $ Just (expr, ProtoNothing, taint)
              _ -> pure Nothing

  where
    numeric = GOneOf [ GChar, GInt, GFloat ]

    quantifier =  (match TokForall                    $> (ForAll,    GBool))
              <|> (match TokExist                     $> (Exists,    GBool))
              <|> (match TokPi                        $> (Product,   numeric))
              <|> (match TokSigma                     $> (Summation, numeric))
              <|> (match TokMax                       $> (Maximum,   numeric))
              <|> (match TokMin                       $> (Minimum,   numeric))
              <|> ((match TokCount <|> match TokHash) $> (Count,     GBool))

    quantifiableTypes = GOneOf [ GInt, GChar ]

    declaration = do
      from <- getPosition

      var <- identifier
      void $ match TokColon
      tname <- identifier

      to <- getPosition

      let loc = Location (from, to)

      typeEntry <- Map.lookup tname <$> lift (asks nativeTypes)

      case typeEntry of
        Nothing -> do
          putError from . UnknownError $ ("type `" <> unpack tname <> "` does not exist" )
          pure (var, Nothing)

        Just (t,_) ->
          if t =:= quantifiableTypes
            then do
              lift $ symbolTable %= insertSymbol var Entry
                { _entryName = var
                , _loc = Location (from, to)
                , _info = Var
                  { _varType  = t
                  , _varValue = Nothing
                  , _varConst = True }}
              pure (var, Just t)
            else do
              putError from . UnknownError $
                "type `" <> unpack tname <> "` is not quantifiable"
              pure (var, Nothing)


data IfBuilder
  = IfGuards     (Seq (Expression, Expression)) (Maybe Expression)
  | IfExp        Expression
  | IfNothing
  deriving (Show)

instance Semigroup IfBuilder where
  IfNothing <> _ = IfNothing
  _ <> IfNothing = IfNothing

  IfExp e <> _ = IfExp e

  IfGuards gs0 Nothing <> IfGuards gs1 t =
    IfGuards (gs0 <> gs1) t
  IfGuards gs Nothing  <> IfExp e =
    IfGuards gs (Just e)

  igt@(IfGuards _ (Just _)) <> _ = igt

instance Monoid IfBuilder where
  mempty = IfGuards Seq.empty Nothing
  mappend = (<>)

data IfState = IfState
  { ifType    :: Type
  , ifBuilder :: IfBuilder
  , ifTaint   :: Taint
  , ifConst   :: Bool }

initialIfState = IfState
  { ifType    = GAny
  , ifBuilder = IfGuards Seq.empty Nothing
  , ifTaint   = Taint False
  , ifConst   = False }


ifExp :: ParserExp (Maybe MetaExpr)
ifExp = do
  from <- getPosition
  void $ match TokIf

  -- The StateT on top of ParserExp allows us to manage
  -- a state local to this level of abstraction.
  IfState { ifType, ifBuilder, ifTaint, ifConst } <- execStateT guards initialIfState

  match' TokFi

  to <- getPosition

  let loc = Location (from, to)

  case ifBuilder of
    IfNothing -> pure Nothing
    IfExp e ->
      pure $ Just (e { E.loc, expType = ifType }, ProtoNothing, ifTaint)
    IfGuards gs t ->
      let
        expr = Expression
          { E.loc
          , expType  = ifType
          , expConst = ifConst
          , exp' = EConditional gs t }
      in pure $ Just (expr, ProtoNothing, ifTaint)

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
        Nothing -> pure (Nothing, st, Taint False)
        Just (l, _, taint0) ->
          case l of
            e @ Expression { expType = GBool, E.loc } ->
            -- 1. We have a good boolean expression, which is ideal
              pure (Just e, st, taint0)

            Expression { E.loc = Location (from, _) } -> do
              -- 2. We have a good expression which isn't boolean, so we
              -- report the error and clear the previous guards
              lift . putError from . UnknownError $
                "bad left side in conditional expression"
              pure (Nothing, st, taint0)

        -- badEXPRESSION { E.loc } ->
        -- -- 3. We have a bad expression, which means there was an error
        -- -- before which we can only propagate
        --   pure (Nothing, st, taint0)


    rhs (ml, st@IfState { ifType, ifBuilder, ifTaint, ifConst }, taint0) = do
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

                Expression { E.loc = Location (rfrom,_), expType } ->
                  case expType <> ifType of
                    GUndef -> do
                      -- 2. The rhs type doesn't match previous lines
                      lift . putError rfrom . UnknownError $
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
                              { ifType = newType
                              , ifBuilder = ifBuilder <> IfExp r
                              , ifTaint = taint1
                              , ifConst = expConst r }

                          | exp' l == Value (BoolV False) ->
                            -- 5. We have a good rhs that must be ignored because
                            -- its lhs was false. Its type does affect the ifExp.
                            put st { ifType = newType }

                          | otherwise ->
                            -- 6. We have a good rhs whose type matches the
                            -- type of previous guards, while the lhs value will be
                            -- known only at runtime, so we leave this guard
                            -- expressed as a tuple. (In the case of the first
                            -- guard, the match is done against GAny, i.e., any
                            -- type will match).
                            put IfState
                              { ifType    = newType
                              , ifBuilder =
                                ifBuilder <> IfGuards (Seq.singleton (l, r)) Nothing
                              , ifTaint   = ifTaint <> taint0 <> taint1
                              , ifConst   = ifConst && expConst l && expConst r }


operator :: [[ Operator ParserExp (Maybe MetaExpr) ]]
operator =
  [ {-Level 0-}
    [ Postfix (foldr1 (>=>) <$> some dotField) ]
  , {-Level 1-}
    [ Prefix  (foldr1 (>=>) <$> some deref) ]
  , {-Level 2-}
    [ Postfix (foldr1 (>=>) <$> some subindex) ]
  , {-Level 3-}
    [ Postfix (foldr1 (>=>) <$> some subindex) ]
  , {-Level 4-}
    [ Prefix (match TokNot        <&> unary Op.not    )
    , Prefix (match TokMinus      <&> unary Op.uMinus ) ]
  , {-Level 5-}
    [ InfixR (match TokPower      <&> binary Op.power ) ]
  , {-Level 6-}
    [ InfixL (match TokTimes      <&> binary Op.times )
    , InfixL (match TokDiv        <&> binary' Op.div  )
    , InfixL (match TokMod        <&> binary' Op.mod  ) ]
  , {-Level 7-}
    [ InfixL (match TokPlus         <&> binary Op.plus       )
    , InfixL (match TokMinus        <&> binary Op.bMinus     )
    , InfixL (match TokSetUnion     <&> binary Op.union      )
    , InfixL (match TokMultisetSum  <&> binary Op.multisum   )
    , InfixL (match TokSetIntersect <&> binary Op.intersect  )
    , InfixL (match TokSetMinus     <&> binary Op.difference )
    , InfixL (match TokConcat       <&> binary Op.concat     ) ]
  , {-Level 8-}
    [ InfixL (match TokMax        <&> binary Op.max    )
    , InfixL (match TokMin        <&> binary Op.min    ) ]
  , {-Level 9-}
    [ InfixL (match TokHash       <&> binary Op.seqAt    )
    , InfixL (match TokAtSign     <&> binary Op.bifuncAt ) ]
  , {-Level 10-}
    [ InfixN (match TokElem       <&> membership             )
    , InfixN (match TokNotElem    <&> binary Op.notElem      )
    , InfixN (match TokLT         <&> comparison Op.lt       )
    , InfixN (match TokLE         <&> comparison Op.le       )
    , InfixN (match TokGT         <&> comparison Op.gt       )
    , InfixN (match TokGE         <&> comparison Op.ge       )
    , InfixN (match TokSubset     <&> binary     Op.subset   )
    , InfixN (match TokSSubset    <&> binary     Op.ssubset  )
    , InfixN (match TokSuperset   <&> binary     Op.superset )
    , InfixN (match TokSSuperset  <&> binary     Op.ssuperset) ]
  , {-Level 11-}
    [ InfixN (match TokAEQ        <&> pointRange    )
    , InfixN (match TokANE        <&> binary Op.ane ) ]
  , {-Level 12-}
    [ InfixR (match TokAnd        <&> conjunction ) ]
  , {-Level 13-}
    [ InfixR (match TokOr         <&> binary' Op.or ) ]
  , {-Level 14-}
    [ InfixR (match TokImplies    <&> binary' Op.implies    )
    , InfixL (match TokConsequent <&> binary' Op.consequent ) ]
  , {-Level 15-}
    [ InfixN (match TokBEQ        <&> binary' Op.beq )
    , InfixN (match TokBNE        <&> binary' Op.bne ) ]
  ]


subindex :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
subindex = do
  from' <- getPosition
  -- subind <- between (match TokLeftBracket) (match' TokRightBracket) metaexpr
  subindices' <- between
    (match TokLeftBracket)
    (match' TokRightBracket)
    (subAux `sepBy` match TokComma)
  to <- getPosition

  let subindices = sequence subindices'

  case subindices of
    Nothing -> pure (\_ -> pure Nothing)
    Just subs -> if null subs
      then pure $ \case
        Just (Expression { expType, loc }, _, _) -> case expType of
          GArray _ _ -> do
            putError (pos loc) . UnknownError $
              "Missing dimensions in array access."
            pure Nothing
          _ -> do
            putError (pos loc) . UnknownError $ "Cannot subindex non-array."
            pure Nothing
        Nothing -> pure Nothing
      else pure $ \case
        Nothing -> pure Nothing
        Just (expr, _, taint1) -> case expr of
          Expression
            { E.loc = Location (from, _)
            , expType = GArray { dimensions, innerType }
            , exp' = Obj o } -> do
              let
                lsubs = length subs
                ldims = length dimensions
              if lsubs /= ldims
                then do
                  putError from . UnknownError $
                    "Attempted to index " <> show ldims <>"-dimensional array \
                    \with a " <> show lsubs <> "-dimensional subindex."
                  pure Nothing
                else
                  let
                    taint = foldMap (view _3) subs <> taint1
                    expr = Expression
                      { E.loc = Location (from, to)
                      , expType = innerType
                      , expConst = False
                      , exp' = Obj
                        { theObj = Object
                          { O.loc = Location (from, to)
                          , objType = innerType
                          , obj' = Index
                            { O.inner = o
                            , indices = view _1 <$> subs }}}}
                  in pure $ Just (expr, ProtoNothing, taint)

          _ -> do
            putError (pos . E.loc $ expr) . UnknownError $
              "Cannot subindex non-array."

            pure Nothing

  where
    subAux :: ParserExp (Maybe MetaExpr)
    subAux = do
      e <- metaexpr
      case e of
        je@(Just (Expression { expType = GInt }, _, _)) -> pure je
        Just (Expression { expType, loc }, _, _) -> do
          putError (pos loc) . UnknownError $
            "Cannot use expression of type `" <> show expType <>
            " as subindex, integer expression was expected`."
          pure Nothing
        Nothing -> pure Nothing


dotField :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
dotField = do
  lookAhead $ match TokDot
  from' <- getPosition
  match TokDot
  fieldName' <- safeIdentifier

  to <- getPosition

  case fieldName' of
    Nothing -> pure (\_ -> pure Nothing)
    Just fieldName -> pure $ \case
      Nothing -> pure Nothing
      Just (e@Expression { exp', loc }, _, taint) -> do
        let Location (from,_) = loc
        case exp' of
          (Obj obj) -> do
            case objType obj of
              GDataType n _ typeArgs-> do
                cstruct <- lift $ use currentStruct
                case cstruct of
                  Just (GDataType name _ _, structFields, _)
                    | name == n ->
                      aux obj (objType obj) loc fieldName structFields taint
                  _ -> do
                    structs <- lift $ use dataTypes
                    case n `Map.lookup` structs of
                      Just Struct { structFields } ->
                        let structFields' = fillTypes typeArgs structFields
                        in aux obj (objType obj) loc fieldName structFields' taint
                      _ -> internal "GDataType without struct."

              GFullDataType n typeArgs -> do
                dts <- lift $ use dataTypes
                case n `Map.lookup` dts of
                  Nothing -> do
                    pure Nothing
                  Just Struct { structFields } ->
                    let structFields' = fillTypes typeArgs structFields
                    in aux obj (objType obj) loc fieldName structFields' taint
              t -> do
                putError from' . UnknownError $
                  "Bad field access. Cannot access an expression \
                  \of type " <> show t <> "."
                pure Nothing
          _ -> do
            putError from' . UnknownError $
              "Bad field access. Cannot access an expression."
            pure Nothing
  where
    aux o oType loc fieldName structFields taint =
      case fieldName `Map.lookup` structFields of
        Just (i, t, _) ->
          let
            expr = Expression
              { loc
              , expType  = t
              , expConst = False
              , exp'     = Obj
                { theObj = Object
                  { loc
                  , objType = t
                  , obj' = Member
                    { inner = o
                    , field = i
                    , fieldName }}}}
          in pure $ Just (expr, ProtoNothing, taint)
        Nothing -> do
          let Location (pos, _) = loc
          putError pos . UnknownError $
            "Bad field access. Object of type `" <> show oType <>
            "` does not have a field named `" <>
            unpack fieldName <> "`"
          pure Nothing

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
                , expConst = False
                , exp' = Obj
                  { theObj = Object
                    { O.loc = Location (from, to)
                    , objType = pointerType
                    , obj' = Deref
                      { O.inner = o }}}}
          in pure $ Just (expr, ProtoNothing, taint)

      e -> do
        let
          Location (_, to) = E.loc e
          loc = Location (from, to)

        putError from . UnknownError $ "Cannot deref non-pointer."

        pure Nothing

unary :: Op.Un -> Location
      -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
unary unOp
  opLoc @ (Location (from,_))
  (Just (i @ Expression { expType = itype, exp', expConst }, _, taint))
  = case Op.unType unOp itype of
    Left expected -> do
      let loc = Location (from, to i)
      putError from . UnknownError $
        "Operator `" <> show (Op.unSymbol unOp) <> "` at " <> show opLoc <>
        " expected an expression of type " <> expected <>
        ",\n\tbut received " <> show itype <> "."
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
          , expConst
          , exp' = exp'' }
      pure $ Just (expr, ProtoNothing, taint)

unary _ _ _ = pure Nothing


binary :: Op.Bin -> Location
       -> Maybe MetaExpr -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
binary _ _ Nothing _ = pure Nothing
binary _ _ _ Nothing = pure Nothing
binary binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc, exp' = lexp }, _, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc, exp' = rexp }, _, rtaint))
  = case Op.binType binOp ltype rtype of

    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <> show opLoc <>
          "\n\texpected two expressions of types " <> expected <>
          ",\n\tbut received " <> show (ltype, rtype) <> ".")
      pure Nothing

    Right ret ->
      let
        taint = ltaint <> rtaint

        exp' = case (lexp, rexp) of
          -- (Value v, Value w) ->
          --   Value $ Op.binFunc binOp v w
          _ -> Binary
            { binOp = Op.binSymbol binOp
            , lexpr = l
            , rexpr = r }

        expr = Expression
          { E.loc = Location (from l, to r)
          , expType = ret
          , expConst = lc && rc
          , exp' }

      in pure $ Just (expr, ProtoNothing, taint)


binary' :: Op.Bin' -> Location
        -> Maybe MetaExpr -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
binary' _ _ Nothing _ = pure Nothing
binary' _ _ _ Nothing = pure Nothing
binary' binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc, exp' = lexp }, _, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc, exp' = rexp }, _, rtaint))
  = case Op.binType' binOp ltype rtype of

    Left expected -> do
      putError (from l) . UnknownError $
        ("Operator `" <> show (Op.binSymbol' binOp) <> "` at " <> show opLoc <>
          " expected two expressions of types " <> expected <>
          ", but received " <> show (ltype, rtype) <> ".")
      pure Nothing

    Right ret ->
      let
        taint = ltaint <> rtaint

        expr = Op.binFunc' binOp l r

        range = if exp' expr == Value (BoolV False)
          then ProtoQRange EmptyRange
          else ProtoNothing

      in pure $ Just (expr, range, taint)


membership :: Location
           -> Maybe MetaExpr -> Maybe MetaExpr
           -> ParserExp (Maybe MetaExpr)
membership opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
  = case Op.binType Op.elem ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (SetRange r), Taint False)

    Right _ -> internal "impossible membership type"

membership opLoc l r = binary Op.elem opLoc l r


comparison :: Op.Bin -> Location
           -> Maybe MetaExpr -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
comparison binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, _, Taint False))
  (Just (r @ Expression { expType = rtype, expConst = rc }, ProtoVar, _))
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError (from l) . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> "\n\texpected two expressions of types " <> expected <>
            ",\n\tbut received " <> show (ltype, rtype) <> ".")
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoLow  (succ' l)
            LE -> ProtoLow  l
            GT -> ProtoHigh (pred' l)
            GE -> ProtoHigh l
            _  -> internal "impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , expConst = lc && rc
            , exp'     = eSkip }
        in pure $ Just (expr, range, Taint True)

      Right _ ->
        internal "impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, expConst, exp' } =
      Expression { E.loc, expType, expConst, exp' = Unary op e }

comparison binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, _))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError (from l) . UnknownError $
          ("Operator `" <> show (Op.binSymbol binOp) <> "` at " <>
            show opLoc <> "\n\texpected two expressions of types " <> expected <>
            ",\n\tbut received " <> show (ltype, rtype) <> ".")
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoHigh (pred' r)
            LE -> ProtoHigh r
            GT -> ProtoLow  (succ' r)
            GE -> ProtoLow  r
            _  -> internal "impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , expConst = lc && rc
            , exp'     = eSkip }
        in pure $ Just (expr, range, Taint True)

      Right _ ->
        internal "impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, expConst, exp' } =
      Expression { E.loc, expType, expConst, exp' = Unary op e }

comparison binOp opLoc l r = binary binOp opLoc l r


pointRange :: Location
           -> Maybe MetaExpr -> Maybe MetaExpr
           -> ParserExp (Maybe MetaExpr)
pointRange opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (PointRange r), Taint False)

    Right _ -> internal "impossible type equality"

pointRange opLoc
  (Just (l @ Expression { expType = rtype, expConst = lc }, _, Taint False))
  (Just (r @ Expression { expType = ltype, expConst = rc }, ProtoVar, ltaint))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        ("Operator `" <> show Elem <> "` at " <> show opLoc <> " expected two\
          \ expressions of types " <> expected <> ",\n\tbut received " <>
          show (ltype, rtype) <> ".")
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (PointRange l), Taint False)

    Right _ -> internal "impossible type equality"

pointRange opLoc l r = binary Op.aeq opLoc l r


conjunction :: Location
            -> Maybe MetaExpr -> Maybe MetaExpr
            -> ParserExp (Maybe MetaExpr)
conjunction _ Nothing _ = pure Nothing
conjunction _ _ Nothing = pure Nothing
conjunction opLoc
  l@(Just (Expression { }, ProtoNothing, ltaint))
  r@(Just (Expression { }, ProtoNothing, rtaint))
  = binary' Op.and opLoc l r
conjunction opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc, exp' = lexp' }, lproto, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc, exp' = rexp' }, rproto, rtaint))
  = case Op.binType' Op.and ltype rtype of
    Right GBool -> do
      varname <- gets head
      let
        taint = ltaint <> rtaint
        (conds :: Seq Expression, range) = case (lproto, rproto) of
          (ProtoVar, _) -> internal "boolean ProtoVar"
          (_, ProtoVar) -> internal "boolean ProtoVar"

          (q @ (ProtoQRange EmptyRange), _) ->
            ([wrap eSkip], q)
          (_, q @ (ProtoQRange EmptyRange)) ->
            ([wrap eSkip], q)

          (ProtoNothing, proto) ->
            ([l, r], proto)
          (proto, ProtoNothing) ->
            ([l, r], proto)

          (ProtoLow low, ProtoHigh high) ->
            ([l, r], ProtoQRange (ExpRange low high))
          (ProtoHigh high, ProtoLow low) ->
            ([l, r], ProtoQRange (ExpRange low high))
          (ProtoLow  llow,  ProtoLow  rlow ) ->
            ([l, r], ProtoLow  (joinProtos Max llow rlow))
          (ProtoHigh lhigh, ProtoHigh rhigh) ->
            ([l, r], ProtoHigh (joinProtos Min lhigh rhigh))

          (ProtoQRange lr @ ExpRange {}, ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRanges lr rr)

          (ProtoQRange lr @ ExpRange {}, rr @ ProtoLow {}) ->
            ([l, r], joinExpRangeProto lr rr)
          (ProtoQRange lr @ ExpRange {}, rr @ ProtoHigh {}) ->
            ([l, r], joinExpRangeProto lr rr)
          (lr @ ProtoLow {} , ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRangeProto rr lr)
          (lr @ ProtoHigh {}, ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRangeProto rr lr)

          (point @ (ProtoQRange PointRange {}), proto) ->
            ([wrap $ rebuild varname proto, l, r], point)
          (proto, point @ (ProtoQRange PointRange {})) ->
            ([wrap $ rebuild varname proto, l, r], point)

          (set @ (ProtoQRange SetRange {}), proto) ->
            ([wrap $ rebuild varname proto, l, r], set)
          (proto, set @ (ProtoQRange SetRange {})) ->
            ([wrap $ rebuild varname proto, l, r], set)

        expr = foldr1 joinCond conds

      pure $ Just (expr, range, taint)

    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        "Operator `" <> show And <> "` at " <> show opLoc <> " expected two\
        \ expressions of types " <> expected <> ",\n\tbut received " <>
        show (ltype, rtype) <> "."
      pure Nothing

    Right _ -> internal "Bad andOp type"

    where
      loc = E.loc l <> E.loc r

      wrap exp' = Expression
        { E.loc
        , expType  = GBool
        , expConst = False
        , exp' }

      joinCond = Op.binFunc' Op.and

      joinProtos binOp l r = Expression
        { E.loc
        , expType  = expType l
        , expConst = False
        , exp'     = Binary binOp l r }

      joinExpRanges l@ExpRange {} ExpRange { low, high }
        = let ProtoQRange l' = joinExpRangeProto l (ProtoLow low)
          in  joinExpRangeProto l' (ProtoHigh high)

      joinExpRanges _ _ = internal "can only join two ExpRanges"

      joinExpRangeProto
        ExpRange { low = elow, high }
        (ProtoLow plow)
        = let
            t = expType elow
            low = Expression
              { E.loc = Location
                (from elow `min` from plow, to elow `max` to plow)
              , expType = t
              , expConst = False
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
              , expConst = False
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
            _           -> internal "impossible set type"
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
            { E.loc
            , expType = GBool
            , expConst = False
            , exp' = lexpr' }
          , rexpr = Expression
            { E.loc
            , expType = GBool
            , expConst = False
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
          { E.loc
          , expType
          , expConst = False
          , exp' = Obj
            { theObj = Object
              { O.loc
              , objType = expType
              , obj' = Variable
                { O.name = name
                , mode = Nothing}}}}
