{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TupleSections            #-}


module Language.Graciela.Parser.Definition
  ( function
  , procedure
  , functionDeclaration
  , procedureDeclaration
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.Parser.Assertion   hiding (bound)
import qualified Language.Graciela.Parser.Assertion   as A (bound)
import           Language.Graciela.Parser.Declaration
import           Language.Graciela.Parser.Expression
import           Language.Graciela.Parser.Instruction
import           Language.Graciela.Parser.Monad
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Type
import           Language.Graciela.SymbolTable        hiding (empty)
import qualified Language.Graciela.SymbolTable        as ST (empty)
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Applicative                  (empty)
import           Control.Lens                         (over, use, (%=), (%~),
                                                       (.=), (^.), _3, _Just)
import           Control.Monad                        (join, liftM5, when)
import qualified Data.Map.Strict                      as Map (insert, lookup)
import           Data.Maybe                           (isJust, isNothing)

import           Data.Sequence                        (Seq, ViewL (..), (|>))
import qualified Data.Sequence                        as Seq (empty, viewl)
import qualified Data.Set                             as Set (member)
import           Data.Text                            (Text, unpack)
import           Text.Megaparsec                      (between, eof,
                                                       errorUnexpected,
                                                       getPosition, lookAhead,
                                                       manyTill, optional, try,
                                                       withRecovery, (<|>))
--------------------------------------------------------------------------------

function :: Parser (Maybe Definition)
function = do
  lookAhead $ match TokFunc

  Location(_,from) <- match TokFunc
  symbolTable %= openScope from

  idFrom <- getPosition
  funcName' <- safeIdentifier
  idTo <- getPosition

  symbolTable %= openScope from

  funcParams' <- parens doFuncParams

  match' TokArrow
  logicAW <- Set.member LogicAnywhere <$> use pragmas
  funcRetType <- if logicAW then abstractType else type'

  dt <- use currentStruct
  goToDT <- case (dt, funcParams', funcName') of
    (Just (dtType, _, procs, _), Just params, Just funcName) -> do
      let
        aux = (\case; Just t -> t =:= dtType; _ -> False)
        hasTV  = any (hasTypeVar  . snd) params
        hasDT' = any (aux . hasDT . snd) params
      if hasTV || hasTypeVar funcRetType || hasDT'
        then if hasDT'
          then pure True
          else do
            putError from . UnknownError $
                  "One of the parameters of the function `" <> unpack funcName <>
                  "`\n\tmust have type " <> show dtType <> " when using Variable Types."
            pure False
      else do
        pure False

    _ -> pure False
  useLet .= True
  decls'  <- sequence <$> declaration `endBy` match' TokSemicolon
  useLet .= False
  prePos  <- getPosition
  pre'    <- precond <!> (prePos, UnknownError "Precondition was expected")
  postFrom <- getPosition

  symbolTable %= openScope postFrom
  case funcName' of
    Nothing -> pure ()
    Just funcName -> do
      symbolTable %= insertSymbol funcName Entry
        { _entryName = funcName
        , _loc       = Location (idFrom, idTo)
        , _info      = Var
          { _varType  = funcRetType
          , _varValue = Nothing
          , _varConst = False }}

  post'   <- postcond <!> (postFrom, UnknownError "Postcondition was expected")
  postTo <- getPosition

  symbolTable %= closeScope postTo

  bnd     <- join <$> optional A.bound

  let
    callTypeArgs = if goToDT
      then
        let Just (dtType,_,_,_) = dt
        in Just (typeName dtType, typeArgs dtType)
      else Nothing
  currentFunc .= case (funcName', funcParams') of
    (Just funcName, Just params) -> Just CurrentRoutine
      { _crName       = funcName
      , _crPos        = from
      , _crParams     = params
      , _crType       = funcRetType
      , _crTypeArgs   = callTypeArgs
      , _crRecAllowed = isJust bnd
      , _crRecursive  = False  }
    _ -> Nothing

  funcBody' <- between (match' TokOpenBlock) (match' TokCloseBlock) expression

  funcRecursive <- use currentFunc >>= pure . \case
    Nothing -> False
    Just cr -> cr ^.crRecursive

  currentFunc .= Nothing

  to <- getPosition
  symbolTable %= closeScope to
  symbolTable %= closeScope postTo
  let loc = Location (from, to)

  case (funcName', funcParams', pre', post', funcBody', decls') of
    (Just funcName, Just funcParams, Just pre, Just post, Just funcBody, Just funcDecls) ->
      if funcRetType =:= expType funcBody

        then do
          let
            def = Definition
              { defLoc   = loc
              , defName  = funcName
              , pre
              , post
              , bound = bnd
              , def' = FunctionDef
                { funcBody
                , funcParams
                , funcDecls
                , funcRetType
                , funcRecursive } }

          if goToDT
            then do
              let Just (_,_, procs,_) = dt
              case funcName `Map.lookup` procs of
                  Nothing -> do
                    currentStruct %= over _Just (_3 %~ (Map.insert funcName def))
                    pure $ Just def
                  Just _  -> do
                    putError from . UnknownError $
                      "Redefinition of function `" <> unpack funcName <> "`."
                    pure Nothing
          else do
            defs <- use definitions
            case funcName `Map.lookup` defs of
              Nothing -> do
                definitions %= Map.insert funcName def
                pure $ Just def
              Just _  -> do
                putError from . UnknownError $
                  "Redefinition of function `" <> unpack funcName <> "`."
                pure Nothing
      else do
        putError from BadFuncExpressionType
          { fName = funcName
          , fType = funcRetType
          , eType = expType funcBody }
        pure Nothing

    _ -> pure Nothing


doFuncParams =  lookAhead (match TokRightPar) $> Just Seq.empty
                <|> sequence <$> p `sepBy` match TokComma
  where
    p = funcParam `followedBy` oneOf [TokRightPar, TokComma]

    funcParam = do
      pos <- getPosition
      noParam pos <|> yesParam pos

    noParam pos = do
      lookAhead (oneOf [TokRightPar, TokComma])
      putError pos . UnknownError $ "A parameter was expected."
      pure Nothing

    yesParam from = do
      parName <- identifier
      match' TokColon
      logicAW <- Set.member LogicAnywhere <$> use pragmas
      t <- if logicAW then abstractType else type'

      to <- getPosition
      let loc = Location (from, to)

      st <- use symbolTable

      case parName `local` st of
        Right Entry { _loc } -> do
          putError from . UnknownError $
            "Redefinition of parameter `" <> unpack parName <>
            "`, original definition was at " <> show _loc <> "."
          pure Nothing
        Left _ -> do
          symbolTable %= insertSymbol parName
            (Entry parName loc (Argument In t))
          pure . Just $ (parName, t)



procedure :: Parser (Maybe Definition)
procedure = do
  lookAhead $ match TokProc

  Location (_, from) <- match TokProc

  procName' <- safeIdentifier
  symbolTable %= openScope from
  params' <- parens doProcParams

  dt <- use currentStruct
  goToDT <- case (dt, params', procName') of
    (Just (dtType, _, procs, _), Just params, Just procName) -> do
      let
        aux = (\case; Just t@GDataType{} -> t =:= dtType; _ -> False)
        hasTV  = any (\(_,pType,_) -> hasTypeVar  pType) params
        hasDT' = any (\(_,pType,_) -> aux $ hasDT pType) params
      if hasTV || hasDT'
        then if hasDT'
          then pure True
          else do
            putError from . UnknownError $
                  "One of the parameters of the procedure `" <> unpack procName <>
                  "`\n\tmust have type " <> show dtType <> " when using Variable Types."
            pure False
      else do
        existsDT .= False
        pure False

    _ -> pure False

  decls'  <- declarationOrRead
  prePos  <- getPosition
  pre'    <- precond <!> (prePos, UnknownError "Missing Precondition ")
  postPos <- getPosition
  post'   <- postcond <!> (postPos, UnknownError "Missing Postcondition")
  bnd     <- join <$> optional A.bound

  let
    callTypeArgs = if goToDT
      then
        let Just (dtType,_,_,_) = dt
        in Just (typeName dtType, typeArgs dtType)
      else Nothing
  currentProc .= case (procName', params') of
    (Just procName, Just params) -> Just CurrentRoutine
      { _crName       = procName
      , _crPos        = from
      , _crParams     = params
      , _crType       = ()
      , _crTypeArgs   = callTypeArgs
      , _crRecAllowed = isJust bnd
      , _crRecursive  = False  }
    _ -> Nothing

  symbolTable %= openScope from

  body' <- block <!>
    (from, UnknownError "Procedure lacks a body; block expected.")

  to <- getPosition
  symbolTable %= closeScope to -- body
  symbolTable %= closeScope to -- params

  procRecursive <- use currentProc >>= pure . \case
    Nothing -> False
    Just cr -> cr ^.crRecursive

  currentProc .= Nothing
  existsDT .= True
  let
    loc = Location (from, to)

  case (procName', params', decls', pre', post', body') of
    (Just procName, Just params, Just decls, Just pre, Just post, Just body) -> do
      let
        def = Definition
          { defLoc   = loc
          , defName  = procName
          , pre
          , post
          , bound = bnd
          , def' = ProcedureDef
            { procDecl = decls
            , procBody = body
            , procParams = params
            , procRecursive }}

      if goToDT
        then do
          let Just (_,_, procs,_) = dt
          case procName `Map.lookup` procs of
              Nothing -> do
                currentStruct %= over _Just (_3 %~ (Map.insert procName def))
                pure $ Just def
              Just _  -> do
                putError from . UnknownError $
                  "Redefinition of procedure `" <> unpack procName <> "`."
                pure Nothing
        else do
          defs <- use definitions
          case procName `Map.lookup` defs of
            Nothing -> do
              definitions %= Map.insert procName def
              pure $ Just def
            Just _  -> do
              putError from . UnknownError $
                "Redefinition of procedure `" <> unpack procName <> "`."
              pure Nothing



    _ -> pure Nothing

doProcParams =  lookAhead (match TokRightPar) $> Just Seq.empty
            <|> sequence <$> p `sepBy` match TokComma
  where
    p = procParam `followedBy` oneOf [TokRightPar, TokComma]

    procParam :: Parser (Maybe (Text, Type, ArgMode))
    procParam = do
      pos <- getPosition
      noParam pos <|> yesParam pos

    noParam pos = do
      lookAhead (oneOf [TokRightPar, TokComma])
      putError pos . UnknownError $ "A parameter was expected."
      pure Nothing

    yesParam from = do
      mode' <- paramMode <!!>
        (from, UnknownError "A parameter mode must be specified.")

      parName' <- safeIdentifier
      match' TokColon
      logicAW <- Set.member LogicAnywhere <$> use pragmas
      t <- if logicAW then abstractType else type'

      to <- getPosition
      let loc = Location (from, to)

      st <- use symbolTable

      case (parName', mode') of
        (Just parName, Just mode) ->
          case parName `local` st of
            Right Entry { _loc } -> do
              putError from . UnknownError $
                "Redefinition of parameter `" <> unpack parName <>
                "`, original definition was at " <> show _loc <> "."
              pure Nothing
            Left _
              |  not (t =:= basic) && mode == Const -> do
                putError from . UnknownError $
                  "Can not declare a parameter of type " <> show t <> "with mode Const"
                pure Nothing
              | otherwise -> do
                symbolTable %= insertSymbol parName
                  (Entry parName loc (Argument mode t))
                pure . Just $ (parName, t, mode)
        _ -> pure Nothing


    paramMode =  match TokIn    $> In
             <|> match TokInOut $> InOut
             <|> match TokOut   $> Out
             <|> match TokRef   $> Ref
             <|> match TokConst $> Const


functionDeclaration :: Parser (Maybe Definition)
functionDeclaration = do
  lookAhead $ match TokFunc
  Location(_,from) <- match TokFunc

  idFrom <- getPosition
  funcName' <- safeIdentifier
  idTo <- getPosition

  symbolTable %= openScope from
  params' <- parens $ doFuncParams

  match' TokArrow
  logicAW <- Set.member LogicAnywhere <$> use pragmas
  retType <- if logicAW then abstractType else type'

  dt <- use currentStruct
  case (dt, params', funcName') of
    (Just (dtType, _, procs, _), Just params, Just funcName) -> do
      let
        aux = (\case; Just t@GDataType{} -> t =:= dtType; _ -> False)
        hasTV  = any (\(_,pType) -> hasTypeVar  pType) params
        hasDT' = any (\(_,pType) -> aux $ hasDT pType) params
      when ((hasTV || hasTypeVar retType) && not hasDT') .
        putError from . UnknownError $
          "One of the parameters of the abstract function `" <> unpack funcName <>
          "`\n\tmust have type " <> show dtType <> " when using Variable Types."

    _ -> pure ()
  useLet .= True
  decls'  <- sequence <$> declaration `endBy` match' TokSemicolon
  useLet .= False
  pre'  <- (precond  <!>) . (,UnknownError "Missing Precondition ") =<< getPosition

  case funcName' of
    Nothing -> pure ()
    Just funcName -> do
      symbolTable %= insertSymbol funcName Entry
        { _entryName = funcName
        , _loc       = Location (idFrom, idTo)
        , _info      = Var
          { _varType  = retType
          , _varValue = Nothing
          , _varConst = False }}

  post' <- (postcond <!>) . (,UnknownError "Missing Postcondition") =<< getPosition

  to   <- getPosition
  let loc = Location (from,to)

  symbolTable %= closeScope to
  case (funcName', params', pre', post', decls') of
    (Just funcName, Just params, Just pre, Just post, Just decls) -> do
      pure . Just $ Definition
          { defLoc   = loc
          , defName  = funcName
          , pre
          , post
          , bound = Nothing
          , def' = AbstractFunctionDef
            { abstFParams = params
            , funcRetType = retType
            , abstFDecl   = decls
            }}

    _ -> pure Nothing

procedureDeclaration :: Parser (Maybe Definition)
procedureDeclaration = do
  lookAhead $ match TokProc
  Location(_,from) <- match TokProc

  procName' <- safeIdentifier
  symbolTable %= openScope from
  params' <- parens $ doProcParams

  useLet .= True
  decls' <- sequence <$> declaration `endBy` match' TokSemicolon
  useLet .= False

  dt <- use currentStruct
  case (dt, params', procName') of
    (Just (dtType, _, procs, _), Just params, Just procName) -> do
      let
        aux = (\case; Just t -> t =:= dtType; _ -> False)
        hasTV  = any (\(_,pType,_) -> hasTypeVar  pType) params
        hasDT' = any (\(_,pType,_) -> aux $ hasDT pType) params
      if hasTV && not hasDT'
        then putError from . UnknownError $
          "One of the parameters of the abstract procedure `" <> unpack procName <>
          "`\n\tmust have type " <> show dtType <> " when using Variable Types."
      else do
        existsDT .= False
    _ -> pure ()

  pre'  <- (precond  <!>) . (,UnknownError "Missing Precondition ") =<< getPosition
  post' <- (postcond <!>) . (,UnknownError "Missing Postcondition") =<< getPosition

  to   <- getPosition
  let loc = Location (from,to)
  existsDT .= True
  symbolTable %= closeScope to
  case (procName', params', pre', post', decls') of
    (Just procName, Just params, Just pre, Just post, Just decls) -> do
      pure . Just $ Definition
          { defLoc   = loc
          , defName  = procName
          , pre
          , post
          , bound = Nothing
          , def' = AbstractProcedureDef params decls }

    _ -> pure Nothing
