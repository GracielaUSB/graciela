{-# LANGUAGE NamedFieldPuns #-}

module Parser.Definition
  ( function
  , procedure
  , procedureDeclaration
  , listDefProc
  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression
import           Entry
import           Error
import           Location
import           Parser.Assertion    hiding (bound)
import qualified Parser.Assertion    as A (bound)
import           Parser.Expression
import           Parser.Instruction
import           Parser.Monad
import           Parser.State
import           Parser.Type
import           SymbolTable         hiding (empty)
import qualified SymbolTable         as ST (empty)
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Applicative (empty)
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (join, liftM5, when)
import           Data.Functor        (void, ($>))
import qualified Data.Map.Strict           as Map (insert)
import           Data.Maybe          (isJust, isNothing)
import           Data.Semigroup      ((<>))
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq (empty)
import           Data.Text           (Text, unpack)
import           Text.Megaparsec     (between, eof, errorUnexpected,
                                      getPosition, lookAhead, manyTill,
                                      optional, withRecovery, (<|>))
--------------------------------------------------------------------------------
import           Debug.Trace

listDefProc :: Parser (Maybe (Seq Definition))
listDefProc = sequence <$> many (function <|> procedure)

function :: Parser (Maybe Definition)
function = do
  lookAhead $ match TokFunc

  Location(_,from) <- match TokFunc
  symbolTable %= openScope from

  funcName' <- safeIdentifier

  symbolTable %= openScope from

  funcParams' <- parens doFuncParams

  match' TokArrow
  funcRetType <- type'

  prePos  <- getPosition
  pre'    <- precond <!> (prePos, UnknownError "Precondition was expected")
  postPos <- getPosition
  post'   <- postcond <!> (postPos, UnknownError "Postcondition was expected")
  bnd     <- join <$> optional A.bound

  currentFunc .= case (funcName', funcParams') of
    (Nothing,_) -> Nothing
    (_,Nothing) -> Nothing
    (Just funcName, Just params) ->
      Just (funcName, from, funcRetType, params, isJust bnd)


  funcBody' <- between (match' TokOpenBlock) (match' TokCloseBlock) expression

  currentFunc .= Nothing

  to <- getPosition
  symbolTable %= closeScope to

  let loc = Location (from, to)

  case (funcName', funcParams', pre', post', funcBody') of
    (Just funcName, Just funcParams, Just pre, Just post, Just funcBody) ->
      if funcRetType == expType funcBody
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
                , funcRetType } }
          definitions %= Map.insert funcName def
          pure . Just $ def

        else do
          putError from BadFuncExpressionType
            { fName = funcName
            , fType = funcRetType
            , eType = expType funcBody }
          pure Nothing
    _ -> pure Nothing


  where
    doFuncParams =  lookAhead (match TokRightPar) $> Just Seq.empty
                <|> sequence <$> p `sepBy` match TokComma

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
      t <- type'

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

  Location(_,from) <- match TokProc

  procName' <- safeIdentifier
  symbolTable %= openScope from
  params' <- parens doProcParams

  decls'  <- declarationOrRead
  prePos  <- getPosition
  pre'    <- precond <!> (prePos, UnknownError "Missing Precondition ")
  postPos <- getPosition
  post'   <- postcond <!> (postPos, UnknownError "Missing Postcondition")
  bnd     <- join <$> optional A.bound

  currentProc .= case (procName', params') of
    (Just procName, Just params) -> Just (procName, from, params, isJust bnd)
    _ -> Nothing

  symbolTable %= openScope from

  body' <- block <!>
    (from, UnknownError "Procedure lacks a body; block expected.")

  to <- getPosition
  symbolTable %= closeScope to -- body
  symbolTable %= closeScope to -- params
  currentProc .= Nothing

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
            , procParams = params }}

      -- Struct does not add thier procs to the table
      dt <- use currentStruct
      when (isNothing Nothing) $ definitions %= Map.insert procName def

      pure $ Just def
    _ -> pure Nothing

doProcParams =  lookAhead (match TokRightPar) $> Just Seq.empty
            <|> sequence <$> p `sepBy` match TokComma

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
  t <- type'

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
        Left _ -> do
          symbolTable %= insertSymbol parName
            (Entry parName loc (Argument mode t))
          pure . Just $ (parName, t, mode)
    _ -> pure Nothing


paramMode =  match TokIn    $> In
         <|> match TokInOut $> InOut
         <|> match TokOut   $> Out
         <|> match TokRef   $> Ref

procedureDeclaration :: Parser (Maybe Definition)
procedureDeclaration = do
  lookAhead $ match TokProc
  Location(_,from) <- match TokProc

  procName' <- safeIdentifier
  symbolTable %= openScope from
  params' <- parens doProcParams

  prePos <- getPosition
  pre'    <- precond <!> (prePos, UnknownError "Missing Precondition ")
  postPos <- getPosition
  post'   <- postcond <!> (postPos, UnknownError "Missing Postcondition")

  to   <- getPosition
  let loc = Location (from,to)

  symbolTable %= closeScope to
  case (procName', params', pre', post') of
    (Just procName, Just params, Just pre, Just post) -> do
      let
        def = Definition
          { defLoc   = loc
          , defName  = procName
          , pre
          , post
          , bound = Nothing
          , def' = AbstractProcedureDef params }

      pure $ Just def
    _ -> pure Nothing
