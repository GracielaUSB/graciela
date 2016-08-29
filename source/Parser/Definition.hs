{-# LANGUAGE NamedFieldPuns #-}

module Parser.Definition
  (

  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression
import           Entry
import           Error
import           Location
import           Parser.Assertion   hiding (bound)
import qualified Parser.Assertion   as A (bound)
import           Parser.Expression
import           Parser.Instruction
import           Parser.Monad
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens       (use, (%=), (.=))
import           Control.Monad      (join, liftM5)
import           Data.Functor       (($>))
import qualified Data.Map           as Map (insert)
import           Data.Maybe         (isJust)
import           Data.Semigroup     ((<>))
import           Data.Sequence      (Seq)
import           Data.Text          (Text, unpack)
import           Text.Megaparsec    (lookAhead)
import           Text.Megaparsec    (between, getPosition, optional, (<|>))
--------------------------------------------------------------------------------

listDefProc :: Parser (Maybe (Seq Definition))
listDefProc = sequence <$> many (function <|> procedure)

function :: Parser (Maybe Definition)
function = do
  lookAhead $ match TokFunc

  from <- getPosition

  match TokFunc

  funcName' <- safeIdentifier
  funcParams'   <- between (match TokLeftPar) (match' TokRightPar) $
    sequence <$> funcParam `sepBy` match TokComma

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

  symbolTable %= openScope from

  funcBody' <- between (match' TokOpenBlock) (match' TokCloseBlock) expression

  currentFunc .= Nothing

  to <- getPosition
  symbolTable %= closeScope to

  let
    loc = Location (from, to)


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
    funcParam :: Parser (Maybe (Text, Type))
    funcParam = p `followedBy` oneOf [TokComma, TokRightPar]
      where
        p = do
          from <- getPosition

          parName' <- safeIdentifier
          match' TokColon
          t <- type'

          to <- getPosition
          let loc = Location (from, to)

          st <- use symbolTable

          case parName' of
            Nothing -> pure Nothing
            Just parName ->
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

  from <- getPosition
  match TokProc

  procName' <- safeIdentifier
  symbolTable %= openScope from
  params' <- between (match TokLeftPar) (match' TokRightPar) $
    sequence <$> procParam `sepBy` match TokComma

  decls' <- declarationOrRead

  pre'  <- precond
  post' <- postcond
  bnd   <- join <$> optional A.bound

  currentProc .= case (procName', params') of
    (Nothing,_) -> Nothing
    (_,Nothing) -> Nothing
    (Just procName, Just params) -> Just (procName, from, params, isJust bnd)

  symbolTable %= openScope from

  body' <- block <!>
    (from, UnknownError "Procedure lacks a body; block expected.")

  to <- getPosition
  symbolTable %= closeScope to
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
      definitions %= Map.insert procName def
      pure $ Just def
    _ -> pure Nothing

  where
    procParam :: Parser (Maybe (Text, Type, ArgMode))
    procParam = p `followedBy` oneOf [TokComma, TokRightPar]
      where
        p = do
          from <- getPosition
          mode' <- paramMode <!!>
            (from, UnknownError "A parameter mode must be specified.")

          parName' <- safeIdentifier
          match' TokColon
          t <- type'

          to <- getPosition
          let loc = Location (from, to)

          st <- use symbolTable

          case (parName', mode') of
            (Nothing,_) -> pure Nothing
            (_,Nothing) -> pure Nothing
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

    paramMode =  match TokIn    $> In
             <|> match TokInOut $> InOut
             <|> match TokOut   $> Out
             <|> match TokRef   $> Ref

-- procedureDeclaration :: Parser (Maybe Definition)
-- procedureDeclaration = do
--   from <- getPosition
--
--   match TokProc
--   id <- identifier
--   symbolTable %= openScope from
--   params <- parens $ procParam `sepBy` match TokComma
--
--   notFollowedBy $ match TokArrow
--   pre  <- precond
--   post <- postcond
--   st   <- use symbolTable
--
--   to   <- getPosition
--   let loc = Location (from,to)
--
--   symbolTable %= closeScope to
--   let
--     entry = Entry
--       { _entryName = id
--       , _loc       = loc
--       , _info      = Procedure
--        { _procParams = params
--        , _procTable  = st }}
--   symbolTable %= insertSymbol id entry
--
--   let def = Definition
--        { defLoc   = loc
--        , defName  = id
--
--        , st       = st
--        , defBound = Nothing
--        , def'     = AbstractProcedureDef
--          { pre    = pre
--          , post   = post
--          , params = params }}
--   return def
