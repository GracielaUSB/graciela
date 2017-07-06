{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Graciela.Parser.Program
  ( program
  ) where
-------------------------------------------------------------------------------
import           Language.Graciela.Location           (initialPos)
import           Language.Graciela.AST.Program        hiding (pragmas)
import qualified Language.Graciela.AST.Program        as P (pragmas)
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Location           (Location (..))
import           Language.Graciela.Parser.Definition
import           Language.Graciela.Parser.Instruction (block)
import           Language.Graciela.Parser.Monad       hiding (sepBy1)
import           Language.Graciela.Parser.Module      (gModule, includes)
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Struct
import           Language.Graciela.SymbolTable        (closeScope, openScope)
import           Language.Graciela.Token
-------------------------------------------------------------------------------
import           Control.Lens                         (use, (%=), (.=))
import           Control.Monad.State.Lazy             as State (MonadState(get))
import           Data.Either
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe)

import qualified Data.Sequence                        as Seq (empty, fromList)
import qualified Data.Set                             as Set (fromList, insert)
import qualified Data.Text                            as T (intercalate)
import           Prelude                              hiding (lex)
import           Text.Megaparsec                      (eitherP, eof,
                                                       getPosition, optional,
                                                       sepBy1, (<|>))
import           Text.Megaparsec                      as MP (getInput,setInput,pushPosition, popPosition)
import           System.Directory                     (doesFileExist)
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'


program :: Parser (Maybe Program)
program = do
  from <- getPosition
  symbolTable %= openScope from -- Open the program scope
  
  includes -- parse includes statements and move to the included file

  match' TokProgram
  name' <- safeIdentifier
  ext <- optional $ do
    match TokDot
    exts <- identifier `sepBy1` match TokDot
    pure $ "." <> T.intercalate "." exts
  match' TokBegin
  
  many $ eitherP (abstractDataType <|> dataType) (function <|> procedure)
  main' <- mainRoutine
  defs  <- Seq.fromList . toList <$> use definitions
  many $ eitherP (abstractDataType <|> dataType) (function <|> procedure)

    -- These aren't compiled since they can't be reached, but they're
    -- still checked so the user knows.

  match' TokEnd
  eof

  to <- getPosition
  symbolTable %= closeScope to

  case (name', main') of
    (Just name, Just main) -> do
      readFilesStack %= (:) (unpack name)
      pend    <- use pendingDataType
      structs     <- use dataTypes
      fdts'   <- use fullDataTypes
      strings <- use stringIds


      -- Put pending data types as full data types
      forM_ (Map.toList fdts') $ \(name, typeargs) -> do
        case name `Map.lookup` pend of
          Just pending -> forM_ pending $ \name' -> do
            forM_ (Map.toList typeargs) $ \(x,_) ->
              let
                t = Map.fromList [(x, False)]
                fAlter = Just . \case
                  Nothing     -> t
                  Just types0 -> types0 `Map.union` t

              in fullDataTypes %= Map.alter fAlter name'
          Nothing -> pure ()

      -- Take the new full data types
      fdts' <- use fullDataTypes
      -- internal $ show fdts'

      let
        aux (name, typeArgs) = case name `Map.lookup` structs of
          Just struct -> (name, (struct, typeArgs))
          Nothing     -> internal $ "Couldn't find struct " <> show name
        fullStructs = Map.fromList $ aux <$> (Map.toList fdts')

      p <- use pragmas

      readFilesStack %= tail
      pure $ Just Program
        { name        = name <> fromMaybe "" ext
        , loc         = Location (from, to)
        , defs
        , insts       = main
        , structs
        , fullStructs
        , P.pragmas   = p
        , strings }

    _ -> pure Nothing

  where
    mainRoutine = match' TokMain *> block
