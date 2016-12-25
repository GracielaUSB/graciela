{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Parser.Program
  ( program
  ) where

-------------------------------------------------------------------------------
import           AST.Program
import           AST.Type
import           Common
import           Location           (Location (..))
import           Parser.Definition
import           Parser.Instruction (block)
import           Parser.Monad       hiding (sepBy1)
import           Parser.State
import           Parser.Struct
import           SymbolTable        (closeScope, openScope)
import           Token
-------------------------------------------------------------------------------
import           Control.Lens       (use, (%=))
import           Data.Either
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)

import qualified Data.Sequence      as Seq (empty, fromList)
import qualified Data.Set           as Set (fromList, insert)
import qualified Data.Text          as T (intercalate)
import           Text.Megaparsec    (eof, getPosition, optional, sepBy1, (<|>), eitherP)
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Parser (Maybe Program)
program = do
  from <- getPosition

  match' TokProgram
  name' <- safeIdentifier
  ext <- optional $ do
    match TokDot
    exts <- identifier `sepBy1` match TokDot
    pure $ "." <> T.intercalate "." exts
  match' TokBegin

  symbolTable %= openScope from -- Open the program scope

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
      pend    <- use pendingDataType
      dts     <- use dataTypes
      fdts'   <- use fullDataTypes
      strings <- use stringIds
      

      -- Put pending data types as full data types
      forM_ (Map.toList fdts') $ \(name, typeargs) -> do
        case name `Map.lookup` pend of 
          Just pending -> forM_ pending $ \name' -> do
            forM_ typeargs $ \x -> 
              let 
                fAlter = Just . \case
                  Nothing -> Set.fromList [x]
                  Just y  -> Set.insert x y

              in fullDataTypes %= Map.alter fAlter name'
          Nothing -> pure ()

      -- Take the new full data types
      fdts' <- use fullDataTypes   
      -- internal $ show fdts'

      let 
        aux (name, typeArgs) = case name `Map.lookup` dts of
          Just struct -> (name, (struct, typeArgs))
          Nothing -> internal $ "Couldn't find struct " <> show name
        fdts = Map.fromList $ aux <$> (Map.toList fdts')

      pure $ Just Program
        { name        = name <> fromMaybe "" ext
        , loc         = Location (from, to)
        , defs
        , insts       = main
        , structs     = dts
        , fullStructs = fdts
        , strings }

    _ -> pure Nothing

  where
    mainRoutine = match' TokMain *> block
