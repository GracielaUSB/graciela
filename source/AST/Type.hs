{-|
Module      : Type
Description : Tipos del lenguaje
Copyright   : Graciela


-}

{-# LANGUAGE LambdaCase #-}

module AST.Type
  ( ArgMode (..)
  , Type (..)
  , Type' (..)
  , (=:=)
  ) where
--------------------------------------------------------------------------------
import           AST.Expression
import           Type        (Type'(..), ArgMode(..), (=:=))
--------------------------------------------------------------------------------


type Type = Type' Expression

getDimension :: Type' a -> Int
getDimension (GArray _ t) = 1 + getDimension t
getDimension _            = 0

