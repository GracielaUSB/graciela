{-|
Module      : Type
Description : Tipos del lenguaje
Copyright   : Graciela

Modulo donde se encuentra todo lo referente a los tipos provisto en el lenguaje,
como tambien los utilizados de forma interna en el compilador.
-}

{-# LANGUAGE LambdaCase #-}

module Type
  ( ArgMode (..)
  , Type (..)
  , (=:=)
  ) where
--------------------------------------------------------------------------------
import           Data.Int    (Int32)
import           Data.Monoid ((<>))
import           Data.Text   (Text, unpack)
--------------------------------------------------------------------------------

-- | Es el tipos para los argumentos.
data ArgMode
  = In    -- ^ Argumento de entrada
  | Out   -- ^ Argumento de salida
  | InOut -- ^ Argumento de entrada/salida
  | Ref   -- ^ Argumento pasado por referencia
  deriving (Eq)

-- | Instancia 'Show' para los tipos de argumentos
instance Show ArgMode where
  show = \case
    In    -> "In"
    Out   -> "Out"
    InOut -> "In/Out"
    Ref   -> "Ref"

-- | Son los tipos utilizados en el compilador.
data Type
  = GUndef              -- ^ Tipo indefinido ( graciela 2.0 )
  | GSet      Type      -- ^ Tipo conjunto ( graciela 2.0 )
  | GMultiset Type      -- ^ Tipo multiconjunto ( graciela 2.0 )
  | GSeq      Type      -- ^ Tipo secuencia ( graciela 2.0 )
  | GFunc     Type Type -- ^ Tipo func para TDAs ( graciela 2.0 )
  | GRel      Type Type -- ^ Tipo relación ( graciela 2.0 )
  | GTuple   [Type]     -- ^ Tipo n-upla ( graciela 2.0 )
  | GTypeVar  Text      -- ^ Variable de tipo ( graciela 2.0 )

  | GAny              -- Tipo arbitrario para polimorfismo ( graciela 2.0 )
  | GOneOf     [Type] -- Tipo arbitrario limitado para polimorfismo ( graciela 2.0 )
  | GUnsafeName Text  -- Tipo para

  | GInt   -- ^ Tipo entero
  | GFloat -- ^ Tipo flotante
  | GBool  -- ^ Tipo boleano
  | GChar  -- ^ Tipo caracter

  | GString

  -- Tipo para los Data type' as
  | GDataType
    { typeName ::  Text
    -- , oftype :: [Type]
    , types :: [Type]
    -- , procs  :: [Type]
    }
  | GPointer Type
  -- | Tipo para las funciones
  -- | GFunction
  --  { fParamType  :: [Type] -- ^ Los tipos de los parametros
  --  , fReturnType ::  Type   -- ^ El tipo de retorno
  --  }
  -- | GProcedure [Type] -- ^ Tipo para los procedimientos
  -- | GError            -- ^ Tipo usado para propagar los errores
  -- | GEmpty            -- ^ Tipo usado cuando la ver. de tipos es correcta

  -- | Tipo para los arreglos
  | GArray
    { size      :: Int32
    , arrayType :: Type       -- ^ Tipo del arreglo
    }
  deriving (Eq, Ord)


(=:=) :: Type -> Type -> Bool
a =:= b
  |  a == b
  || a == GAny
  || b == GAny = True
GOneOf        as =:= a                = True `elem` fmap (=:= a) as 
a                =:= GOneOf        as = True `elem` fmap (=:= a) as 
GDataType     {} =:= GDataType     {} = True
-- GProcedure    {} =:= GProcedure    {} = True
GArray       _ a =:= GArray       _ b = a =:= b
-- GFunction    _ a =:= GFunction    _ b = a =:= b
GPointer       a =:= GPointer       b = a =:= b
GSet           a =:= GSet           b = a =:= b
GMultiset      a =:= GMultiset      b = a =:= b
GSeq           a =:= GSeq           b = a =:= b
GFunc      da ra =:= GFunc      db rb = da =:= db && ra =:= rb
GRel       da ra =:= GRel       db rb = da =:= db && ra =:= rb
GTuple        as =:= GTuple        bs = and $ zipWith (=:=) as bs
GTypeVar       a =:= GTypeVar       b = a == b
_                =:= _                = False

-- | Instancia 'Show' para los tipos.
instance Show Type where
  show t' = "\ESC[0;32m" <> show' t' <> "\ESC[m"
    where
      show' = \case
        GUndef          -> "\ESC[0;31m" <> "undefined" <> "\ESC[0;32m"
        GInt            -> "int"
        GFloat          -> "double"
        GBool           -> "boolean"
        GChar           -> "char"
        -- GEmpty          -> "void"
        -- GError          -> "error"
        GPointer     t  -> "pointer to " <> show' t
        -- GProcedure   _  -> "proc"
        -- GFunction  _ t  -> "func -> (" <> show' t <> ")"
        GArray    s  t  -> "array[" <> show s <> "] of " <> show' t
        GSet      t     -> "set of " <> show' t
        GMultiset t     -> "multiset of " <> show' t
        GSeq      t     -> "sequence of " <> show' t
        GFunc     ta tb -> "function " <> show' ta <> " -> " <> show' tb
        GRel      ta tb -> "relation " <> show' ta <> " -> " <> show' tb
        GTuple    ts    ->
          "tuple (" <> (unwords . fmap show' $ ts) <> ")"
        GTypeVar  n     -> unpack n
        GDataType n f   -> "data type " <> unpack n

        GAny            -> "any type"
        GOneOf       as -> "one of " <> show as

        GUnsafeName t     -> unpack t
        GString  -> "string"


-- | Retorna la dimensión del arreglo.
-- getDimension :: Type -> Int
-- getDimension (GArray _ t) = 1 + getDimension t
-- getDimension _            = 0


-- | Verifica si el tipo es cuantificable (Tipo Enumerado).
-- isQuantifiable :: Type -> Bool
-- isQuantifiable GInt     = True
-- isQuantifiable GChar    = True
-- isQuantifiable GBool    = True
-- isQuantifiable GFloat   = False
-- isQuantifiable _        = False
