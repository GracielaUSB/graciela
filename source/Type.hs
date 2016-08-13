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
  , Type' (..)
  , (=:=)
  ) where
--------------------------------------------------------------------------------
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
data Type' e
  = GUndef              -- ^ Tipo indefinido ( graciela 2.0 )
  | GSet      (Type' e)      -- ^ Tipo conjunto ( graciela 2.0 )
  | GMultiset (Type' e)      -- ^ Tipo multiconjunto ( graciela 2.0 )
  | GSeq      (Type' e)      -- ^ Tipo secuencia ( graciela 2.0 )
  | GFunc     (Type' e) (Type' e) -- ^ Tipo func para TDAs ( graciela 2.0 )
  | GRel      (Type' e) (Type' e) -- ^ Tipo relación ( graciela 2.0 )
  | GTuple    [Type' e]     -- ^ Tipo n-upla ( graciela 2.0 )
  | GTypeVar   Text      -- ^ Variable de tipo ( graciela 2.0 )

  | GAny                -- Tipo arbitrario para polimorfismo ( graciela 2.0 )
  | GOneOf    [Type' e]     -- Tipo arbitrario limitado para polimorfismo ( graciela 2.0 )
  | GUnsafeName Text    -- Tipo para

  | GInt           -- ^ Tipo entero
  | GFloat         -- ^ Tipo flotante
  | GBool          -- ^ Tipo boleano
  | GChar          -- ^ Tipo caracter

  -- Tipo para los Data type' as
  | GDataType
    { name   ::  Text
    -- , oftype :: [Type' e]
    -- , fields :: [Type' e]
    -- , procs  :: [Type' e]
    }
  | GAbstractType
    { name   ::  Text
    -- , oftype :: [Type' e]
    -- , fields :: [Type' e]
    -- , procs  :: [Type' e]
    }
  | GPointer (Type' e)
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
    { size      :: e
    , arrayType :: Type' e       -- ^ Tipo del arreglo
    }
  deriving (Eq, Ord)


(=:=) :: Eq e => Type' e -> Type' e -> Bool
a =:= b
  |  a == b
  || a == GAny
  || b == GAny = True
GOneOf        as =:= a                = a `elem` as
a                =:= GOneOf        as = a `elem` as
GDataType     {} =:= GDataType     {} = True
GAbstractType {} =:= GAbstractType {} = True
GDataType     {} =:= GAbstractType {} = True
GAbstractType {} =:= GDataType     {} = True
-- GProcedure    {} =:= GProcedure    {} = True
GArray       _ a =:= GArray       _ b = a =:= b
-- GFunction    _ a =:= GFunction    _ b = a =:= b
GSet           a =:= GSet           b = a =:= b
GMultiset      a =:= GMultiset      b = a =:= b
GSeq           a =:= GSeq           b = a =:= b
GFunc      da ra =:= GFunc      db rb = da =:= db && ra =:= rb
GRel       da ra =:= GRel       db rb = da =:= db && ra =:= rb
GTuple        as =:= GTuple        bs = and $ zipWith (=:=) as bs
GTypeVar       a =:= GTypeVar       b = a == b
_                =:= _                = False

-- | Instancia 'Show' para los tipos.
instance Show (Type' e) where
  show t = "\ESC[0;32m" <> show' t <> "\ESC[m"
    where
      show' = \case
        GUndef          -> "\ESC[0;31m" <> "undefined" <> "\ESC[0;32m"
        GInt            -> "int"
        GFloat          -> "double"
        GBool           -> "boolean"
        GChar           -> "char"
        -- GEmpty          -> "void"
        -- GError          -> "error"
        GPointer      t -> "pointer of " <> show' t
        -- GProcedure   _  -> "proc"
        -- GFunction  _ t  -> "func -> (" <> show' t <> ")"
        GArray     _ t  -> "array of " <> show' t
        GSet      t     -> "conjunto de " <> show' t
        GMultiset t     -> "multiconjunto de " <> show' t
        GSeq      t     -> "secuencia de " <> show' t
        GFunc     ta tb -> "función " <> show' ta <> "->" <> show' tb
        GRel      ta tb -> "relación " <> show' ta <> "->" <> show' tb
        GTuple    ts    ->
          "tupla (" <> (unwords . fmap show' $ ts) <> ")"
        GTypeVar  n     -> "variable de tipo " <> unpack n
        GDataType n     -> "type " <> unpack n
        GAbstractType n -> "abstract " <> unpack n

        GAny            -> "any type"
        GOneOf       as -> "one of " <> show as

        GUnsafeName t     -> unpack t


-- | Retorna la dimensión del arreglo.
getDimension :: Type' e -> Int
getDimension (GArray _ t) = 1 + getDimension t
getDimension _            = 0


-- | Verifica si el tipo es cuantificable (Tipo Enumerado).
-- isQuantifiable :: Type' e -> Bool
-- isQuantifiable GInt     = True
-- isQuantifiable GChar    = True
-- isQuantifiable GBool    = True
-- isQuantifiable GFloat   = False
-- isQuantifiable _        = False
