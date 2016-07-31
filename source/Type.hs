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
  , isQuantifiable
  ) where
--------------------------------------------------------------------------------
import           Data.Text (Text, unpack)
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

  | GInt           -- ^ Tipo entero
  | GFloat         -- ^ Tipo flotante
  | GBool          -- ^ Tipo boleano
  | GChar          -- ^ Tipo caracter

  -- Tipo para los Data types
  | GDataType
    { name   ::  Text
    , oftype :: [Type]
    , fields :: [Type]
    , procs  :: [Type]
    }
  | GAbstractType
    { name   ::  Text
    , oftype :: [Type]
    , fields :: [Type]
    , procs  :: [Type]
    }
  | GPointer Type
  -- | Tipo para las funciones
  | GFunction
   { paramType  :: [Type] -- ^ Los tipos de los parametros
   , returnType ::  Type   -- ^ El tipo de retorno
   }
  | GProcedure [Type] -- ^ Tipo para los procedimientos
  | GError            -- ^ Tipo usado para propagar los errores
  | GEmpty            -- ^ Tipo usado cuando la ver. de tipos es correcta

  -- | Tipo para los arreglos
  | GArray
    { getSize   :: Either Text Integer -- ^ Tamano del arreglo
    , arrayType :: Type                 -- ^ Tipo del arreglo
    }


-- | Instancia 'Eq' para los tipos.
instance Eq Type where
  GInt                      == GInt                      = True
  GFloat                    == GFloat                    = True
  GBool                     == GBool                     = True
  GChar                     == GChar                     = True
  GError                    == GError                    = True
  GEmpty                    == GEmpty                    = True
  (GDataType _n1 _ _ _)     == (GDataType _n2 _ _ _)     = True
  (GAbstractType _n1 _ _ _) == (GAbstractType _n2 _ _ _) = True
  (GDataType _n1 _ _ _)     == (GAbstractType _n2 _ _ _) = True
  (GProcedure  _)           == (GProcedure   _)          = True
  (GFunction _ t)           == (GFunction _ t')          = t == t'
  (GArray    _ t)           == (GArray    _ t')          = t == t'

  (GSet t)                  == (GSet t')                 = t == t'
  (GMultiset t)             == (GMultiset t')            = t == t'
  (GSeq t)                  == (GSeq t')                 = t == t'

  (GFunc d r)               == (GFunc d' r')             =
      (d == d') && (r == r')
  (GRel d r)                == (GRel d' r')              =
      (d == d') && (r == r')

  (GTuple    ts)            == (GTuple    ts')           = ts == ts'
  (GTypeVar  t)             == (GTypeVar  t')            = t == t'

  _                         == _                         = False


-- | Instancia 'Show' para los tipos.
instance Show Type where
  show = \case
    GInt              -> "int"
    GFloat            -> "double"
    GBool             -> "boolean"
    GChar             -> "char"
    GEmpty            -> "void"
    GError            -> "error"
    GPointer      t   -> "pointer of "++show t
    (GProcedure   _)  -> "proc"
    (GFunction  _ t)  -> "func -> (" ++ show t ++ ")"
    (GArray     s t)  -> "array " ++ show s ++ " of `" ++ show t ++ "`"
    GSet      t       -> "conjunto de `" ++ show t ++ "`"
    GMultiset t       -> "multiconjunto de `" ++ show t ++ "`"
    GSeq      t       -> "secuencia de `" ++ show t ++ "`"
    GFunc     ta tb   -> "función `" ++ show ta ++ "->" ++ show tb ++ "`"
    GRel      ta tb   -> "relación `" ++ show ta ++ "->" ++ show tb ++ "`"
    GTuple    ts      ->
      "tupla (" ++ (unwords . map show $ ts) ++ ")"
    GTypeVar  n       -> "variable de tipo `" ++ show n ++ "`"
    GDataType n _ _ _ -> "type `" ++ unpack n ++ "`"
    GAbstractType n _ _ _ -> "abstract `" ++ unpack n ++ "`"
    GUndef -> undefined


-- | Retorna la dimensión del arreglo.
getDimension :: Type -> Int
getDimension (GArray _ t) = 1 + getDimension t
getDimension _            = 0


-- | Verifica si el tipo es cuantificable (Tipo Enumerado).
isQuantifiable :: Type -> Bool
isQuantifiable GInt     = True
isQuantifiable GChar    = True
isQuantifiable GBool    = True
isQuantifiable GFloat   = False
isQuantifiable _        = False
