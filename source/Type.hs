{-# LANGUAGE LambdaCase #-}
{-|
Module      : Type
Description : Tipos del lenguaje
Copyright   : GraCieLa

Modulo donde se encuentra todo lo referente a los tipos provisto en el lenguaje,
como tambien los utilizados de forma interna en el compilador.
-}
module Type where
--------------------------------------------------------------------------------
import           Data.Text (Text)
--------------------------------------------------------------------------------

-- | Es el tipos para los argumentos.
data TypeArg
    = In    -- ^ Argumento de entrada
    | Out   -- ^ Argumento de salida
    | InOut -- ^ Argumento de entrada/salida
    | Ref   -- ^ Argumento pasado por referencia
    deriving (Eq)


-- | Instancia 'Show' para los tipos de argumentos
instance Show TypeArg where
    show = \case
        In    -> " Var Int"
        Out   -> " Var Out"
        InOut -> " Var Int/Out"
        Ref   -> " Var Ref"


-- | Son los tipos utilizados en el compilador.
data Type
    = GUndef -- ^ Tipo indefinido ( graciela 2.0 )
    | GInt   -- ^ Tipo entero
    | GFloat -- ^ Tipo flotante
    | GBool  -- ^ Tipo boleano
    | GChar  -- ^ Tipo caracter

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
        { getSize :: Either Text Integer -- ^ Tamano del arreglo
        , getType :: Type                 -- ^ Tipo del arreglo
        }


-- | Instancia 'Eq' para los tipos.
instance Eq Type where
    GInt                 == GInt                 = True
    GFloat               == GFloat               = True
    GBool                == GBool                = True
    GChar                == GChar                = True
    GError               == GError               = True
    GEmpty               == GEmpty               = True
    (GDataType n1 _ _ _) == (GDataType n2 _ _ _) = True
    (GAbstractType n1 _ _ _) == (GAbstractType n2 _ _ _) = True
    (GDataType n1 _ _ _) == (GAbstractType n2 _ _ _) = True
    (GProcedure  _)      == (GProcedure   _)     = True
    (GFunction _ t)      == (GFunction _ t')     = t == t'
    (GArray    _ t)      == (GArray    _ t')     = t == t'
    _                    ==  _                   = False


-- | Instancia 'Show' para los tipos.
instance Show Type where
    show = \case
        GInt             -> "int"
        GFloat           -> "double"
        GBool            -> "boolean"
        GChar            -> "char"
        GEmpty           -> "void"
        GError           -> "error"
        (GProcedure   _) -> "proc"
        (GFunction  _ t) -> "func -> (" ++ show t ++ ")"
        (GArray     s t) -> "array " ++ show s ++ " of `" ++ show t ++ "`"


-- | Retorna la dimensión del arreglo.
getDimension :: Type -> Int
getDimension (GArray _ t) = 1 + getDimension t
getDimension _            = 0


-- | Verifica si el tipo es cuantificable (Tipo Enumerado).
isQuantifiable :: Type -> Bool
isQuantifiable GInt  = True
isQuantifiable GChar = True
isQuantifiable GBool = True
isQuantifiable _      = False
