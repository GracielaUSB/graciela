{-|
Module      : Type
Description : Tipos del lenguaje
Copyright   : GraCieLa

Modulo donde se encuentra todo lo referente a los tipos provisto en el lenguaje,
como tambien los utilizados de forma interna en el compilador.
-}
module Type where

import Data.Text


-- | Es el tipos para los argumentos.
data TypeArg =   In    -- ^ Argumento de entrada
               | Out   -- ^ Argumento de salida
               | InOut -- ^ Argumento de entrada/salida
               | Ref   -- ^ Argumento pasado por referencia
      deriving (Eq)


-- | Instancia 'Show' para los tipos de argumentos
instance Show TypeArg where
   show In    = " Var Int"
   show Out   = " Var Out"
   show InOut = " Var Int/Out"
   show Ref   = " Var Ref"  


-- | Son los tipos utilizados en el compilador.
data Type =   MyInt      -- ^ Tipo entero
            | MyFloat    -- ^ Tipo flotante
            | MyBool     -- ^ Tipo boleano
            | MyChar     -- ^ Tipo caracter
              
            -- | Tipo para las funciones
            | MyFunction {  paramType :: [Type] -- ^ Los tipos de los parametros
                         , retuType :: Type     -- ^ El tipo de retorno
                         } 
            | MyProcedure [Type] -- ^ Tipo para los procedimientos 
            | MyError    -- ^ Tipo usado para propagar los errores
            | MyEmpty    -- ^ Tipo usado cuando la ver. de tipos es correcta
            
            -- | Tipo para los arreglos
            | MyArray { getTam :: Either Text Integer -- ^ Tamano del arreglo
                      , getType :: Type               -- ^ Tipo del arreglo
                      }


-- | Instancia 'Eq' para los tipos.
instance Eq Type where
   MyInt            ==  MyInt             = True
   MyFloat          ==  MyFloat           = True           
   MyBool           ==  MyBool            = True
   MyChar           ==  MyChar            = True
   MyError          ==  MyError           = True
   MyEmpty          ==  MyEmpty           = True
   (MyProcedure  _) == (MyProcedure  _)   = True
   (MyFunction _ t) == (MyFunction _ t')  = t == t'
   (MyArray    d t) == (MyArray d' t')    = t == t'
   _                ==  _                 = False


-- | Instancia 'Show' para los tipos.
instance Show Type where
   show  MyInt             = "int"
   show  MyFloat           = "double"
   show  MyBool            = "boolean"
   show  MyChar            = "char"
   show  MyEmpty           = "vacio"
   show  MyError           = "error"
   show (MyFunction xs t ) = "func, tipo de Retorno: " ++ show t
   show (MyArray    t  xs) = "array of " ++ show t ++ " de tamaño " ++ show xs
   show (MyProcedure   xs) = "proc"


-- | Verifica si el tipo es un arreglo.
isArray :: Type -> Bool
isArray (MyArray _ _) = True
isArray _             = False


-- | Verifica si el tipo es un procedimiento.
isTypeProc :: Type -> Bool
isTypeProc (MyProcedure _) = True
isTypeProc _               = False
 

-- | Verifica si el tipo es una función.
isTypeFunc :: Type -> Bool
isTypeFunc (MyFunction _ _ ) = True
isTypeFunc _                 = False


-- | Retorna la dimencion del arreglo.
getDimention :: Type -> Int
getDimention (MyArray tam t)= 1 + getDimention t
getDimention _              = 0


-- | Verifica si el tipo es cuantificable (Tipo Enumerado).
isCuantificable :: Type -> Bool
isCuantificable x = x == MyInt || x == MyChar || x == MyBool

