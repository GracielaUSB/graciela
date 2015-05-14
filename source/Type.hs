module Type where

import AST

data TypeArg = In | Out | InOut
      deriving (Eq)


instance Show TypeArg where
   show In    = " es una Variable de Entrada"
   show Out   = " es una Variable de Salida"
   show InOut = " es una Variable de Entrada y Salida"


data Type = MyInt | MyFloat | MyBool | MyChar | MyString |  Function [Type] Type | Array Type [AST ()] | Procedure [Type]
  deriving (Eq) 



instance Show Type where
   show MyInt            = "Entero"
   show MyFloat          = "Flotante"
   show MyBool           = "Booleano"
   show MyString         = "Cadenas de Caracteres"
   show MyChar           = "Caracter"
   show (Function xs t ) = "Función que retorna " ++ show t
   show (Array    t  xs) = "Arreglo de tipo "     ++ show t 
   show (Procedure   xs) = "Procedimiento"



