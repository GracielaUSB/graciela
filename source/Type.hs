module Type where

data Type = MyInt | MyFloat | MyBool | MyChar | MyString | Function Type | Array Type
  deriving (Read, Eq) 

instance Show Type where
   show MyInt         = "Entero"
   show MyFloat       = "Flotante"
   show MyBool        = "Cadenas de Caracteres"
   show MyChar        = "Caracter"
   show (Function  t) = "Función que retorna " ++ show t
   show (Array     t) = "Arreglo de tipo " ++ show t 
