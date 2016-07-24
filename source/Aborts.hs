{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Aborts
Description : Etiquetas para las llamadas al sistema
Copyright   : Graciela

En este modulo se encuentran las funciones que son utilizadas para crear las etiquetas 
con su respectivo codigo, para los casos en que puede ocurrir un error a tiempo de ejecucion
-}
module Aborts where

--------------------------------------------------------------------------------
import           LLVM.CodegenState
import           Location
--------------------------------------------------------------------------------
import qualified LLVM.General.AST.Constant   as C
import           LLVM.General.AST.Attribute
import           LLVM.General.AST                
--------------------------------------------------------------------------------

-- | String usado para terminar la ejecucion del programa
abortString :: String
abortString = "_abort"


-- | Atributos de los argumentos usados para hacer las llamadas del Abort.
abortArgs :: (Operand, [ParameterAttribute]) -> Location -> [(Operand, [ParameterAttribute])]
abortArgs x loc = [x, (ConstantOperand $ C.Int 32 (fromIntegral $ line   loc), []), 
                      (ConstantOperand $ C.Int 32 (fromIntegral $ column loc), []) ]


-- | Llama al sistema para terminar la ejecucion del programa
callAbort :: Integer -> Location -> LLVM (Operand)
callAbort num loc = do
    let x  = (ConstantOperand $ C.Int 32 num, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc


-- | Etiqueta usada para el caso en que el selector no se cumpla
createTagIf :: Name -> Location -> LLVM ()
createTagIf next loc = do

    callAbort 1 loc
    setLabel next $ nothing


-- | Etiqueta usada la instruccion abort
createTagAbort :: Location -> LLVM ()
createTagAbort loc = do

    callAbort 2 loc
    return ()


-- | Etiqueta usada para el caso en que la precondicion no se cumpla
createTagPre :: Name -> Location -> LLVM ()
createTagPre next loc = do

    callAbort 3 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que la postcondicion no se cumpla
createTagPost :: Name -> Location -> LLVM ()
createTagPost next loc = do

    callAbort 4 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que la asercion no se cumpla
createTagAsert :: Name -> Location -> LLVM ()
createTagAsert next loc = do

    callAbort 5 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que la invariante no se cumpla
createTagInv :: Name -> Location -> LLVM ()
createTagInv next loc = do

    callAbort 6 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que la funcion de cota no se cumpla
createTagBound :: Name -> Location -> Int -> LLVM ()
createTagBound next loc 1 = do

    callAbort 7 loc
    setLabel next $ nothing

createTagBound next loc 2 = do

    callAbort 8 loc
    setLabel next $ nothing


-- | Etiqueta usada para el caso en que se encuentre division por cero
createTagZero :: Name -> Location -> LLVM ()
createTagZero next loc = do 

    callAbort 9 loc
    setLabel next $ nothing


-- | Etiqueta usada para el caso en que el cuantificador para todo no se cunpla 
createTagForAll :: Name -> Location -> LLVM ()
createTagForAll next loc = do

    callAbort 10 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que el cuantificador existencial no se cunpla 
createTagExists :: Name -> Location -> LLVM ()
createTagExists next loc = do

    callAbort 11 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que ocurra un overflow en el programa
createTagOverflow :: Name -> Location -> LLVM ()
createTagOverflow next loc = do

    callAbort 12 loc
    setLabel next $ nothing


-- | Etiqueta usada para el caso en que el rango sea vacio
createTagRange :: Name -> Location -> LLVM ()
createTagRange next loc = do

    callAbort 13 loc
    setLabel next $ branch next 


-- | Etiqueta usada para el caso en que el rango no sea valido
createTagRangeAbort :: Name -> Location -> LLVM ()
createTagRangeAbort next loc = do

    callAbort 14 loc
    setLabel next $ nothing