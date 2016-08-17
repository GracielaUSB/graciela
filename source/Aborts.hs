{-|
Module      : Aborts
Description : Etiquetas para las llamadas al sistema
Copyright   : Graciela

En este modulo se encuentran las funciones que son utilizadas para crear las etiquetas
con su respectivo codigo, para los casos en que puede ocurrir un error a tiempo de ejecucion
-}
module Aborts where

--------------------------------------------------------------------------------
import           LLVM.State
import           LLVM.Type
import           Location
--------------------------------------------------------------------------------
import           Data.Sequence
import           LLVM.General.AST
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import           Text.Megaparsec.Pos                     (SourcePos (..),
                                                          unPos, unsafePos)
--------------------------------------------------------------------------------

-- | String usado para terminar la ejecucion del programa
abortString :: String
abortString = "_abort"


-- | Atributos de los argumentos usados para hacer las llamadas del Abort.
abortArgs :: (Operand, [ParameterAttribute])
          -> SourcePos
          -> [(Operand, [ParameterAttribute])]
abortArgs x pos =
  [ x
  , (ConstantOperand $ C.Int 32 (fromIntegral $ unPos $ sourceLine   pos), [])
  , (ConstantOperand $ C.Int 32 (fromIntegral $ unPos $ sourceColumn pos), [])
  ]


-- | Llama al sistema para terminar la ejecucion del programa
callAbort :: Integer -> SourcePos -> LLVM Operand
callAbort num pos = do
    let x  = (ConstantOperand $ C.Int 32 num, [])
    let df = Right . ConstantOperand . C.GlobalReference intType $ Name abortString
    label <- newLabel

    let 
    	args = abortArgs x pos
    	call = Call Nothing CC.C [] df args [] []
    addInstructions $ singleton (label := call)
    return $ LocalReference voidType $ label 

-- | Etiqueta usada para el caso en que el selector no se cumpla
createTagIf :: Name -> SourcePos -> LLVM ()
createTagIf next pos = do
    callAbort 1 pos
    setLabel next $ Do $ Unreachable []


-- | Etiqueta usada la instruccion abort
createTagAbort :: SourcePos -> LLVM ()
createTagAbort pos = do
    callAbort 2 pos
    return ()


-- | Etiqueta usada para el caso en que la precondicion no se cumpla
createTagPre :: Name -> SourcePos -> LLVM ()
createTagPre next pos = do
    callAbort 3 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que la postcondicion no se cumpla
createTagPost :: Name -> SourcePos -> LLVM ()
createTagPost next pos = do
    callAbort 4 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que la asercion no se cumpla
createTagAsert :: Name -> SourcePos -> LLVM ()
createTagAsert next pos = do
    callAbort 5 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que la invariante no se cumpla
createTagInv :: Name -> SourcePos -> LLVM ()
createTagInv next pos = do
    callAbort 6 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que la funcion de cota no se cumpla
createTagBound :: Name -> SourcePos -> Int -> LLVM ()
createTagBound next pos 1 = do
    callAbort 7 pos
    setLabel next $ Do $ Unreachable []

createTagBound next pos 2 = do
    callAbort 8 pos
    setLabel next $ Do $ Unreachable []


-- | Etiqueta usada para el caso en que se encuentre division por cero
createTagZero :: Name -> SourcePos -> LLVM ()
createTagZero next pos = do
    callAbort 9 pos
    setLabel next $ Do $ Unreachable []


-- | Etiqueta usada para el caso en que el cuantificador para todo no se cunpla
createTagForAll :: Name -> SourcePos -> LLVM ()
createTagForAll next pos = do
    callAbort 10 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que el cuantificador existencial no se cunpla
createTagExists :: Name -> SourcePos -> LLVM ()
createTagExists next pos = do
    callAbort 11 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que ocurra un overflow en el programa
createTagOverflow :: Name -> SourcePos -> LLVM ()
createTagOverflow next pos = do
    callAbort 12 pos
    setLabel next $ Do $ Unreachable []


-- | Etiqueta usada para el caso en que el rango sea vacio
createTagRange :: Name -> SourcePos -> LLVM ()
createTagRange next pos = do
    callAbort 13 pos
    setLabel next $ Do $ Br next []


-- | Etiqueta usada para el caso en que el rango no sea valido
createTagRangeAbort :: Name -> SourcePos -> LLVM ()
createTagRangeAbort next pos = do
    callAbort 14 pos
    setLabel next $ Do $ Unreachable []
