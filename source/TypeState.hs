{-|
Module      : TypeState
Description : Monad State para agregar los errores
Copyright   : GraCieLa

Modulo donde se encuentra las funciones utilizadas para agregar los errores
encontrados al estado.
-}
module TypeState where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import MyTypeError                        as PT
import SymbolTable
import Location
import Type
import AST


-- | Tipo del Monad el cual contiene, la tabla de simbolos, una secuencia de errores y una lista de strings,
-- | usados para el manejo de los errores a momento de ejecucion
type MyVerType a = RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) ([String]) a


addTypeError :: MyTypeError -> MyVerType Type 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError


addFunArgError :: T.Text -> Bool -> Type -> Type -> Location -> MyVerType Type
addFunArgError name isFunc t t' loc = addTypeError $ FunArgError name isFunc t t' loc 


addNumberArgsError :: T.Text -> Bool -> Int -> Int -> Location -> MyVerType Type 
addNumberArgsError name isFunc wtL prL loc = addTypeError $ NumberArgsError name isFunc wtL prL loc


addUndecFuncError :: T.Text -> Bool -> Location -> MyVerType Type 
addUndecFuncError name isFunc loc = addTypeError $ UndecFunError name isFunc loc


addRetFuncError :: T.Text -> Type -> Type ->  Location -> MyVerType Type 
addRetFuncError name tf body loc = addTypeError $ RetFuncError name tf body loc


addDifSizeDecError :: Location -> MyVerType Type
addDifSizeDecError loc = addTypeError $ DiffSizeError loc


addTypeDecError :: T.Text -> Location -> Type -> Type -> MyVerType Type 
addTypeDecError id loc t t' = addTypeError $ TypeDecError id loc t t'


addNotOccursVarError :: OpQuant -> T.Text -> Location -> MyVerType Type
addNotOccursVarError op sym loc = addTypeError $ NotOccursVar op sym loc


addInvalidPar :: T.Text -> AST Type -> Location -> MyVerType Type
addInvalidPar name id loc = addTypeError $ InvalidPar name id loc


addQuantRangeError  :: OpQuant -> Type -> Location -> MyVerType Type 
addQuantRangeError op range loc = addTypeError $ QuantRangeError op range loc


addQuantBoolError :: OpQuant -> Type -> Location -> MyVerType Type 
addQuantBoolError op tt loc = addTypeError $ QuantBoolError op tt loc


addQuantIntError :: OpQuant -> Type -> Location -> MyVerType Type 
addQuantIntError op tt loc = addTypeError $ QuantIntError op tt loc


addAssignError :: T.Text -> Type -> Type -> Location -> MyVerType Type  
addAssignError name op1 op2 loc = addTypeError $ AssignError name op1 op2 loc
