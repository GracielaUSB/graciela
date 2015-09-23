module TypeState where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import MyTypeError                        as PT
import SymbolTable
import Location
import Type
import AST


type MyVerType a = RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) ([String]) a


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


addTypeError :: MyTypeError -> MyVerType Type 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError

addTypeDecError :: T.Text -> Location -> Type -> Type -> MyVerType Type 
addTypeDecError id loc t t' = addTypeError $ TypeDecError id loc t t'


addNotOccursVarError :: T.Text -> Location -> MyVerType Type
addNotOccursVarError id loc = addTypeError $ NotOccursVar id loc


addInvalidPar :: AST Type -> Location -> MyVerType Type
addInvalidPar id loc = addTypeError $ InvalidPar id loc


addQuantBoolError :: OpQuant -> Type -> Type -> Location -> MyVerType Type 
addQuantBoolError op tr tt loc = addTypeError $ QuantBoolError op tr tt loc


addQuantIntError :: OpQuant -> Type -> Type -> Location -> MyVerType Type 
addQuantIntError op tr tt loc = addTypeError $ QuantIntError op tr tt loc

addAssignError :: T.Text -> Type -> Type -> Location -> MyVerType Type  
addAssignError name op1 op2 loc = addTypeError $ AssignError name op1 op2 loc
