module TypeState where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import MyTypeError                        as PT
import SymbolTable
import Location
import Type
import AST


type MyVerType = RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type


addFunArgError :: T.Text -> Bool -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addFunArgError name isFunc t t' loc = addTypeError $ FunArgError name isFunc t t' loc 


addListError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () ()
addListError err = do RWSS.tell $ DS.singleton $ err


addNumberArgsError :: T.Text -> Bool -> Int -> Int -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addNumberArgsError name isFunc wtL prL loc = addTypeError $ NumberArgsError name isFunc wtL prL loc


addUndecFuncError :: T.Text -> Bool -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addUndecFuncError name isFunc loc = addTypeError $ UndecFunError name isFunc loc


addRetFuncError :: T.Text -> Type -> Type ->  Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addRetFuncError name tf body loc = addTypeError $ RetFuncError name tf body loc


addDifSizeDecError :: Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addDifSizeDecError loc = addTypeError $ DiffSizeError loc


addTypeError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError

addTypeDecError :: T.Text -> Location -> Type -> Type -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addTypeDecError id loc t t' = addTypeError $ TypeDecError id loc t t'


addNotOccursVarError :: T.Text -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addNotOccursVarError id loc = addTypeError $ NotOccursVar id loc


addInvalidPar :: AST Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addInvalidPar id loc = addTypeError $ InvalidPar id loc


addQuantBoolError :: OpQuant -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addQuantBoolError op tr tt loc = addTypeError $ QuantBoolError op tr tt loc


addQuantIntError :: OpQuant -> Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addQuantIntError op tr tt loc = addTypeError $ QuantIntError op tr tt loc
