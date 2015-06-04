module TypeState where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import SymbolTable
import MyTypeError                        as PT
import qualified Data.Text                as T
import Location
import Type

addFunArgError :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
addFunArgError t t' loc = addTypeError $ FunArgError t t' loc

addListError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () ()
addListError err = do RWSS.tell $ DS.singleton $ err

addNumberArgsError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addNumberArgsError loc name = addTypeError $ NumberArgsError loc name


addUndecFuncError :: Location -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addUndecFuncError loc name = addTypeError $ UndecFunError loc name


addRetFuncError :: Location -> Type -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addRetFuncError loc tf name = addTypeError $ RetFuncError name tf loc


addTypeError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type) 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError

addNotOccursVarError :: T.Text -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (Type)
addNotOccursVarError id loc = addTypeError $ NotOccursVar id loc

