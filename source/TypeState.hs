module TypeState where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import MyTypeError                        as PT
import SymbolTable
import Location
import Type



addFunArgError :: Type -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addFunArgError t t' loc = addTypeError $ FunArgError t t' loc


addListError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () ()
addListError err = do RWSS.tell $ DS.singleton $ err


addNumberArgsError :: T.Text -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addNumberArgsError name loc = addTypeError $ NumberArgsError name loc


addUndecFuncError ::  T.Text -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addUndecFuncError name loc = addTypeError $ UndecFunError name loc


addRetFuncError :: T.Text -> Type -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addRetFuncError name tf loc = addTypeError $ RetFuncError name tf loc


addTypeError :: MyTypeError -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type 
addTypeError error = do RWSS.tell $ DS.singleton error
                        return $ MyError


addNotOccursVarError :: T.Text -> Location -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () Type
addNotOccursVarError id loc = addTypeError $ NotOccursVar id loc



