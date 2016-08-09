{-#LANGUAGE NamedFieldPuns,InstanceSigs#-}

module MyParseError where
--------------------------------------------------------------------------------
import           Location
import           Token
import           Type                  (Type)
import           Data.Text             (unpack, Text)
import           Data.Monoid ((<>))
import           Text.Megaparsec       hiding (Token)
import           Text.Megaparsec.Error  
--------------------------------------------------------------------------------

data MyParseError
  = CustomError -- Mientras no mejoremos los errores jajaja
    { msg :: String
    , loc :: Location
    }

data Error 
  = InvalidProcedureArgumentType
    { name  :: Text
    , pName :: Text
    , pPos  :: SourcePos
    , pType :: Type 
    , aType :: Type
    }
  | InvalidReadArgument
    { name :: Text
    }
  | InvalidReadArgumentType
    { name  :: Text
    , aType :: Type
    }
  | UndefinedProcedure
    { name :: Text
    }
  | UndefinedSymbol
    { name :: Text
    }
  | UnknowError
    { emsg :: String 
    }
  deriving (Ord, Eq)

instance ErrorComponent Error where
  representFail :: String -> Error
  representFail =  UnknowError 

  {- Unused, just to remove the class warning-}
  representIndentation _ _ _ = UnknowError ""


instance ShowErrorComponent Error where
  showErrorComponent err = case err of
    InvalidProcedureArgumentType { name, pName, pPos, pType, aType} ->
      "The parameter `" <> unpack name <>"` of the procedure `" <> unpack pName <>
      "` defined at " <> showPos' pPos <> " has type " <> show pType <> 
      ", but a expression with type " <> show aType <> " was given."

    InvalidReadArgument { name } -> 
      "The variable `" <> unpack name <> "` cannot be a constant."

    InvalidReadArgumentType { name, aType } -> 
      "The variable `" <> unpack name <> "` has type " <> show aType <> 
      " but only basic type can be read."

    UndefinedProcedure {name} -> 
      "Undefined procedure named `" <> unpack name <> "`."

    UndefinedSymbol {name} -> 
      "Undefined symbol named `" <> unpack name <> "`."

    UnknowError {emsg} -> emsg 



instance Show MyParseError where
  show (CustomError msg loc) = msg

