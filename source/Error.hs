{-#LANGUAGE NamedFieldPuns,InstanceSigs#-}

module Error where
--------------------------------------------------------------------------------
import           AST.Type              (Type, Type'(..))
import           Location
import           Token
import           Data.Text             (unpack, Text)
import           Data.Monoid ((<>))
import           Text.Megaparsec       hiding (Token)
import           Text.Megaparsec.Error  
--------------------------------------------------------------------------------

data MyParseError
  = CustomError -- Mientras no mejoremos los errores jajaja
    { msg :: String
    , mpeloc :: Location
    }

instance Show MyParseError where
  show (CustomError msg loc) = show loc <> "  " <> msg


data Error 
  = BadAssertType
    { aType :: Type
    }
  | BadBoundType
    { bType :: Type
    }
  | BadProcNumberofArgs
    { pName   :: Text
    , pPos    :: SourcePos
    , nParams :: Int
    , nArgs   :: Int
    }
  | BadProcedureArgumentType
    { paramName :: Text
    , pName     :: Text
    , pPos      :: SourcePos
    , pType     :: Type 
    , aType     :: Type
    }
  | BadReadArgument
    { aName :: Text
    }
  | BadReadArgumentType
    { aName  :: Text
    , aType :: Type
    }
  | NoAbstractInvariant
    { aName :: Text
    }
  | NoTypeRepInv
    { tName :: Text
    }
  | NoTypeCoupInv
    { tName :: Text
    }
  | NoProcPrecondition
    { pName :: Text
    }
  | NoProcPostcondition
    { pName :: Text
    }
  | UndefinedProcedure
    { pName :: Text
    }
  | UndefinedSymbol
    { sName :: Text
    }
  | UnknowError
    { emsg :: String 
    }
  deriving ( Eq)

instance ErrorComponent Error where
  representFail :: String -> Error
  representFail =  UnknowError 

  {- Unused, just to remove the class warning-}
  representIndentation _ _ _ = UnknowError ""


instance ShowErrorComponent Error where
  showErrorComponent err = case err of
    BadAssertType { aType } ->
      "Assertions must contain an expression of type " <> show GBool <>
      ". Actual type is " <> show aType <> "."

    BadBoundType  { bType } -> 
      "Bounds must contain an expression of type " <> show GInt <>
      ". Actual type is " <> show bType <> "."

    BadProcNumberofArgs { pName, pPos, nParams, nArgs } ->
      "The procedure `" <> unpack pName <> "` " <> showPos' pPos <>
      " was defined with " <> show nParams <> 
      (if nParams == 1 then " parameter" else " parameters") <> ", but recived " <>
       show nArgs <> (if nArgs == 1 then " argument." else " arguments.")

    BadProcedureArgumentType { paramName, pName, pPos, pType, aType} ->
      "The parameter `" <> unpack paramName <>"` of the procedure `" <> unpack pName <>
      "` " <> showPos' pPos <> " has type " <> show pType <> 
      ", but recived a expression with type " <> show aType

    BadReadArgument { aName } -> 
      "The variable `" <> unpack aName <> "` cannot be a constant."

    BadReadArgumentType { aName, aType } -> 
      "The variable `" <> unpack aName <> "` has type " <> show aType <> 
      " but only basic type can be read."

    NoAbstractInvariant { aName } -> 
      "Missing invariant in abstract type `" <> unpack aName <> "`"

    NoTypeRepInv { tName } -> 
      "Missing representation invariant in type `" <> unpack tName <> "`"

    NoTypeCoupInv { tName } -> 
      "Missing couple invariant in type `" <> unpack tName <> "`"

    NoProcPrecondition { pName } -> 
      "Missing precondition of procedure `" <> unpack pName <> "`"

    NoProcPostcondition { pName } -> 
      "Missing postcondition of procedure `" <> unpack pName <> "`"

    UndefinedProcedure { pName } -> 
      "Undefined procedure named `" <> unpack pName <> "`."

    UndefinedSymbol { sName } -> 
      "Undefined symbol named `" <> unpack sName <> "`."

    UnknowError {emsg} -> emsg 

instance Ord Error where
  a <= b = True



