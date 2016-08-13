{-#LANGUAGE NamedFieldPuns,InstanceSigs#-}

module Error where
--------------------------------------------------------------------------------
import           AST.Type              (Type, Type'(..))
import           AST.Expression        (Expression)
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
  |BadFuncExpressionType 
    { fName :: Text
    , fType :: Type
    , eType :: Type
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
    { aExpr :: Expression
    }
  | BadReadArgumentType
    { aExpr :: Expression
    , aType :: Type
    }
  | EmptyBlock
  
  | NoDoInvariant 

  | NoDoBound

  | NoAbstractInvariant
    { aName :: Text
    }
  | NoTypeRepInv
    { tName :: Text
    }
  | NoTypeCoupInv
    { tName :: Text
    }
  | NoProcBody 
    { pName :: Text
    }
  | NoProcPrecondition
    { pName :: Text
    }
  | NoProcPostcondition
    { pName :: Text
    }
  | NotInScope
    { sName :: Text
    }
  | UndefinedProcedure
    { pName :: Text
    }
  | UndefinedSymbol
    { sName :: Text
    }
  | UndefinedType 
    { tName :: Text
    }
  | UnknownError
    { emsg :: String 
    }
  deriving ( Eq)

instance ErrorComponent Error where
  representFail :: String -> Error
  representFail =  UnknownError 

  {- Unused, just to remove the class warning-}
  representIndentation _ _ _ = UnknownError ""


instance ShowErrorComponent Error where
  showErrorComponent err = case err of
    BadAssertType { aType } ->
      "Assertions must contain an expression of type " <> show GBool <>
      ". Actual type is " <> show aType <> "."

    BadBoundType  { bType } -> 
      "Bounds must contain an expression of type " <> show GInt <>
      ". Actual type is " <> show bType <> "."
    BadFuncExpressionType { fName, fType, eType } ->
      "The function `" <> unpack fName <> "` returns " <> show fType <> 
      " but has an expression of type " <> show eType
    BadProcNumberofArgs { pName, pPos, nParams, nArgs } ->
      "The procedure `" <> unpack pName <> "` " <> showPos' pPos <>
      " was defined with " <> show nParams <> 
      (if nParams == 1 then " parameter" else " parameters") <> ", but recived " <>
       show nArgs <> (if nArgs == 1 then " argument." else " arguments.")

    BadProcedureArgumentType { paramName, pName, pPos, pType, aType} ->
      "The parameter `" <> unpack paramName <>"` of the procedure `" <> unpack pName <>
      "` " <> showPos' pPos <> " has type " <> show pType <> 
      ", but recived a expression with type " <> show aType

    BadReadArgument { aExpr } -> 
      "The expression `" <> show aExpr <> "` is a constant expression."

    BadReadArgumentType { aExpr, aType } -> 
      "The variable `" <> show aExpr <> "` has type " <> show aType <> 
      " but only basic types can be read."

    EmptyBlock ->
      "Instruction blocks can not be empty and must contain at least an instruccion"

    NoDoInvariant ->
      "Missing invariant of instruction `do`."

    NoDoBound ->
      "Missing bound of instruction `do`."

    NoAbstractInvariant { aName } -> 
      "Missing invariant in abstract type `" <> unpack aName <> "`"

    NoTypeRepInv { tName } -> 
      "Missing representation invariant in type `" <> unpack tName <> "`"

    NoTypeCoupInv { tName } -> 
      "Missing couple invariant in type `" <> unpack tName <> "`"

    NoProcBody { pName } ->
      "Procedure `" <> unpack pName <> "` has not instruction block.\n" <>
      "Possible solution: Declare a instruction block using `|[` and `]|`"

    NoProcPrecondition { pName } -> 
      "Missing precondition of procedure `" <> unpack pName <> "`"

    NoProcPostcondition { pName } -> 
      "Missing postcondition of procedure `" <> unpack pName <> "`"

    NotInScope { sName } ->
      "Not in the scope: `" <> unpack sName <> "`"

    UndefinedProcedure { pName } -> 
      "Undefined procedure named `" <> unpack pName <> "`."

    UndefinedSymbol { sName } -> 
      "Undefined symbol named `" <> unpack sName <> "`."

    UndefinedType { tName } ->
      "Undefined type `" <> unpack tName <> "`"

    UnknownError {emsg} -> emsg 

instance Ord Error where
  a <= b = True



