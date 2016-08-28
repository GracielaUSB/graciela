{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Error where
--------------------------------------------------------------------------------
import           AST.Expression        (Expression)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, unpack)
import           Location
import           Text.Megaparsec       hiding (Token)
import           Text.Megaparsec.Error
import           Token
import           Type                  (Type (..))
--------------------------------------------------------------------------------
import           Data.List             (intercalate)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty    as NE
import           Data.Set              (Set)
import qualified Data.Set              as Set
--------------------------------------------------------------------------------

data MyParseError
  = CustomError -- Mientras no mejoremos los errores jajaja
    { msg    :: String
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
  | UnexpectedToken
    { uts :: Set (ErrorItem TokenPos)
    }
  | UnknownError
    { emsg :: String
    }
  deriving (Show, Eq)

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
      "The procedure `" <> unpack pName <> "` " <> showPos pPos <>
      " was defined with " <> show nParams <>
      (if nParams == 1 then " parameter" else " parameters") <> ", but recived " <>
       show nArgs <> (if nArgs == 1 then " argument." else " arguments.")

    BadProcedureArgumentType { paramName, pName, pPos, pType, aType} ->
      "The parameter `" <> unpack paramName <>"` of the procedure `" <> unpack pName <>
      "` " <> showPos pPos <> " has type " <> show pType <>
      ", but recived a expression with type " <> show aType

    BadReadArgument { aExpr } ->
      "The expression `" <> show aExpr <> "` is a constant expression."

    BadReadArgumentType { aExpr, aType } ->
      "The variable `" <> show aExpr <> "` has type " <> show aType <>
      "\n\tbut only variables of type "<> show GChar <>", "<> show GFloat <>
      " or " <> show GInt <>" can be read."

    EmptyBlock ->
      "Instruction blocks must contain at least one instruccion"

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

    UnexpectedToken { uts } ->
      (\x -> "Unexpected " <> show x) `concatMap` uts
      -- "Unexpected " <> show uts

    UnknownError {emsg} -> emsg

instance Ord Error where
  a <= b = True

-- Modify the pretty print of errors
prettyError :: ( Ord t
               , ShowToken t
               , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
prettyError (ParseError pos us ps xs) =
  sourcePosStackPretty pos <> ": " <> "\ESC[1;31m" <> "Error:" <> "\ESC[m\n" <>
  if Set.null us && Set.null ps && Set.null xs
    then "unknown parse error\n"
    else concat
      [ messageItemsPretty "\t Found unexpected: " us
      , messageItemsPretty "\t instead of: "  ps
      , unlines . fmap ("\t"<>) $ (showErrorComponent <$> Set.toAscList xs)
      ]

messageItemsPretty :: ShowErrorComponent a
  => String            -- ^ Prefix to prepend
  -> Set a             -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | Set.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . Set.toAscList . Set.map showErrorComponent
    in prefix <> f ts <> "\n"


orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs
