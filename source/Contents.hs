module Contents where
--------------------------------------------------------------------------------
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Data.Text           (Text, unpack)
import           Text.Megaparsec.Pos (SourcePos)
--------------------------------------------------------------------------------

data VarBehavior = Constant | Variable
    deriving (Eq)


instance Show VarBehavior where
    show Constant = "const"
    show Variable = "var"


data Value = I Integer | C Char | D Double | S String | B Bool
    deriving (Eq)

instance Show Value where
    show (I a) = show a
    show (C a) = show a
    show (D a) = show a
    show (S a) = show a
    show (B a) = show a


data Contents s
    = Contents
        { symbolName     :: Text
        , symbolBehavior :: VarBehavior
        , symbolLoc      :: SourcePos
        , symbolType     :: Type
        , symbolValue    :: Maybe Value
        , symbolInit     :: Bool
        }
    | ArgProcCont
        { argName    :: Text
        , argTypeArg :: TypeArg
        , argLoc     :: SourcePos
        , argType    :: Type
        }
    | FunctionCon
        { funcName  :: Text
        , funcLoc   :: SourcePos
        , funcType  :: Type
        , funcArgs  :: [Text]
        , funcTable :: s
        }
    | ProcCon
        { procName  :: Text
        , procLoc   :: SourcePos
        , procType  :: Type
        , procArgs  :: [Text]
        , procTable :: s
        }
    | AbstractContent
        { abstractName :: Text
        , abstractLoc  :: SourcePos
        -- , abstractTable :: s
        }
    | TypeContent
        { typeName :: Text
        , typeLoc  :: SourcePos
        -- , typeTable :: s
        }
    deriving (Eq)

instance Treelike s => Treelike (Contents s) where
    toTree (Contents name behavior pos sType value initialize) =
        Node ("Variable `" ++ unpack name ++ "` " ++ showPos pos) $
            [ leaf $ "Behavior: "    ++ show behavior
            , leaf $ "Type: "        ++ show sType
            ] ++ case value of
                    Just x -> [leaf $ "Value: " ++ show x]
                    _      -> []

    toTree (ArgProcCont name targ pos sType) =
        Node ("Argument `" ++ unpack name ++ "` " ++ showPos pos)
            [ leaf $ "Arg Type: "    ++ show targ
            , leaf $ "Type: "        ++ show sType
            ]

    toTree (FunctionCon name pos sType args st) =
        Node ("Function `" ++ unpack name ++
              "` : " ++ show sType ++ " " ++ showPos pos) []

    toTree (ProcCon name pos sType args st) =
        Node ("Procedure `" ++ unpack name ++ "` " ++ showPos pos) []

    toTree (AbstractContent name pos) =
        Node ("Abstract Type `" ++ unpack name ++ "` " ++ showPos pos) []

    toTree (TypeContent name pos) =
        Node ("Type `" ++ unpack name ++ "` " ++ showPos pos) []

instance Show a => Show (Contents a) where
    show (Contents _ var pos t v i) =
        show var           ++
        ", Tipo: "         ++ show t  ++
        ", Declarada en: " ++ showPos pos ++
        ", Valor: "        ++ show v ++
        ", Inicializada: " ++ show i

    show (ArgProcCont _ argT pos t) =
        show argT ++
        ", Tipo: "         ++ show t ++
        ", Declarada en: " ++ showPos pos
    show (FunctionCon _ pos t args _) =
        ", Tipo: "         ++ show t    ++
        ", Declarada en: " ++ showPos pos ++
        ", Argumentos: "   ++ show (map unpack args)
    show (ProcCon _ _ _ ln sb) =
        show ln ++ show sb


isInitialized :: Contents a -> Bool
isInitialized (Contents _ _ _ _ _ ini) = ini
isInitialized ArgProcCont {}           = True
isInitialized FunctionCon {}           = True
isInitialized ProcCon {}               = True
isInitialized _                        = False


isRValue :: Contents a -> Bool
isRValue Contents {}                = True
isRValue (ArgProcCont _ In    _ _ ) = True
isRValue (ArgProcCont _ InOut _ _ ) = True
isRValue (ArgProcCont _ Ref   _ _ ) = True
isRValue FunctionCon {}             = True
isRValue ProcCon {}                 = True
isRValue _                          = False


isLValue :: Contents a -> Bool
isLValue (Contents _ Variable _ _ _ _) = True
isLValue (ArgProcCont _ Out   _ _)     = True
isLValue (ArgProcCont _ InOut _ _)     = True
isLValue (ArgProcCont _ Ref   _ _)     = True
isLValue _                             = False


isArg :: Contents a -> Bool
isArg Contents {} = False
isArg _           = True


initSymbolContent :: Contents a -> Contents a
initSymbolContent (Contents n vb pos t v _) = Contents n vb pos t v True
initSymbolContent c                         = c


getPos :: Contents a -> SourcePos
getPos (Contents _ _ l _ _ _)  = l
getPos (ArgProcCont _ _ l _ )  = l
getPos (FunctionCon _ l _ _ _) = l
getPos (ProcCon _ l _ _ _)     = l
getPos (AbstractContent _ l)   = l
getPos (TypeContent _ l)       = l


getContentType :: Contents a -> Type
getContentType (Contents _ _ _ t _ _)  = t
getContentType (ArgProcCont _ _ _ t )  = t
getContentType (FunctionCon _ _ t _ _) = t
getContentType _                       = GEmpty


getVarBeh :: Contents a -> Maybe VarBehavior
getVarBeh (Contents _ vb _ _ _ _) = Just vb
getVarBeh _                       = Nothing


getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont _ pat _ _) = Just pat
getProcArgType _                     = Nothing
