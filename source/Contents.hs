module Contents where
--------------------------------------------------------------------------------
import           Location
import           Type
import           Treelike
--------------------------------------------------------------------------------
import           Data.Text (Text, unpack)
--------------------------------------------------------------------------------

data VarBehavior = Constant | Variable
    deriving (Eq)


instance Show VarBehavior where
    show Constant = " es una Constante"
    show Variable = " es una Variable"


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
        { symbolName :: Text
        , varBeh     :: VarBehavior
        , symbolLoc  :: Location
        , symbolType :: Type
        , value      :: Maybe Value
        , ini        :: Bool
        }
    | ArgProcCont
        { symbolName  :: Text
        , procArgType :: TypeArg
        , symbolLoc   :: Location
        , symbolType  :: Type
        }
    | FunctionCon
        { symbolName :: Text
        , symbolLoc  :: Location
        , symbolType :: Type
        , nameArgs   :: [Text]
        , sTable     :: s
        }
    | ProcCon
        { symbolName :: Text
        , symbolLoc  :: Location
        , symbolType :: Type
        , nameArgs   :: [Text]
        , sTable     :: s
        }
    deriving (Eq)


instance Treelike s => Treelike (Contents s) where
    toTree (Contents name behavior loc sType value initialize) =
        Node (unpack name ++ " " ++ showL loc) $
            [ leaf $ "Behavior: "    ++ show behavior
            , leaf $ "Type: "        ++ show sType
            , leaf $ "Value: "       ++ show value
            , leaf $ "Initialized: " ++ show initialize
            ]
            
    toTree (ArgProcCont name targ loc sType) =
        Node (unpack name ++ " " ++ showL loc) $
            [ leaf $ "Arg Type: "    ++ show targ
            , leaf $ "Type: "        ++ show sType
            ]

    toTree (FunctionCon name loc sType args st) =
        Node ("Function " ++ unpack name ++ " -> " ++ show sType ++ " " ++ showL loc)
            (if null args 
                then [] 
                else [Node "Argments: " (fmap (leaf . unpack) args)])

    toTree (ProcCon name loc sType args st) =
        Node ("Procedure " ++ unpack name ++ " " ++ showL loc)
            (if null args 
                then [] 
                else [Node "Argments: " (fmap (leaf . unpack) args)])



instance Show a => Show (Contents a) where
    show (Contents _ var loc t v i) =
        show var           ++ 
        ", Tipo: "         ++ show t  ++ 
        ", Declarada en: " ++ showL loc ++ 
        ", Valor: "        ++ show v ++ 
        ", Inicializada: " ++ show i

    show (ArgProcCont _ argT loc t) =
        show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ showL loc
    show (FunctionCon _ loc t args _) =
        ", Tipo: " ++ show t  ++ ", Declarada en: " ++ showL loc ++
        ", Argumentos: " ++ show (map unpack args)
    show (ProcCon _ _ _ ln sb) =
        show ln ++ show sb


isInitialized :: Contents a -> Bool
isInitialized (Contents _ _ _ _ _ True) = True
isInitialized (ArgProcCont {})        = True
isInitialized (FunctionCon {})        = True
isInitialized (ProcCon {}    )        = True
isInitialized _                       = False


isRValue :: Contents a -> Bool
isRValue (Contents {})              = True
isRValue (ArgProcCont _ In    _ _ ) = True
isRValue (ArgProcCont _ InOut _ _ ) = True
isRValue (ArgProcCont _ Ref   _ _ ) = True
isRValue (FunctionCon {})           = True
isRValue (ProcCon {})               = True
isRValue _                          = False


isLValue :: Contents a -> Bool
isLValue (Contents _ Variable _ _ _ _) = True
isLValue (ArgProcCont _ Out   _ _)     = True
isLValue (ArgProcCont _ InOut _ _)     = True
isLValue (ArgProcCont _ Ref   _ _)     = True
isLValue _                             = False


isArg :: Contents a -> Bool
isArg (Contents {}) = False
isArg _             = True


initSymbolContent :: Contents a -> Contents a
initSymbolContent (Contents n vb loc t v _) = Contents n vb loc t v True
initSymbolContent c                       = c


getVarBeh :: Contents a -> Maybe VarBehavior
getVarBeh (Contents _ vb _ _ _ _) = Just vb
getVarBeh _                       = Nothing


getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont _ pat _ _) = Just pat
getProcArgType _                     = Nothing
