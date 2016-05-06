module Contents where
--------------------------------------------------------------------------------
import           Location
import           Type
--------------------------------------------------------------------------------
import           Data.Text (Text, unpack)
--------------------------------------------------------------------------------

data VarBehavior = Constant | Variable
    deriving (Eq)


instance Show VarBehavior where
    show Constant = " es una Constante"
    show Variable = " es una Variable"


data Value = I Integer | C Char | D Double | S String | B Bool
    deriving (Show, Eq)


data Contents s
    = Contents
        { varBeh     :: VarBehavior
        , symbolLoc  :: Location
        , symbolType :: Type
        , value      :: Maybe Value
        , ini        :: Bool
        }
    | ArgProcCont
        { procArgType :: TypeArg
        , symbolLoc   :: Location
        , symbolType  :: Type
        }
    | FunctionCon
        { symbolLoc  :: Location
        , symbolType :: Type
        , nameArgs   :: [Text]
        , sTable     :: s
        }
    | ProcCon
        { symbolLoc  :: Location
        , symbolType :: Type
        , nameArgs   :: [Text]
        , sTable     :: s
        }
    deriving (Eq)


instance Show a => Show (Contents a) where
    show (Contents var loc t v i) =
        show var  ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++
        show loc ++ ", Valor: " ++ show v ++ ", Inicializada: " ++ show i
    show (ArgProcCont argT loc t) =
        show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc
    show (FunctionCon loc t args _) =
        ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc ++
        ", Argumentos: " ++ show (map unpack args)
    show (ProcCon _ _ ln sb) =
        show ln ++ show sb


isInitialized :: Contents a -> Bool
isInitialized (Contents _ _ _ _ True) = True
isInitialized ArgProcCont {}          = True
isInitialized FunctionCon {}          = True
isInitialized ProcCon {}              = True
isInitialized _                       = False


isRValue :: Contents a -> Bool
isRValue Contents {}              = True
isRValue (ArgProcCont In    _ _ ) = True
isRValue (ArgProcCont InOut _ _ ) = True
isRValue (ArgProcCont Ref   _ _ ) = True
isRValue FunctionCon {}           = True
isRValue ProcCon {}               = True
isRValue _                        = False


isLValue :: Contents a -> Bool
isLValue (Contents Variable _ _ _ _) = True
isLValue (ArgProcCont Out   _ _)     = True
isLValue (ArgProcCont InOut _ _)     = True
isLValue (ArgProcCont Ref   _ _)     = True
isLValue _                           = False


isArg :: Contents a -> Bool
isArg Contents {} = False
isArg _           = True


initSymbolContent :: Contents a -> Contents a
initSymbolContent (Contents vb loc t v _) = Contents vb loc t v True
initSymbolContent c                       = c


getVarBeh :: Contents a -> Maybe VarBehavior
getVarBeh (Contents vb _ _ _ _) = Just vb
getVarBeh _                     = Nothing


getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont pat _ _) = Just pat
getProcArgType _                     = Nothing
