module ParserState where
--------------------------------------------------------------------------------
import           AST                 hiding (Constant)
import           Contents
import           Data.Maybe
import           Location
import           MyParseError
import           MyTypeError
import           ParserError
import           State
import           SymbolTable
import           Text.Parsec
import           Token
import           TokenParser
import           Type
--------------------------------------------------------------------------------
import           Control.Monad.State (get, modify)
import           Data.Text           (Text)
--------------------------------------------------------------------------------


addFileToReadParser :: String -> MyParser()
addFileToReadParser file = modify $ addFileToRead file


addFunTypeParser :: Text -> Maybe [(Text, Type)] -> Type -> Location
                 -> SymbolTable -> MyParser()
addFunTypeParser id (Just lt) t loc sb =
    addSymbolParser id (FunctionCon loc (MyFunction snds t) fsts sb)
    where (fsts, snds) = unzip lt
addFunTypeParser _ _ _ _ _ = return ()


addProcTypeParser :: Text -> Maybe [(Text, Type)] -> Location
                  -> SymbolTable -> MyParser()
addProcTypeParser id (Just xs) loc sb =
    addSymbolParser id $ ProcCon loc (MyProcedure snds) fsts sb
    where (fsts, snds) = unzip xs
addProcTypeParser _ _ _ _             = return ()


getActualScope :: MyParser SymbolTable
getActualScope = do s <- get
                    return $ symbolTable s


newScopeParser :: MyParser ()
newScopeParser = modify newScopeState


getScopeParser :: MyParser Int
getScopeParser = do st <- get
                    return $ getScopeState st


exitScopeParser :: MyParser ()
exitScopeParser = modify exitScopeState


addManyUniSymParser :: Maybe [(Text, Location)] -> Type -> MyParser()
addManyUniSymParser (Just xs) t = f xs t
    where
        f ((id, loc):xs) t = do
            addSymbolParser id $ Contents Variable loc t Nothing False
            f xs t
        f [] _ = return()

addManyUniSymParser _ _ = return()


addManySymParser :: VarBehavior -> Maybe [(Text , Location)] -> Type
                 -> Maybe [AST Type] -> MyParser()
addManySymParser vb (Just xs) t (Just ys) =
    if length xs /= length ys then
        do pos <- getPosition
           modify $ addTypeError $ IncomDefError vb (toLocation pos)
    else f vb xs t ys
      where
        f vb ((id, loc):xs) t (ast:ys) =
            do addSymbolParser id $ Contents vb loc t (astToValue ast) True
               f vb xs t ys
        f _ [] _ []                    = return()

addManySymParser _ _ _ _               = return()


astToValue (Int _ n _)    = Just $ I n
astToValue (Float _ f _)  = Just $ D f
astToValue (Bool _ b _)   = Just $ B b
astToValue (Char _ c _)   = Just $ C c
astToValue (String _ s _) = Just $ S s
astToValue _              = Nothing


verifyReadVars :: Maybe [(Text, Location)] -> MyParser [Type]
verifyReadVars (Just lid) = catMaybes <$> mapM (lookUpConsParser . fst) lid
verifyReadVars _          = return []


addFunctionArgParser :: Text -> Text -> Type -> Location -> MyParser ()
addFunctionArgParser idf id t loc =
    if id /= idf then
        addSymbolParser id $ Contents Constant loc t Nothing True
    else
        addFunctionNameError id loc


addArgProcParser :: Text -> Text -> Type -> Location
                 -> Maybe TypeArg -> MyParser ()
addArgProcParser id pid t loc (Just targ) =
    if id /= pid then
      addSymbolParser id $ ArgProcCont targ loc t
    else
      addFunctionNameError id loc
addArgProcParser _ _ _ _ _ = return()


addSymbolParser :: Text -> Contents SymbolTable -> MyParser ()
addSymbolParser id c = do modify $ addNewSymbol id c
                          return()


addCuantVar :: OpQuant -> Text -> Type -> Location -> MyParser()
addCuantVar op id t loc =
    if isCuantificable t then
       addSymbolParser id $ Contents Constant loc t Nothing True
    else
       addUncountableError op loc


lookUpSymbol :: Text -> MyParser (Maybe (Contents SymbolTable))
lookUpSymbol id = do
    st <- get
    case lookUpVarState id (symbolTable st) of
        Nothing -> do
            addNonDeclVarError id
            return Nothing
        Just c  -> return $ Just c



lookUpVarParser :: Text -> Location -> MyParser (Maybe Type)
lookUpVarParser id loc = do
    st <- get
    c  <- lookUpSymbol id
    case c of
        Just c' -> return $ fmap symbolType c
        Nothing -> return Nothing



lookUpConsParser :: Text -> MyParser (Maybe Type)
lookUpConsParser id = do
    c <- lookUpSymbol id
    case c of
        Nothing   -> return Nothing
        Just a    ->
            if isLValue a then do
                newInitVar id
                return $ Just $ symbolType a
            else do
                addConsIdError id
                return Nothing



newInitVar :: Text -> MyParser()
newInitVar id = do
    modify $ initVar id
    return ()


lookUpConstIntParser :: Text -> Location -> MyParser (Maybe Type)
lookUpConstIntParser id loc = do
    c <- lookUpSymbol id
    loc <- getPosition
    case c of
        Nothing -> return Nothing
        Just a  ->
            if isInitialized a then
                if isRValue a then
                    if symbolType a == MyInt then
                        return $ return MyInt
                    else do
                        addNotIntError id (toLocation loc)
                        return Nothing
                else do
                    addNotRValueError id (toLocation loc)
                    return Nothing
            else do
                addNotInitError id (toLocation loc)
                return Nothing


addConsIdError :: Text -> MyParser ()
addConsIdError id = do
    pos <- getPosition
    modify $ addTypeError (ConstIdError id (toLocation pos))
    return ()


addNonDeclVarError :: Text -> MyParser ()
addNonDeclVarError id = do
    pos <- getPosition
    modify $ addTypeError $ NonDeclError id (toLocation pos)
    return ()


addNonAsocError :: MyParser ()
addNonAsocError = do
    pos <- getPosition
    modify $ addParsingError $ NonAsocError (toLocation pos)
    return ()


addArrayCallError :: Int -> Int -> MyParser ()
addArrayCallError waDim prDim = do
    pos <- getPosition
    modify $ addParsingError $ ArrayError waDim prDim (toLocation pos)
    return ()


genNewError :: MyParser Token -> ExpectedToken -> MyParser ()
genNewError laset msg = do
    pos <- cleanEntry laset
    modify $ addParsingError $ newParseError msg pos
    return ()


genNewEmptyError :: MyParser ()
genNewEmptyError = do
    pos <- getPosition
    modify $ addParsingError $ newEmptyError pos
    return ()

addOutOfBoundsError :: Text -> Location -> MyParser ()
addOutOfBoundsError t l = do
    modify $ addTypeError $ IntOutOfBounds t l
    return ()


addUncountableError :: OpQuant -> Location -> MyParser ()
addUncountableError op loc = do
    modify $ addTypeError $ UncountError op loc
    return ()


addFunctionNameError :: Text -> Location -> MyParser ()
addFunctionNameError id loc = do
    modify $ addTypeError $ FunNameError id loc
    return ()


addNotIntError :: Text -> Location -> MyParser ()
addNotIntError id loc = do
    modify $ addTypeError $ NotIntError id loc
    return ()


addNotConsIdError :: Text -> Location -> MyParser ()
addNotConsIdError id loc = do
    modify $ addTypeError $ NotConstError id loc
    return ()


addNotInitError :: Text -> Location -> MyParser()
addNotInitError id loc = do
    modify $ addTypeError $ NotInitError id loc
    return ()


addNotRValueError :: Text -> Location -> MyParser()
addNotRValueError id loc = do
    modify $ addTypeError $ NotRValueError id loc
    return ()
