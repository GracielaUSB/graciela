module ParserState where
--------------------------------------------------------------------------------
import           Parser.TokenParser
import           AST                 hiding (Constant)
import           Contents
import           Data.Maybe
import           Location
import           MyParseError
import           MyTypeError
import           ParserError
import           Graciela
import           SymbolTable
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad.Identity (Identity)
import           Control.Lens           (use, (.=), (%=))
import           Control.Monad.State    (StateT)
import           Control.Monad.State    (get, modify)
import           Data.Foldable          (toList)
import           Data.Function          (on)
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq (empty, null, sortBy)
import           Data.Text              (Text)
import           Text.Parsec
import qualified Data.Set               as Set (Set, empty, insert)
--------------------------------------------------------------------------------


addFileToReadParser :: String -> MyParser ()
addFileToReadParser file = do
        filesToRead %= Set.insert file
        return ()

addFunTypeParser :: Text 
                 -> Maybe [(Text, Type)] 
                 -> Type 
                 -> Location
                 -> SymbolTable -> MyParser ()
addFunTypeParser id (Just lt) t loc sb =
    addSymbolParser id (FunctionCon id loc (GFunction snds t) fsts sb)
    where 
        (fsts, snds) = unzip lt

addFunTypeParser _ _ _ _ _ = return ()


addProcTypeParser :: Text 
                  -> Maybe [(Text, Type)] 
                  -> Location
                  -> SymbolTable -> MyParser ()
addProcTypeParser id (Just xs) loc sb =
    addSymbolParser id $ ProcCon id loc (GProcedure snds) fsts sb
    where (fsts, snds) = unzip xs
addProcTypeParser _ _ _ _             = return ()


getCurrentScope :: MyParser SymbolTable
getCurrentScope = do
    st <- use symbolTable
    return $ st


newScopeParser :: MyParser ()
newScopeParser = symbolTable %= enterScope


getScopeParser :: MyParser Int
getScopeParser = do st <- use symbolTable
                    return $ getScope st


exitScopeParser :: MyParser ()
exitScopeParser = do
    st <- use symbolTable
    case exitScope st of
        Nothing   -> synErrorList %= (|> ScopesError)
        Just sbtl -> symbolTable .= sbtl
        


addManyUniSymParser :: Maybe [(Text, Location)] -> Type -> MyParser ()
addManyUniSymParser (Just xs) t = f xs t
    where
        f ((id, loc):xs) t = do
            addSymbolParser id $ Contents id Variable loc t Nothing False
            f xs t
        f [] _ = return ()

addManyUniSymParser _ _ = return ()


addManySymParser :: VarBehavior -> Maybe [(Text , Location)] -> Type
                 -> Maybe [AST Type] -> MyParser ()
addManySymParser vb (Just xs) t (Just ys) =
    if length xs /= length ys then
        do pos <- getPosition
           sTableErrorList %= (|> IncomDefError vb (toLocation pos))
    else f vb xs t ys
      where
        f vb ((id, loc):xs) t (ast:ys) =
            do addSymbolParser id $ Contents id vb loc t (astToValue ast) True
               f vb xs t ys
        f _ [] _ []                    = return ()

addManySymParser _ _ _ _               = return ()


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
        addSymbolParser id $ Contents id Constant loc t Nothing True
    else
        addFunctionNameError id loc


addArgProcParser :: Text -> Text -> Type -> Location
                 -> Maybe TypeArg -> MyParser ()
addArgProcParser id pid t loc (Just targ) =
    if id /= pid then
      addSymbolParser id $ ArgProcCont id targ loc t
    else
      addFunctionNameError id loc
addArgProcParser _ _ _ _ _ = return ()


addSymbolParser :: Text -> Contents SymbolTable -> MyParser ()
addSymbolParser symbol content = do 
    st <- use symbolTable
    case addSymbol symbol content st of
        Left con ->
            sTableErrorList %=
                (|> (RepSymbolError symbol `on` symbolLoc) con content) 
        Right sb ->
            symbolTable .= sb
        


addCuantVar :: OpQuant -> Text -> Type -> Location -> MyParser ()
addCuantVar op id t loc =
    if isQuantifiable t then
       addSymbolParser id $ Contents id Constant loc t Nothing True
    else
       addUncountableError op loc


lookUpSymbol :: Text -> MyParser (Maybe (Contents SymbolTable))
lookUpSymbol id = do
    st <- use symbolTable
    case checkSymbol id st of
        Nothing -> do
            addNonDeclVarError id
            return Nothing
        Just c  -> return $ Just c



lookUpVarParser :: Text -> Location -> MyParser (Maybe Type)
lookUpVarParser id loc = do
    st <- use symbolTable
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



newInitVar :: Text -> MyParser ()
newInitVar id = do
    symbolTable %= initSymbol id
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
                    if symbolType a == GInt then
                        return $ return GInt
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
    sTableErrorList %= (|> ConstIdError id (toLocation pos))
    return ()


addNonDeclVarError :: Text -> MyParser ()
addNonDeclVarError id = do
    pos <- getPosition
    sTableErrorList %= (|> ConstIdError id (toLocation pos))
    return ()


addNonAsocError :: MyParser ()
addNonAsocError = do
    pos <- getPosition
    synErrorList %= (|> NonAsocError (toLocation pos))
    return ()


addArrayCallError :: Int -> Int -> MyParser ()
addArrayCallError waDim prDim = do
    pos <- getPosition
    synErrorList %= (|> ArrayError waDim prDim (toLocation pos))
    return ()


genNewError :: MyParser Token -> ExpectedToken -> MyParser ()
genNewError laset msg = do
    pos <- cleanEntry laset
    synErrorList %= (|> newParseError msg pos)
    return ()


genNewEmptyError :: MyParser ()
genNewEmptyError = do
    pos <- getPosition
    synErrorList %= (|> newEmptyError pos)
    return ()

addOutOfBoundsError :: Text -> Location -> MyParser ()
addOutOfBoundsError t l = do
    sTableErrorList %= (|> IntOutOfBounds t l)
    return ()


addUncountableError :: OpQuant -> Location -> MyParser ()
addUncountableError op loc = do
    sTableErrorList %= (|> UncountError op loc)
    return ()


addFunctionNameError :: Text -> Location -> MyParser ()
addFunctionNameError id loc = do
    sTableErrorList %= (|> FunNameError id loc)
    return ()


addNotIntError :: Text -> Location -> MyParser ()
addNotIntError id loc = do
    sTableErrorList %= (|> NotIntError id loc)
    return ()


addNotConsIdError :: Text -> Location -> MyParser ()
addNotConsIdError id loc = do
    sTableErrorList %= (|> NotConstError id loc)
    return ()


addNotInitError :: Text -> Location -> MyParser ()
addNotInitError id loc = do

    sTableErrorList %= (|> NotInitError id loc)
    return ()


addNotRValueError :: Text -> Location -> MyParser ()
addNotRValueError id loc = do
    sTableErrorList %= (|> NotRValueError id loc)
    return ()
