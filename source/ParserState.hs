module ParserState where
--------------------------------------------------------------------------------
import           Parser.TokenParser
import           AST                 hiding (Constant)
import           Contents
import           Data.Maybe
import           Location
import           MyParseError
import           TypeError
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


addFileToReadParser :: String -> Graciela ()
addFileToReadParser file = do
        filesToRead %= Set.insert file

addFunTypeParser :: Text 
                 -> Maybe [(Text, Type)] 
                 -> Type 
                 -> Location
                 -> SymbolTable 
                 -> Graciela ()
addFunTypeParser id (Just lt) t loc sb =
    addSymbolParser id (FunctionCon id loc (GFunction snds t) fsts sb)
    where 
        (fsts, snds) = unzip lt

addFunTypeParser _ _ _ _ _ = return ()


addProcTypeParser :: Text 
                  -> Maybe [(Text, Type)] 
                  -> Location
                  -> SymbolTable -> Graciela ()
addProcTypeParser id (Just xs) loc sb =
    addSymbolParser id $ ProcCon id loc (GProcedure snds) fsts sb
    where (fsts, snds) = unzip xs
addProcTypeParser _ _ _ _             = return ()


getCurrentScope :: Graciela SymbolTable
getCurrentScope = do
    st <- use symbolTable
    return $ st


newScopeParser :: Graciela ()
newScopeParser = symbolTable %= enterScope


getScopeParser :: Graciela Int
getScopeParser = do st <- use symbolTable
                    return $ getScope st


exitScopeParser :: Graciela ()
exitScopeParser = do
    st <- use symbolTable
    case exitScope st of
        Nothing   -> synErrorList %= (|> ScopesError)
        Just sbtl -> symbolTable .= sbtl
        


addManyUniSymParser :: Maybe [(Text, Location)] -> Type -> Graciela ()
addManyUniSymParser (Just xs) t = f xs t
    where
        f ((id, loc):xs) t = do
            addSymbolParser id $ Contents id Variable loc t Nothing False
            f xs t
        f [] _ = return ()

addManyUniSymParser _ _ = return ()


addManySymParser :: VarBehavior 
                 -> Maybe [(Text , Location)] 
                 -> Type
                 -> Maybe [AST Type] 
                 -> Graciela ()
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


verifyReadVars :: Maybe [(Text, Location)] -> Graciela [Type]
verifyReadVars (Just lid) = catMaybes <$> mapM (lookUpConsParser . fst) lid
verifyReadVars _          = return []


addFunctionArgParser :: Text -> Text -> Type -> Location -> Graciela ()
addFunctionArgParser idf id t loc =
    if id /= idf then
        addSymbolParser id $ Contents id Constant loc t Nothing True
    else
        typeError $ FunctionNameError id loc


addArgProcParser :: Text -> Text 
                 -> Type -> Location
                 -> Maybe TypeArg -> Graciela ()
addArgProcParser id pid t loc (Just targ) =
    if id /= pid then
      addSymbolParser id $ ArgProcCont id targ loc t
    else
      typeError $ FunctionNameError id loc
addArgProcParser _ _ _ _ _ = return ()


addSymbolParser :: Text -> Contents SymbolTable -> Graciela ()
addSymbolParser symbol content = do 
    st <- use symbolTable
    case addSymbol symbol content st of
        Left con ->
            sTableErrorList %=
                (|> (RepSymbolError symbol `on` getLoc) con content) 
        Right sb ->
            symbolTable .= sb
        


addCuantVar :: OpQuant -> Text -> Type -> Location -> Graciela ()
addCuantVar op id t loc =
    if isQuantifiable t then
       addSymbolParser id $ Contents id Constant loc t Nothing True
    else
       typeError $ UncountableError op loc


lookUpSymbol :: Text -> Graciela (Maybe (Contents SymbolTable))
lookUpSymbol id = do
    st <- use symbolTable
    case checkSymbol id st of
        Nothing -> do
            addNonDeclVarError id
            return Nothing
        Just c  -> return $ Just c



lookUpVarParser :: Text -> Location -> Graciela (Maybe Type)
lookUpVarParser id loc = do
    st <- use symbolTable
    c  <- lookUpSymbol id
    case c of
        Just content -> return $ fmap getContentType c
        _ -> return Nothing



lookUpConsParser :: Text -> Graciela (Maybe Type)
lookUpConsParser id = do
    symbol <- lookUpSymbol id
    case symbol of
        Just content ->
            if isLValue content then do
                symbolTable %= initSymbol id
                return $ Just $ getContentType content
            else do
                addConsIdError id
                return Nothing
        _   -> return Nothing


lookUpConstIntParser :: Text -> Location -> Graciela (Maybe Type)
lookUpConstIntParser id loc = do
    symbol <- lookUpSymbol id
    loc <- getPosition
    case symbol of
        Nothing -> return Nothing
        Just content  ->
            if isInitialized content then
                if isRValue content then
                    if getContentType content == GInt then
                        return $ return GInt
                    else do
                        typeError $ NotIntError id (toLocation loc)
                        return Nothing
                else do
                    typeError $ NotRValueError id (toLocation loc)
                    return Nothing
            else do
                typeError $ NotInitError id (toLocation loc)
                return Nothing


addConsIdError :: Text -> Graciela ()
addConsIdError id = do
    pos <- getPosition
    sTableErrorList %= (|> ConstIdError id (toLocation pos))

addNonDeclVarError :: Text -> Graciela ()
addNonDeclVarError id = do
    pos <- getPosition
    sTableErrorList %= (|> ConstIdError id (toLocation pos))

addNonAsocError :: Graciela ()
addNonAsocError = do
    pos <- getPosition
    synErrorList %= (|> NonAsocError (toLocation pos))

addArrayCallError :: Int -> Int -> Graciela ()
addArrayCallError waDim prDim = do
    pos <- getPosition
    synErrorList %= (|> ArrayError waDim prDim (toLocation pos))

genNewError :: Graciela Token -> ExpectedToken -> Graciela ()
genNewError laset msg = do
    pos <- cleanEntry laset
    synErrorList %= (|> newParseError msg pos)

genCustomError :: String -> Graciela ()
genCustomError msg = do 
    pos <- getPosition
    synErrorList %= (|> CustomError msg (toLocation pos))    

genNewEmptyError :: Graciela ()
genNewEmptyError = do
    pos <- getPosition
    synErrorList %= (|> newEmptyError pos)
