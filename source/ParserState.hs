module ParserState where
--------------------------------------------------------------------------------
-- import           Parser.TokenParser
import           AST                    hiding (Constant)
import           Contents
import           Data.Maybe
import           Graciela
import           MyParseError
import           ParserError
import           SymbolTable
import           Token
import           Type
import           TypeError
--------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (.=))
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (StateT, get, modify)
import           Data.Foldable          (toList)
import           Data.Function          (on)
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq (empty, null, sortBy)
import qualified Data.Set               as Set (Set, empty, insert)
import           Data.Text              (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Pos    (SourcePos)
--------------------------------------------------------------------------------

addFileToReadParser :: String -> Graciela ()
addFileToReadParser file = filesToRead %= Set.insert file


addFunTypeParser :: Text
                 -> Maybe [(Text, Type)]
                 -> Type
                 -> SourcePos
                 -> SymbolTable
                 -> Graciela ()
addFunTypeParser id (Just lt) t pos sb =
    addSymbolParser id (FunctionCon id pos (GFunction snds t) fsts sb)
    where
        (fsts, snds) = unzip lt

addFunTypeParser _ _ _ _ _ = return ()


addProcTypeParser :: Text
                  -> Maybe [(Text, Type)]
                  -> SourcePos
                  -> SymbolTable -> Graciela ()
addProcTypeParser id (Just xs) pos sb =
    addSymbolParser id $ ProcCon id pos (GProcedure snds) fsts sb
    where (fsts, snds) = unzip xs
addProcTypeParser _ _ _ _             = return ()


getCurrentScope :: Graciela SymbolTable
getCurrentScope = use symbolTable


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



addManyUniSymParser :: Maybe [(Text, SourcePos)] -> Type -> Graciela ()
addManyUniSymParser (Just xs) t = f xs t
    where
        f ((id, pos):xs) t = do
            addSymbolParser id $ Contents id Variable pos t Nothing False
            f xs t
        f [] _ = return ()

addManyUniSymParser _ _ = return ()


addManySymParser :: VarBehavior
                 -> Maybe [(Text , SourcePos)]
                 -> Type
                 -> Maybe [AST Type]
                 -> Graciela ()
addManySymParser vb (Just xs) t (Just ys) =
    if length xs /= length ys then
        do pos <- getPosition
           sTableErrorList %= (|> IncomDefError vb pos)
    else f vb xs t ys
      where
        f vb ((id, pos):xs) t (ast:ys) =
            do addSymbolParser id $ Contents id vb pos t (astToValue ast) True
               f vb xs t ys
        f _ [] _ []                    = return ()

addManySymParser _ _ _ _               = return ()


astToValue (Int _ n _)    = Just $ I n
astToValue (Float _ f _)  = Just $ D f
astToValue (Bool _ b _)   = Just $ B b
astToValue (Char _ c _)   = Just $ C c
astToValue (String _ s _) = Just $ S s
astToValue _              = Nothing


verifyReadVars :: Maybe [(Text, SourcePos)] -> Graciela [Type]
verifyReadVars (Just lid) = catMaybes <$> mapM (lookUpConsParser . fst) lid
verifyReadVars _          = return []


addFunctionArgParser :: Text -> Text -> Type -> SourcePos -> Graciela ()
addFunctionArgParser idf id t pos =
    if id /= idf then
        addSymbolParser id $ Contents id Constant pos t Nothing True
    else
        typeError $ FunctionNameError id pos


addArgProcParser :: Text -> Text
                 -> Type -> SourcePos
                 -> Maybe TypeArg -> Graciela ()
addArgProcParser id pid t pos (Just targ) =
    if id /= pid then
      addSymbolParser id $ ArgProcCont id targ pos t
    else
      typeError $ FunctionNameError id pos
addArgProcParser _ _ _ _ _ = return ()


addSymbolParser :: Text -> Contents SymbolTable -> Graciela ()
addSymbolParser symbol content = do
    st <- use symbolTable
    case addSymbol symbol content st of
        Left con ->
            sTableErrorList %=
                (|> (RepSymbolError symbol `on` getPos) con content)
        Right sb ->
            symbolTable .= sb


addCuantVar :: OpQuant -> Text -> Type -> SourcePos -> Graciela ()
addCuantVar op id t pos =
    if isQuantifiable t then
       addSymbolParser id $ Contents id Constant pos t Nothing True
    else
       typeError $ UncountableError op pos


lookUpSymbol :: Text -> Graciela (Maybe (Contents SymbolTable))
lookUpSymbol id = do
    st <- use symbolTable
    case checkSymbol id st of
        Nothing -> do
            addNonDeclVarError id
            return Nothing
        Just c  -> return $ Just c



lookUpVarParser :: Text -> SourcePos -> Graciela (Maybe Type)
lookUpVarParser id pos = do
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


lookUpConstIntParser :: Text -> SourcePos -> Graciela (Maybe Type)
lookUpConstIntParser id pos = do
    symbol <- lookUpSymbol id
    pos <- getPosition
    case symbol of
        Nothing -> return Nothing
        Just content  ->
            if isInitialized content then
                if isRValue content then
                    if getContentType content == GInt then
                        return $ return GInt
                    else do
                        typeError $ NotIntError id pos
                        return Nothing
                else do
                    typeError $ NotRValueError id pos
                    return Nothing
            else do
                typeError $ NotInitError id pos
                return Nothing


addConsIdError :: Text -> Graciela ()
addConsIdError id = do
    pos <- getPosition
    sTableErrorList %= (|> ConstIdError id pos)


addNonDeclVarError :: Text -> Graciela ()
addNonDeclVarError id = do
    pos <- getPosition
    sTableErrorList %= (|> ConstIdError id pos)


addNonAsocError :: Graciela ()
addNonAsocError = do
    pos <- getPosition
    synErrorList %= (|> NonAsocError pos)


addArrayCallError :: Int -> Int -> Graciela ()
addArrayCallError waDim prDim = do
    pos <- getPosition
    synErrorList %= (|> ArrayError waDim prDim pos)


-- genNewError :: Graciela Token -> ExpectedToken -> Graciela ()
-- genNewError laset msg = do
--     pos <- cleanEntry laset
--     synErrorList %= (|> newParseError msg pos)


genCustomError :: String -> Graciela ()
genCustomError msg = do
    pos <- getPosition
    synErrorList %= (|> CustomError msg pos)


genNewEmptyError :: Graciela ()
genNewEmptyError = do
    pos <- getPosition
    synErrorList %= (|> EmptyError pos)
