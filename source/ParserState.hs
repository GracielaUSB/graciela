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
import           Data.Text              (Text, unpack)
import           Text.Megaparsec        hiding (Token)
import           Text.Megaparsec.Pos    (SourcePos)
--------------------------------------------------------------------------------

addFileToReadParser :: Text -> Graciela ()
addFileToReadParser file = filesToRead %= Set.insert (unpack file)


addFunTypeParser :: Text
                 -> [(Text, Type)]
                 -> Type
                 -> SourcePos
                 -> SymbolTable
                 -> Graciela ()
addFunTypeParser id params t pos st =
    addSymbolParser id (FunctionCon id pos (GFunction snds t) fsts st)
    where
        (fsts, snds) = unzip params


addProcTypeParser :: Text
                  -> [(Text, Type)]
                  -> SourcePos
                  -> SymbolTable -> Graciela ()
addProcTypeParser id params pos st =
    addSymbolParser id $ ProcCon id pos (GProcedure snds) fsts st
    where (fsts, snds) = unzip params


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



addManyUniSymParser :: [(Text, SourcePos)] -> Type -> Graciela ()
addManyUniSymParser [] _ = return ()
addManyUniSymParser xs t = mapM_ 
    (\(id,pos) -> addSymbolParser id $ Contents id Variable pos t Nothing False) xs





addManySymParser :: VarBehavior
                 -> [(Text , SourcePos)]
                 -> Type
                 -> [AST]
                 -> Graciela ()
addManySymParser  _ [] _ [] = return ()
addManySymParser vb xs t ys =
    if length xs /= length ys then
        do pos <- getPosition
           sTableErrorList %= (|> IncomDefError vb pos)
    else mapM_ (\((id,pos), ast) -> addSymbolParser id $ Contents id vb pos t (astToValue ast) True) 
               (zip xs ys) 
      





astToValue AST { ast' = (Int    n) } = Just $ I n
astToValue AST { ast' = (Float  f) } = Just $ D f
astToValue AST { ast' = (Bool   b) } = Just $ B b
astToValue AST { ast' = (Char   c) } = Just $ C c
astToValue AST { ast' = (String s) } = Just $ S s
astToValue _                         = Nothing


verifyReadVars :: [(Text, SourcePos)] -> Graciela [Type]
verifyReadVars []  = return []
verifyReadVars lid = mapM (lookUpConsParser . fst) lid



addFunctionArgParser :: Text -> Text -> Type -> SourcePos -> Graciela ()
addFunctionArgParser idf id t pos =
    if id /= idf then
        addSymbolParser id $ Contents id Constant pos t Nothing True
    else
        typeError $ FunctionNameError id pos


addArgProcParser :: Text -> Text -> Type 
                 -> SourcePos -> Maybe TypeArg 
                 -> Graciela ()
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


addCuantVar :: QuantOp -> Text -> Type -> SourcePos -> Graciela ()
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



lookUpConsParser :: Text -> Graciela Type
lookUpConsParser id = do
    symbol <- lookUpSymbol id
    case symbol of
        Just content ->
            if isLValue content then do
                symbolTable %= initSymbol id
                return $ getContentType content
            else do
                addConsIdError id
                return GError
        _   -> return GError


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


genNewError :: Graciela Token -> ExpectedToken -> Graciela ()
genNewError laset msg = do
    pos <- getPosition
    laset >>= \token -> synErrorList %= (|> newParseError msg (token, pos))


genCustomError :: String -> Graciela ()
genCustomError msg = do
    pos <- getPosition
    synErrorList %= (|> CustomError msg pos)


genNewEmptyError :: Graciela ()
genNewEmptyError = do
    pos <- getPosition
    synErrorList %= (|> EmptyError pos)
