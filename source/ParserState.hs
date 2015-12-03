module ParserState where

import qualified Data.Text  as T 
import Control.Monad.State  as ST
import Contents              as CO
import TokenParser
import MyParseError
import MyTypeError
import ParserError
import Text.Parsec
import SymbolTable
import Data.Maybe
import Location
import Token
import State
import Type
import AST


addFileToReadParser :: String -> MyParser()
addFileToReadParser file = modify $ addFileToRead file


addFunTypeParser :: T.Text -> Maybe [(T.Text, Type)] -> Type -> Location ->  SymbolTable -> MyParser()
addFunTypeParser id (Just lt) t loc sb = addSymbolParser id (FunctionCon loc (MyFunction (map snd lt) t) (map fst lt) sb)
addFunTypeParser _ _ _ _ _ = return () 


addProcTypeParser :: T.Text -> Maybe [(T.Text, Type)] -> Location -> SymbolTable -> MyParser()
addProcTypeParser id (Just xs) loc sb = addSymbolParser id $ ProcCon loc (MyProcedure (map snd xs)) (map fst xs) sb
addProcTypeParser _ _ _ _             = return () 


getActualScope :: MyParser (SymbolTable)
getActualScope = do s <- ST.get
                    return $ symbolTable s


newScopeParser :: MyParser ()
newScopeParser = ST.modify $ newScopeState 


getScopeParser :: MyParser (Int)
getScopeParser = do st <- ST.get
                    return $ getScopeState st


exitScopeParser :: MyParser ()
exitScopeParser = ST.modify $ exitScopeState


addManyUniSymParser :: Maybe([(T.Text, Location)]) -> Type -> MyParser()
addManyUniSymParser (Just xs) t = f xs t
      where
        f ((id, loc):xs) t = do addSymbolParser id $ Contents Variable loc t Nothing False
                                f xs t
        f [] _             = return()

addManyUniSymParser _ _                = return() 
             

addManySymParser :: VarBehavour -> Maybe([(T.Text , Location)]) -> Type -> Maybe([AST(Type)]) -> MyParser()
addManySymParser vb (Just xs) t (Just ys) =
    if length xs /= length ys then 
        do pos <- getPosition
           ST.modify $ addTypeError $ IncomDefError vb (getLocation pos)
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


verifyReadVars :: Maybe [(T.Text, Location)] -> MyParser ([Type])
verifyReadVars (Just lid) = fmap catMaybes $ mapM (lookUpConsParser . fst) lid
verifyReadVars _          = return []


addFunctionArgParser :: T.Text -> T.Text -> Type -> Location -> MyParser ()
addFunctionArgParser idf id t loc = 
    if id /= idf then
        addSymbolParser id $ Contents CO.Constant loc t Nothing True
    else
        addFunctionNameError id loc


addArgProcParser :: T.Text -> T.Text -> Type -> Location -> Maybe (TypeArg) -> MyParser ()
addArgProcParser id pid t loc (Just targ) = 
    if id /= pid then
      addSymbolParser id $ ArgProcCont targ loc t
    else
      addFunctionNameError id loc

addArgProcParser _ _ _ _ _                     = return()


addSymbolParser :: T.Text -> (Contents SymbolTable) -> MyParser ()
addSymbolParser id c = do ST.modify $ addNewSymbol id c
                          return()


addCuantVar :: OpQuant -> T.Text -> Type -> Location -> MyParser()
addCuantVar op id t loc = 
    if isCuantificable t then
       addSymbolParser id $ Contents CO.Constant loc t Nothing True
    else
       addUncountableError op loc


lookUpSymbol :: T.Text -> MyParser (Maybe (Contents SymbolTable))
lookUpSymbol id = 
    do st <- get
       case lookUpVarState id (symbolTable st) of
       { Nothing -> do addNonDeclVarError id
                       return Nothing
       ; Just c  -> return $ Just c
       }


lookUpVarParser :: T.Text -> Location -> MyParser (Maybe Type)
lookUpVarParser id loc = 
    do  st <- get
        c  <- lookUpSymbol id
        case c of
        { Just c' -> return $ fmap (symbolType) c
        ; Nothing -> return Nothing
        }


lookUpConsParser :: T.Text -> MyParser (Maybe Type)
lookUpConsParser id = 
    do c   <- lookUpSymbol id
       case c of
       { Nothing   -> return Nothing
       ; Just a    ->
          if isLValue a then 
            do newInitVar id
               return $ Just $ symbolType a
          else
            do addConsIdError id
               return $ Nothing
       }


newInitVar :: T.Text -> MyParser()
newInitVar id =
    do ST.modify $ initVar id
       return ()


lookUpConstIntParser :: T.Text -> Location -> MyParser (Maybe Type)
lookUpConstIntParser id loc =
    do c <- lookUpSymbol id
       loc <- getPosition
       case c of
       { Nothing -> return Nothing
       ; Just a  -> 
          if isInitialized a then
            if isRValue a then
              if symbolType a == MyInt then
                return $ return MyInt
              else
                do addNotIntError id (getLocation loc)
                   return $ Nothing
            else
              do addNotRValueError id (getLocation loc)
                 return Nothing
          else
            do addNotInitError id (getLocation loc)
               return Nothing
       }

       
addConsIdError id = 
    do pos <- getPosition 
       ST.modify $ addTypeError (ConstIdError id (getLocation pos))
       return ()


addNonDeclVarError id =
    do pos <- getPosition 
       ST.modify $ addTypeError $ NonDeclError id (getLocation pos)
       return ()


addNonAsocError :: MyParser ()
addNonAsocError = 
    do pos <- getPosition
       ST.modify $ addParsingError $ NonAsocError (getLocation pos) 
       return ()
 

addArrayCallError waDim prDim = do pos <- getPosition
                                   ST.modify $ addParsingError $ ArrayError waDim prDim (getLocation pos) 
                                   return ()


genNewError :: MyParser (Token) -> WaitedToken -> MyParser ()
genNewError laset msg =
    do lookAhead (parseEnd <|> laset)
       return ()
       <|> do  pos <- cleanEntry laset
               ST.modify $ addParsingError $ newParseError msg pos
               return ()


genNewEmptyError :: MyParser ()
genNewEmptyError = do  pos <- getPosition
                       ST.modify $ addParsingError $ newEmptyError pos
                       return ()

addOutOfBoundsError :: T.Text -> Location -> MyParser ()
addOutOfBoundsError t l = do ST.modify $ addTypeError $ IntOutOfBounds t l
                             return ()


addUncountableError :: OpQuant -> Location -> MyParser ()
addUncountableError op loc = do ST.modify $ addTypeError $ UncountError op loc
                                return ()


addFunctionNameError :: T.Text -> Location -> MyParser ()
addFunctionNameError id loc = do ST.modify $ addTypeError $ FunNameError id loc
                                 return ()


addNotIntError :: T.Text -> Location -> MyParser ()
addNotIntError id loc = do ST.modify $ addTypeError $ NotIntError id loc
                           return ()


addNotConsIdError :: T.Text -> Location -> MyParser ()
addNotConsIdError id loc = 
    do ST.modify $ addTypeError $ NotConstError id loc
       return ()


addNotInitError :: T.Text -> Location -> MyParser()
addNotInitError id loc =
    do ST.modify $ addTypeError $ NotInitError id loc
       return ()


addNotRValueError :: T.Text -> Location -> MyParser()
addNotRValueError id loc =
    do ST.modify $ addTypeError $ NotRValueError id loc
       return ()
