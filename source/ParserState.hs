module ParserState where

import qualified Text.Parsec.Pos as P
import qualified Data.Text       as T 
import Control.Monad.State       as ST
import Contents                  as CO
import MyParseError
import MyTypeError
import ParserError
import Text.Parsec
import SymbolTable
import TokenParser
import Data.Monoid
import Location
import Token
import State
import Type
import AST

addFunTypeParser :: T.Text -> Maybe [Type] -> Maybe Type -> Location -> MyParser()
addFunTypeParser id (Just lt) (Just t) loc = addSymbolParser id (FunctionCon loc (MyFunction lt t))
addFunTypeParser _ _ _ _ = return () 

addProcTypeParser :: T.Text -> Maybe [(T.Text, Type)]  -> Location -> SymbolTable -> MyParser()
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

addManyUniSymParser :: Maybe([(T.Text, Location)]) -> Maybe(Type) -> MyParser()
addManyUniSymParser (Just xs) (Just t) = f xs t
      where
        f ((id, loc):xs) t = do addSymbolParser id $ Contents Variable loc t Nothing
                                f xs t
        f [] _             = return()
addManyUniSymParser _ _                = return() 
                                                  
addManySymParser :: VarBehavour -> Maybe([(T.Text , Location)]) -> Maybe(Type) -> Maybe([AST(Type)]) -> MyParser()
addManySymParser vb (Just xs) (Just t) (Just ys) =
    if length xs /= length ys then 
        do pos <- getPosition
           ST.modify $ addTypeError $ (IncomDefError vb (getLocation pos))
    else f vb xs t ys
      where
        f vb ((id, loc):xs) t (ast:ys) = 
            do addSymbolParser id $ Contents vb loc t (astToValue ast)
               f vb xs t ys
        f _ [] _ []                    = return()
addManySymParser _ _ _ _               = return()

astToValue (Int _ n _)    = Just $ I n
astToValue (Float _ f _)  = Just $ D f
astToValue (Bool _ b _)   = Just $ B b
astToValue (Char _ c _)   = Just $ C c
astToValue (String _ s _) = Just $ S s
astToValue _              = Nothing

addFunctionArgParser :: T.Text -> T.Text -> Maybe (Type) -> Location -> MyParser ()
addFunctionArgParser idf id (Just t) loc = 
    if id /= idf then
      addSymbolParser id $ FunctionCon loc t
    else
      addFunctionNameError id loc
addFunctionArgParser _ _ _ _             = return ()

addArgProcParser :: T.Text -> T.Text -> Maybe (Type) -> Location -> Maybe (TypeArg) -> MyParser ()
addArgProcParser id pid (Just t) loc (Just targ) = 
    if id /= pid then
      addSymbolParser id $ ArgProcCont targ loc t
    else
      addFunctionNameError id loc
addArgProcParser _ _ _ _ _                     = return()

addSymbolParser :: T.Text -> (Contents SymbolTable) -> MyParser ()
addSymbolParser id c = do ST.modify $ addNewSymbol id c
                          return()

addCuantVar :: T.Text -> Maybe Type -> Location -> MyParser()
addCuantVar id (Just t) loc = 
    if isCuantificable t then
       addSymbolParser id $ Contents CO.Constant loc t Nothing
    else
       addUncountableError loc
addCuantVar _ _ _           = return()

lookUpSymbol :: T.Text -> MyParser (Maybe (Contents SymbolTable))
lookUpSymbol id = 
    do st <- get
       case lookUpVarState id (symbolTable st) of
         Nothing -> do addNonDeclVarError id
                       return Nothing
         Just c  -> return $ Just c
       

lookUpVarParser :: T.Text -> MyParser (Maybe Type)
lookUpVarParser id = do st <- get
                        c  <- lookUpSymbol id
                        return $ fmap (symbolType) c

lookUpConsParser :: T.Text -> MyParser (Maybe Type)
lookUpConsParser id = 
    do c   <- lookUpSymbol id
       case c of
       { Nothing   -> return Nothing
       ; Just a    -> case a of
                      { (Contents    CO.Constant _ _ _) ->
                         do addConsIdError id
                            return $ Nothing
                      ; (ArgProcCont In    _ _     )  -> 
                         do addConsIdError id
                            return $ Nothing
                      ; otherwise                     -> 
                            return $ Just (symbolType a)
                      } 
       }

lookUpConstIntParser :: T.Text -> Location -> MyParser (Maybe Integer)
lookUpConstIntParser id loc =
    do c <- lookUpSymbol id
       case c of
         Nothing -> return Nothing
         Just a  -> case a of
                      Contents CO.Constant _ _ v ->
                        case v of 
                          Just (I n) -> return $ Just n
                          otherwise  ->
                            do addNotIntError id loc
                               return Nothing
                      otherwise ->
                        do addNotConsIdError id loc
                           return $ Nothing
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
genNewError laset msg = do  pos <- cleanEntry laset
                            ST.modify $ addParsingError $ newParseError msg pos
                            return ()

genNewEmptyError :: MyParser ()
genNewEmptyError = do  pos <- getPosition
                       ST.modify $ addParsingError $ newEmptyError pos
                       return ()

addOutOfBoundsError :: T.Text -> Location -> MyParser ()
addOutOfBoundsError t l = do ST.modify $ addTypeError $ IntOutOfBounds t l
                             return ()

addUncountableError :: Location -> MyParser ()
addUncountableError loc = do ST.modify $ addTypeError $ UncountError loc
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
