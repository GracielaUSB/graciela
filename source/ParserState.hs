module ParserState where

import qualified Text.Parsec.Pos as P
import Control.Monad.State       as ST
import qualified Data.Text       as T
import Contents                  as CO
import MyParseError
import MyTypeError
import ParserError
import Text.Parsec
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

addProcTypeParser :: T.Text -> Maybe [Type]  -> Location -> MyParser()
addProcTypeParser id (Just lt) loc = addSymbolParser id (FunctionCon loc (MyProcedure lt))
addProcTypeParser _ _ _ = return () 

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
        f ((id, loc):xs) t = do addSymbolParser id (Contents Variable loc t Nothing)
                                f xs t
        f [] _             = return()
addManyUniSymParser _ _                = return()

addManySymParser :: VarBehavour -> Maybe([(T.Text, Location)]) -> Maybe(Type) -> Maybe([AST(Type)]) -> MyParser()
addManySymParser vb (Just xs) (Just t) (Just ys) = if   length xs /= length ys then do pos <- getPosition
                                                                                       ST.modify $ addTypeError $ (IncomDefError (getLocation pos))
                                                   else f vb xs t ys
      where
        f vb ((id, loc):xs) t (ast:ys) = do addSymbolParser id (Contents vb loc t (Just ast))
                                            f vb xs t ys
        f _ [] _ []                    = return()
addManySymParser _ _ _ _               = return()

addFunctionArgParser :: T.Text -> Maybe (Type) -> Location -> MyParser ()
addFunctionArgParser id (Just t) loc = addSymbolParser id (Contents CO.Constant loc t Nothing)
addFunctionArgParser _ _ _           = return ()

addSymbolParser :: T.Text -> Contents -> MyParser ()
addSymbolParser id c = do ST.modify $ addNewSymbol id c
                          return()

lookUpSymbol :: T.Text -> MyParser (Maybe Contents)
lookUpSymbol id = do st <- get
                     return $ lookUpVarState id (symbolTable st)

lookUpVarParser :: T.Text -> MyParser (Maybe Type)
lookUpVarParser id = do st <- get
                        c  <- lookUpSymbol id
                        return $ fmap (symbolType) c

lookUpConsParser :: T.Text -> MyParser (Maybe Type)
lookUpConsParser id = do c   <- lookUpSymbol id
                         case c of
                         { Nothing   -> return Nothing
                         ; Just a    -> case a of
                                        { (Contents    CO.Constant _ _ _) -> do addConsIdError id
                                                                                return $ Nothing
                                        ; (ArgProcCont In    _ _     )    -> do addConsIdError id
                                                                                return $ Nothing
                                        ; otherwise                       -> return $ Just (symbolType a)
                                        } 
                        }

addConsIdError id = do pos <- getPosition 
                       ST.modify $ addTypeError (ConstIdError id (getLocation pos))
                       return ()

genNewError :: MyParser (Token) -> WaitedToken -> MyParser ()
genNewError laset msg = do  pos <- cleanEntry laset
                            ST.modify $ addParsingError $ newParseError msg pos
                            return ()

addArgProcParser :: T.Text -> Maybe (Type) -> Location -> Maybe (TypeArg) -> MyParser ()
addArgProcParser id (Just t) loc (Just targ) = addSymbolParser id (ArgProcCont targ loc t)
addArgProcParser _ _ _ _                     = return()

genNewEmptyError :: MyParser ()
genNewEmptyError = do  pos <- getPosition
                       ST.modify $ addParsingError $ newEmptyError pos
                       return ()
