module Parser.ADT
    ( abstractDataType
    , dataType 
    ) where

-------------------------------------------------------------------------------
import Parser.Assertions             
import Parser.Expression
import Parser.Declarations
import Parser.Procedures             (listDefProc, listArgProc)
import Parser.Instructions           
import Parser.TokenParser
import MyParseError                  as PE
import ParserState
import ParserType
import Contents
import Location
import Token
import State
import Type
import AST
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP
import qualified Control.Monad       as M
import qualified Data.Text           as T
import           Text.Parsec         
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' ID AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: MyParser (Maybe (AST Type))
abstractDataType = do
    verify TokAbstract
    try $do M.void parseID
     <|> do token <- lookAhead $ parseLeftParent              -- abstract   (t1,t2) begin
            genNewError (return token) PE.IDError             --          ^
     <|> do (token,_) <- anyToken
            manyTill anyToken (lookAhead $ parseLeftParent)   --  abstract +-) (t1,t2) begin
            genNewError (return token) PE.IDError             --           ^^^  
    
    abstractTypes

    try $do M.void $ parseBegin
     <|> do t <- (lookAhead $ parseVar <|> parseTokLeftInv)   -- abstract Dicc (t1,t2) 
            genNewError (return t) PE.Begin                   --                       ^
     <|> do (t:_) <- manyTill anyToken (lookAhead $
                               parseVar <|> parseTokLeftInv)  -- abstract Dicc (t1,t2) [][]
            genNewError (return $fst t) PE.Begin              --                       ^^^^

    abstractBody parseEnd parseEnd

    try $do parseEnd 
            return Nothing
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd                     
             <|> do parseEnd
                    genNewError (return $ fst $ head t) PE.LexEnd
            return Nothing
     <|> do parseEOF
            return Nothing

topDecl :: MyParser Token   
topDecl = choice  [ verify TokAbstract 
                  , verify TokDataType
                  , parseProgram
                  ]      

-- AbstractType -> '(' ListTypes ')'
-- ListTypes: lista de ids contruidas con parsec

-- Podria hacerce con between, pero no se como dar errores "bonitos" 
abstractTypes :: MyParser (Maybe (AST Type))
abstractTypes = do 
    try $do M.void parseLeftParent
     <|> do id <- lookAhead parseID                    -- abstract Dicc  t1,t2)
            genNewError (verify $ TokId id) PE.TokenLP --               ^
     <|> do (token,_) <- anyToken                             
            manyTill  anyToken (lookAhead $ parseID)   -- abstract Dicc [t1,t2)
            genNewError (return token) PE.TokenLP      --               ^ 

    sepBy (parseID) (parseComma)

    try $do parseRightParent
            return Nothing
     <|> do lookAhead $ parseBegin                    -- abstract Dicc (t1,t2
            genNewError (return TokBegin) PE.TokenRP  --                     ^
            return Nothing
     <|> do (token,_) <- anyToken                     -- abstract Dicc (t1,t2]
            manyTill  anyToken (lookAhead parseBegin) --                     ^
            genNewError (return token) PE.TokenRP 
            return Nothing
         
     

-- AbstractBody -> DecList Invariant ListProcDecl
abstractBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
abstractBody follow recSet = do 
    newScopeParser
    dl    <- decList followAction (recSet)
    invariant follow recSet
    procs <- manyTill (procDecl follow parseEnd) $lookAhead (follow)
    exitScopeParser
    return Nothing
    where 
        follow = (topDecl <|> parseEnd)

    
    
-- ProcDecl -> 'proc' ID ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
procDecl follow recSet = do
    pos <- getPosition                                   
    try $do M.void parseProc
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return $t) PE.ProcOrFunc

    id <- try $do parseID
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return t) PE.IDError
            return $ T.pack "No ID"

    try $do M.void $ parseColon
     <|> do t <- lookAhead parseLeftParent                                  -- proc id   (in a :int)
            genNewError (return t) PE.Colon                                 --         ^
     <|> do (t,_) <- anyToken                                               -- proc id [ (in a : int)
            genNewError (return t) PE.Colon                                 --         ^

    try $do M.void parseLeftParent
     <|> do t <- lookAhead argTypes                                         
            genNewError (return t) PE.TokenLP                               -- proc id : in a :int)
     <|> do (t,_) <- anyToken                                               --          ^  
            manyTill  anyToken (lookAhead $ argTypes <|> parseRightParent)    
            genNewError (return t) PE.TokenLP                               -- proc id : [[$ in a :int)
                                                                            --           ^^^
    newScopeParser
    targs <- listArgProc id parseRightParent parseRightParent

    try $do parseRightParent
            return Nothing
     <|> do t <- lookAhead $ parseTokLeftPre 
            genNewError (return t) PE.TokenRP 
            return Nothing
     <|> do (t:_) <- manyTill  anyToken (lookAhead parseTokLeftPre)
            genNewError (return $fst t) PE.TokenRP 
            return Nothing

    pre  <- precondition parseTokOpenBlock (recSet <|> parseTokOpenBlock)
    post <- postcondition follow (recSet <|> follow)
    exitScopeParser
    return Nothing
    where 
        argTypes :: MyParser Token   
        argTypes = choice  [ parseIn
                          , parseOut
                          , parseInOut
                          , parseInOut
                          ] 

-- dataType -> 'type' ID 'implements' ID Types 'begin' DataTypeBody 'end'
dataType :: MyParser (Maybe (AST Type))
dataType = do 
    verify TokDataType

    try $do M.void parseID
     <|> do token <- lookAhead $ verify TokImplements          
            genNewError (return token) PE.IDError             
     <|> do (t:_) <- manyTill anyToken (lookAhead $ verify TokImplements)   
            genNewError (return $fst t) PE.IDError                  
    
    try $do M.void $ verify TokImplements 
     <|> do id <- lookAhead parseID
            genNewError (verify $ TokId id) PE.Implements
     <|> do (t:_) <- manyTill anyToken (lookAhead $ parseID)
            genNewError (return $fst t) PE.Implements
    
    try $do M.void parseID
     <|> do token <- lookAhead $ parseLeftParent
            genNewError (return token) PE.IDError
     <|> do (t:_) <- manyTill anyToken (lookAhead $ parseLeftParent)
            genNewError (return $fst t) PE.IDError

    types

    try $do M.void $ parseBegin
     <|> do t <- lookAhead $ parseVar <|> verify TokLeftRep
            genNewError (return t) PE.Begin
     <|> do (t:_) <- manyTill anyToken (lookAhead $
                             parseVar <|> verify TokLeftRep)
            genNewError (return $fst t) PE.Begin

    dataTypeBody parseEnd parseEnd

    try $do parseEnd 
            return Nothing
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd                     
             <|> do parseEnd
                    genNewError (return $ fst $ head t) PE.LexEnd
            return Nothing
     <|> do parseEOF
            return Nothing

-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
types :: MyParser (Maybe (AST Type))
types = do 
    try $do M.void parseLeftParent
     <|> do t <- lookAhead $ parseType                  -- type Dicc implements D   t1,t2)
            genNewError (return $TokType t) PE.TokenLP  --                        ^
     <|> do (token,_) <- anyToken                             
            manyTill  anyToken (lookAhead $ parseType)  -- type Dicc implements D [[t1,t2)
            genNewError (return token) PE.TokenLP       --                        ^^ 

    sepBy (myType parseSemicolon parseSemicolon) (parseComma)

    try $do parseRightParent
            return Nothing
     <|> do lookAhead $ parseBegin                      -- type Dicc implements D (t1,t2 
            genNewError (return TokBegin) PE.TokenRP    --                              ^
            return Nothing
     <|> do (token,_) <- anyToken                       -- type Dicc implements D (t1,t2]]
            manyTill  anyToken (lookAhead parseBegin)   --                              ^^
            genNewError (return token) PE.TokenRP 
            return Nothing
     where parseType = myType parseSemicolon parseSemicolon  

-- DataTypeBody -> DeclList RepInvariant AcInvariant ListDefProc
dataTypeBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
dataTypeBody follow recSet = do 
    newScopeParser
    dl    <- decList followAction recSet
    repInvariant -- No hace nada
    acInvariant  -- No hace nada
    procs <- listDefProc follow recSet
    exitScopeParser
    return Nothing




