module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import Parser.Assertions
import Parser.Expression
import Parser.Declarations
import Parser.Procedures             (listDefProc, listArgProc, panicMode, panicModeID)
import Parser.Instructions
import Parser.TokenParser
import Parser.ParserType
import MyParseError                  as PE
import ParserState
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
    abstractID <- panicModeID parseLeftParent
    abstractTypes
    panicMode parseBegin (parseVar <|> parseTokLeftInv) PE.Begin
    abstractBody (topDecl <|> parseEnd)

    try $do M.void parseEnd
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd
             <|> do parseEnd
                    genNewError (return $ fst $ head t) PE.LexEnd
     <|> do M.void  parseEOF

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

    try $do M.void parseRightParent
     <|> do lookAhead $ parseBegin                    -- abstract Dicc (t1,t2
            genNewError (return TokBegin) PE.TokenRP  --                     ^
     <|> do (token,_) <- anyToken                     -- abstract Dicc (t1,t2]
            manyTill  anyToken (lookAhead parseBegin) --                     ^
            genNewError (return token) PE.TokenRP

    return Nothing


-- AbstractBody -> DecList Invariant ListProcDecl
abstractBody :: MyParser Token -> MyParser (Maybe (AST Type))
abstractBody follow = do
    newScopeParser
    dl    <- decList followAction follow
    invariant follow
    procs <- manyTill (procDecl follow) $lookAhead (follow)
    exitScopeParser
    return Nothing



-- ProcDecl -> 'proc' ID ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: MyParser Token -> MyParser (Maybe (AST Type))
procDecl follow = do
    pos <- getPosition
    try $do M.void parseProc
     <|> do try $do t <- parseID
                    lookAhead parseID
                    genNewError (return $TokId t) PE.ProcOrFunc
     <|> do t <- lookAhead parseID
            genNewError (return $TokId t) PE.ProcOrFunc
     <|> do (t:_) <- manyTill anyToken (lookAhead $ parseID)
            genNewError (return $fst t) PE.Begin


    id <- panicModeID parseColon                                            -- ID
    -- panicMode parseColon parseLeftParent PE.Colon                           -- :
    panicMode parseLeftParent (argTypes <|> parseRightParent) PE.TokenLP    -- (
    newScopeParser
    targs <- listArgProc id parseRightParent parseRightParent               -- arguments
    panicMode parseRightParent parseTokLeftPre PE.TokenRP                   -- )
    pre  <- precondition parseTokLeftPost                                   -- pre
    post <- postcondition follow                                            -- post
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
    panicModeID (verify TokImplements)
    panicMode   (verify TokImplements) parseTokID PE.Implements
    abstratcID <- panicModeID parseLeftParent
    types
    panicMode parseBegin (parseVar <|> verify TokLeftRep) PE.Begin
    dataTypeBody parseEnd parseEnd

    try $do M.void parseEnd
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd
             <|> do parseEnd
                    genNewError (return $ fst $ head t) PE.LexEnd
     <|> do M.void parseEOF

    return Nothing

-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
types :: MyParser (Maybe (AST Type))
types = do

    -- panicMode parseLeftParent parseType PE.TokenLP
    try $do M.void parseLeftParent
     <|> do t <- lookAhead $ parseType                  -- type Dicc implements D   t1,t2)
            genNewError (return $TokType t) PE.TokenLP  --                        ^
     <|> do (token,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseType)  -- type Dicc implements D [[t1,t2)
            genNewError (return token) PE.TokenLP       --                        ^^

    sepBy (myType parseSemicolon parseSemicolon) (parseComma)

    panicMode parseRightParent parseBegin PE.TokenRP

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






