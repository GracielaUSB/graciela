module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST
import           Location            (Location)
import           MyParseError        as PE
import           Parser.Assertions
import           Parser.Declarations
import           Parser.Instructions
import           Parser.ParserType
import           Parser.Procedures   (listArgProc, listDefProc, panicMode,
                                      panicModeId)
import           Parser.TokenParser
import           ParserState
import           State
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Monad       (void)
import           Text.Parsec
import           Data.Text (Text)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: MyParser (Maybe (AST Type))
abstractDataType = do
    verify TokAbstract
    abstractId <- panicModeId parseLeftParent
    abstractTypes
    panicMode parseBegin (parseVar <|> parseTokLeftInv) PE.Begin
    abstractBody (topDecl <|> parseEnd)

    try $do void parseEnd
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd
             <|> do parseEnd
                    genNewError (return . fst . head $ t) PE.LexEnd
     <|> do void  parseEOF

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
    try $do void parseLeftParent
     <|> do id <- lookAhead parseId                    -- abstract Dicc  t1,t2)
            genNewError (verify $ TokId id) PE.TokenLP --               ^
     <|> do (token,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseId)   -- abstract Dicc [t1,t2)
            genNewError (return token) PE.TokenLP      --               ^

    sepBy (parseId) (parseComma)

    try $do void parseRightParent
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
    abstractDecList
    invariant follow
    procs <- manyTill (procDecl follow) $ lookAhead follow
    exitScopeParser
    return Nothing


abstractDecList :: MyParser ()
abstractDecList = void $ abstractDec `endBy` parseSemicolon


abstractDec :: MyParser ()
abstractDec = do
    parseVar <|> parseConst
    ids <- locId `sepBy` parseComma
    parseColon
    t <- abstType
    addManyUniSymParser (Just ids) t


locId :: MyParser (Text, Location)
locId = do
    loc <- parseLocation
    id <- parseId
    return (id, loc)


abstType :: MyParser Type
abstType =  do {parseSet; parseOf; basic >>= return . GSet }
        <|> do {parseMultiset; parseOf; basic >>= return . GMultiset }
        <|> do {parseSeq; parseOf; basic >>= return . GSeq }
        <|> do {parseFunc; ba <- basic; parseArrow; bb <- basic; return $ GFunc ba bb}
        <|> do {parseRel; ba <- basic; parseArrow; bb <- basic; return $ GRel ba bb}
        <|> (between
                parseLeftParent
                parseRightParent
                (basic `sepBy` parseComma)
                >>= return . GTuple)
        <|> basic


basic :: MyParser Type
basic = (parseId >>= return . GTypeVar) <|> parseType


-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: MyParser Token -> MyParser (Maybe (AST Type))
procDecl follow = do
    pos <- getPosition
    try $do void parseProc
     <|> do try $do t <- parseId
                    lookAhead parseId
                    genNewError (return $TokId t) PE.ProcOrFunc
     <|> do t <- lookAhead parseId
            genNewError (return $TokId t) PE.ProcOrFunc
     <|> do (t:_) <- manyTill anyToken (lookAhead $ parseId)
            genNewError (return $fst t) PE.Begin


    id <- panicModeId parseColon                                            -- Id
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


-- dataType -> 'type' Id 'implements' Id Types 'begin' DataTypeBody 'end'
dataType :: MyParser (Maybe (AST Type))
dataType = do
    verify TokDataType
    panicModeId (verify TokImplements)
    panicMode   (verify TokImplements) parseTokId PE.Implements
    abstratcId <- panicModeId parseLeftParent
    types
    panicMode parseBegin (parseVar <|> verify TokLeftRep) PE.Begin
    dataTypeBody parseEnd parseEnd

    try $do void parseEnd
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd
             <|> do parseEnd
                    genNewError (return $ fst $ head t) PE.LexEnd
     <|> do void parseEOF

    return Nothing


-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
types :: MyParser (Maybe (AST Type))
types = do

    -- panicMode parseLeftParent parseType PE.TokenLP
    try $do void parseLeftParent
     <|> do t <- lookAhead $ parseType                  -- type Dicc implements D   t1,t2)
            genNewError (return $TokType t) PE.TokenLP  --                        ^
     <|> do (token,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseType)  -- type Dicc implements D [[t1,t2)
            genNewError (return token) PE.TokenLP       --                        ^^

    sepBy (myType parseSemicolon parseSemicolon) (parseComma)

    panicMode parseRightParent parseBegin PE.TokenRP

    return Nothing
     where parseType = myType parseSemicolon parseSemicolon


-- DataTypeBody -> DecList RepInvariant AcInvariant ListDefProc
dataTypeBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
dataTypeBody follow recSet = do
    newScopeParser
    dl    <- decList followAction recSet
    repInvariant -- No hace nada
    acInvariant  -- No hace nada
    procs <- listDefProc follow recSet
    exitScopeParser
    return Nothing
