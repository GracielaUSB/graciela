module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST
import           Location            (Location)
import           MyParseError        as PE
import           Contents                      
import           Parser.Assertions
import           Parser.Declarations
import           Parser.Instructions
import           Parser.Type
import           Parser.Procedures   (listArgProc, listDefProc, panicMode,
                                      panicModeId)
import           Parser.TokenParser
import           ParserState
import           Graciela
import           Token
import           Type
import           Location
import           SymbolTable
-------------------------------------------------------------------------------
import           Control.Monad       (void)
import           Control.Lens        (use, (.=), (%=))
import           Text.Parsec
import           Data.Text (Text)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela (Maybe (AST Type))
abstractDataType = do
    pos <- getPosition
    verify TokAbstract
    abstractId <- panicModeId parseLeftParent
    abstractTypes
    insertType abstractId (GAbstractType abstractId [] [] []) (toLocation pos)
    panicMode parseBegin (parseVar <|> parseTokLeftInv) PE.Begin

    newScopeParser
    addSymbolParser abstractId (AbstractContent abstractId (toLocation pos))
    abstractBody (topDecl <|> parseEnd)
    exitScopeParser

    try $do void parseEnd
     <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
            try $do token <- lookAhead topDecl
                    genNewError (return token) PE.LexEnd
             <|> do parseEnd
                    genNewError (return . fst . head $ t) PE.LexEnd
     <|> do void  parseEOF

    return Nothing

topDecl :: Graciela Token
topDecl = choice  [ verify TokAbstract
                  , verify TokDataType
                  , parseProgram
                  ]

-- AbstractType -> '(' ListTypes ')'
-- ListTypes: lista de ids contruidas con parsec

-- Podria hacerce con between, pero no se como dar errores "bonitos"
abstractTypes :: Graciela (Maybe (AST Type))
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
abstractBody :: Graciela Token -> Graciela (Maybe (AST Type))
abstractBody follow = do
    abstractDecList
    invariant follow
    procs <- manyTill (procDecl follow) $ lookAhead follow
    return Nothing


abstractDecList :: Graciela ()
abstractDecList = void $ abstractDec `endBy` parseSemicolon


abstractDec :: Graciela ()
abstractDec = do
    parseVar <|> parseConst
    ids <- locId `sepBy` parseComma
    parseColon
    t <- abstType
    addManyUniSymParser (Just ids) t


locId :: Graciela (Text, Location)
locId = do
    loc <- parseLocation
    id <- parseId
    return (id, loc)


abstType :: Graciela Type
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


basic :: Graciela Type
basic = (parseId >>= return . GTypeVar) <|> parseType


-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: Graciela Token -> Graciela (Maybe (AST Type))
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
    sb <- getCurrentScope
    addProcTypeParser id targs (toLocation pos) sb
    exitScopeParser
    addProcTypeParser id targs (toLocation pos) sb

    return Nothing
    where
        argTypes :: Graciela Token
        argTypes = choice  [ parseIn
                          , parseOut
                          , parseInOut
                          , parseInOut
                          ]


-- dataType -> 'type' Id 'implements' Id Types 'begin' DataTypeBody 'end'
dataType :: Graciela (Maybe (AST Type))
dataType = do
    pos <- getPosition
    verify TokDataType
    typeId <- panicModeId (verify TokImplements)
    panicMode   (verify TokImplements) parseTokId PE.Implements
    abstractId <- panicModeId parseLeftParent
    types
    insertType typeId (GDataType typeId [] [] []) (toLocation pos)
    panicMode parseBegin (parseVar <|> verify TokLeftRep) PE.Begin
    
    newScopeParser
    addSymbolParser typeId (TypeContent typeId (toLocation pos))
    dataTypeBody parseEnd parseEnd
    exitScopeParser

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
types :: Graciela (Maybe (AST Type))
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


-- DataTypeBody -> DecList RepInvariant CoupInvariant ListDefProc
dataTypeBody :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
dataTypeBody follow recSet = do
    dl    <- decList followAction recSet
    repInvariant -- No hace nada
    coupInvariant  -- No hace nada
    procs <- listDefProc follow recSet
    return Nothing
