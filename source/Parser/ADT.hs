module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST

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
import           Text.Megaparsec     (Dec, between, getPosition, 
                                      sepBy, try, (<|>), ParseError)
import           Data.Text           (Text)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela (Maybe (AST Type))
abstractDataType = do
    pos <- getPosition
    match TokAbstract
    abstractId <- panicModeId  -- parseLeftParent
    abstractTypes
    insertType abstractId (GAbstractType abstractId [] [] []) (toLocation pos)
    match TokBegin  -- (parseVar <|> parseTokLeftInv) PE.Begin

    newScopeParser
    addSymbolParser abstractId (AbstractContent abstractId (toLocation pos))
    abstractBody (topDecl <|> parseEnd)
    exitScopeParser
    match TokEnd

    -- try $do void match TokEnd
    --  <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
    --         try $do token <- lookAhead topDecl
    --                 genNewError (return token) PE.LexEnd
    --          <|> do parseEnd
    --                 genNewError (return . fst . head $ t) PE.LexEnd
    --  <|> do void  parseEOF

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
    -- try $do void parseLeftParent
     -- <|> do id <- lookAhead parseId                    -- abstract Dicc  t1,t2)
     --        genNewError (verify $ TokId id) PE.TokenLP --               ^
     -- <|> do (token,_) <- anyToken
     --        manyTill  anyToken (lookAhead $ parseId)   -- abstract Dicc [t1,t2)
     --        genNewError (return token) PE.TokenLP      --               ^

    between (match TokLeftParent) (match RightParent) 
            (identifier sepBy match TokComma)
    -- try $do void parseRightParent
    --  <|> do lookAhead $ parseBegin                    -- abstract Dicc (t1,t2
    --         genNewError (return TokBegin) PE.TokenRP  --                     ^
    --  <|> do (token,_) <- anyToken                     -- abstract Dicc (t1,t2]
    --         manyTill  anyToken (lookAhead parseBegin) --                     ^
    --         genNewError (return token) PE.TokenRP
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
    match TokVar <|> match TokConst
    ids <- identifier `sepBy` match TokComma
    parseColon
    t <- abstType
    addManyUniSymParser (Just ids) t





abstType :: Graciela Type
abstType =  do {match TokSet; match TokOf; basic >>= return . GSet }
        <|> do {match TokMultiset; match TokOf; basic >>= return . GMultiset }
        <|> do {match TokSeq; match TokOf; basic >>= return . GSeq }
        <|> do {match TokFunc; ba <- basic; match TokArrow; bb <- basic; return $ GFunc ba bb}
        <|> do {match TokRel; ba <- basic; parseArrow; bb <- basic; return $ GRel ba bb}
        <|> (between (match TokLeftParent) 
                     (match RightParent)
                     (basic `sepBy` parseComma)
                >>=  return . GTuple)
        <|> basic


basic :: Graciela Type
basic = (identifier >>= return . GTypeVar)


-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: Graciela Token -> Graciela (Maybe (AST Type))
procDecl follow = do
    -- try $do void parseProc
    --  <|> do try $do t <- parseId
    --                 lookAhead parseId
    --                 genNewError (return $TokId t) PE.ProcOrFunc
    --  <|> do t <- lookAhead parseId
    --         genNewError (return $TokId t) PE.ProcOrFunc
    --  <|> do (t:_) <- manyTill anyToken (lookAhead $ parseId)
    --         genNewError (return $fst t) PE.Begin

    match TokProc
    id    <- identifier -- parseLeftParent                                  -- Id
    match TokLeftParent                                                     -- (
    newScopeParser                                                          
    targs <- listArgProc id (match RightParent) (match RightParent)         -- arguments
    match RightParent                                                       -- ) 
    pre   <- precondition parseTokLeftPost                                  -- pre
    post  <- postcondition follow                                           -- post
    sb    <- getCurrentScope
    addProcTypeParser id targs (toLocation pos) sb
    exitScopeParser
    addProcTypeParser id targs (toLocation pos) sb

    -- panicMode parseLeftParent (argTypes <|> parseRightParent) PE.TokenLP
    -- newScopeParser
    -- targs <- listArgProc id parseRightParent parseRightParent
    -- panicMode parseRightParent parseTokLeftPre PE.TokenRP
    -- pre  <- precondition parseTokLeftPost
    -- post <- postcondition follow
    -- sb <- getCurrentScope
    

    return Nothing
    where
        argTypes :: Graciela Token
        argTypes = choice [ match TokIn
                          , match TokOut
                          , match TokInOut
                          , match TokRef
                          ]


-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Graciela (Maybe (AST Type))
dataType = do
    pos <- getPosition
    match TokType
    typeId <- identifier -- (match TokImplements)
    match TokImplements
    match TokLeftParent
    t  <- types 
    insertType typeId (GDataType typeId [] [] []) (toLocation pos)
    newScopeParser
    addSymbolParser typeId (TypeContent typeId (toLocation pos))
    between (match TokBegin) (match TokEnd) typeBody 
    exitScopeParser
    return Nothing
    -- panicMode (verify TokImplements) parseTokId PE.Implements
    -- abstractId <- panicModeId parseLeftParent
    -- types
    -- insertType typeId (GDataType typeId [] [] []) (toLocation pos)
    -- panicMode parseBegin (parseVar <|> verify TokLeftRep) PE.Begin
    -- try $do void parseEnd
    --  <|> do t <- manyTill anyToken $lookAhead (topDecl <|> parseEnd)
    --         try $do token <- lookAhead topDecl
    --                 genNewError (return token) PE.LexEnd
    --          <|> do parseEnd
    --                 genNewError (return $ fst $ head t) PE.LexEnd
    --  <|> do void parseEOF
    where 
        typeBody = do
            dl    <- decList (match TokEnd) (match TokEnd)
            repInvariant
            coupInvariant
            procs <- listDefProc (match TokEnd) (match TokEnd)
        types = do
            match TokLeftParent         
            t  <- identifier `sepBy` match TokComma
            match TokRightParent
            return t

            

-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec





