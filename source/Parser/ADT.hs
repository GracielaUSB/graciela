module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST
import           Contents
import           Graciela
import           MyParseError        as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Instruction
import           Parser.Procedure
import           Parser.Token
import           Parser.Type
import           Parser.State
import           Location
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (void)
import           Data.Text           (Text)
import           Data.Maybe          (catMaybes)
import           Text.Megaparsec     (Dec, ParseError, between, choice, many,
                                      getPosition, sepBy, try, endBy, (<|>),
                                      manyTill, lookAhead)
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela (Maybe AST)
abstractDataType = do
    pos <- getPosition
    match TokAbstract
    abstractId <- identifier
    atypes <- parens (identifier `sepBy` match TokComma)
    insertType abstractId (GAbstractType abstractId [] [] []) pos
    match TokBegin
    newScopeParser
    addSymbolParser abstractId (AbstractContent abstractId pos)
    abstractBody (topDecl <|> match TokEnd)
    exitScopeParser
    match TokEnd
    return Nothing
    where
        -- AbstractType -> '(' ListTypes ')'
        abstractTypes = return between (match TokLeftParent)
                               (match RightParent)
                               (identifier `sepBy` match TokComma)
topDecl :: Graciela Token
topDecl = choice [ match TokAbstract
                 , match TokType
                 , match TokProgram
                 ]

-- AbstractBody -> DecList Invariant ListProcDecl
abstractBody :: Graciela Token -> Graciela (Maybe AST)
abstractBody follow = do
    abstractDecList
    invariant follow
    procs <- manyTill (procDecl follow) $ lookAhead follow
    return Nothing


abstractDecList :: Graciela ()
abstractDecList = void $ abstractDec `endBy` match TokSemicolon


abstractDec :: Graciela ()
abstractDec = do
    match TokVar <|> match TokConst
    ids <- idAndPos `sepBy` match TokComma
    match TokColon
    t  <- abstType
    addManyUniSymParser ids t
    where
        idAndPos = do
                pos <- getPosition
                id  <- identifier
                return (id, pos)

abstType :: Graciela Type
abstType =  do {match TokSet;      match TokOf; GSet      <$> basic }
        <|> do {match TokMultiset; match TokOf; GMultiset <$> basic }
        <|> do {match TokSeq;      match TokOf; GSeq      <$> basic }
        <|> do {match TokFunc; ba <- basic; match TokArrow; bb <- basic; return $ GFunc ba bb}
        <|> do {match TokRel;  ba <- basic; match TokBiArrow; bb <- basic; return $ GRel ba bb}
        <|> GTuple <$> parens (basic `sepBy` match TokComma)
        <|> basic

basic :: Graciela Type
basic = GTypeVar <$> identifier


-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: Graciela Token -> Graciela (Maybe AST)
procDecl follow = do
    pos <- getPosition
    match TokProc
    id    <- identifier
    match TokLeftPar
    newScopeParser
    params' <- parens . many $ procParam id (match TokBegin)
    let params = catMaybes params'
    match TokRightPar
    pre   <- precondition (match TokLeftPost)
    post  <- postcondition follow
    sb    <- getCurrentScope
    addProcTypeParser id params pos sb
    exitScopeParser
    addProcTypeParser id params pos sb

    return Nothing


-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Graciela (Maybe AST)
dataType = do
    pos <- getPosition
    match TokType
    typeId <- identifier -- (match TokImplements)
    match TokImplements
    match TokLeftPar
    t  <- types
    insertType typeId (GDataType typeId [] [] []) pos
    newScopeParser
    addSymbolParser typeId (TypeContent typeId pos)
    (decls, procs) <- between (match TokBegin) (match TokEnd) typeBody
    exitScopeParser
    return Nothing

    where
      typeBody = do
        decls <- decList (match TokEnd)
        repInvariant
        coupInvariant
        procs <- many procedure
        return (decls, procs)
      types = do
        match TokLeftPar
        t  <- identifier `sepBy` match TokComma
        match TokRightPar
        return t



-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
