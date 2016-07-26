module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST
import           Contents
import           Graciela
import           MyParseError        as PE
import           Parser.Assertions
import           Parser.Declarations
import           Parser.Instructions
import           Parser.Procedures   (listArgProc, listDefProc, panicMode,
                                      panicModeId)
import           Parser.Token
import           Parser.Type
import           ParserState
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (void)
import           Data.Text           (Text)
import           Text.Megaparsec     (Dec, ParseError, between, choice,
                                      getPosition, sepBy, try, (<|>))
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela (Maybe (AST Type))
abstractDataType = do
  pos <- getPosition
  match TokAbstract
  abstractId <- panicModeId  -- parseLeftParent
  abstractTypes
  insertType abstractId (GAbstractType abstractId [] [] []) pos
  match TokBegin  -- (parseVar <|> parseTokLeftInv) PE.Begin

  newScopeParser
  addSymbolParser abstractId (AbstractContent abstractId pos)
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
topDecl = choice [ verify TokAbstract
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

    parens $ identifier `sepBy` match TokComma
    -- try $do void parseTokRightPar
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
abstType =  do {match TokSet;      match TokOf; GSet      <$> basic }
        <|> do {match TokMultiset; match TokOf; GMultiset <$> basic }
        <|> do {match TokSeq;      match TokOf; GSeq      <$> basic }
        <|> do {match TokFunc; ba <- basic; match TokArrow; bb <- basic; return $ GFunc ba bb}
        <|> do {match TokRel;  ba <- basic; match TokBiArrow; bb <- basic; return $ GRel ba bb}
        <|> GTuple <$> parens (basic `sepBy` parseComma)
        <|> basic


basic :: Graciela Type
basic = GTypeVar <$> identifier


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
    targs <- listArgProc id (match TokRightPar) (match TokRightPar)         -- arguments
    match TokRightPar                                                       -- )
    pre   <- precondition parseTokLeftPost                                  -- pre
    post  <- postcondition follow                                           -- post
    sb    <- getCurrentScope
    addProcTypeParser id targs pos sb
    exitScopeParser
    addProcTypeParser id targs pos sb

    -- panicMode parseLeftParent (argTypes <|> parseTokRightPar) PE.TokenLP
    -- newScopeParser
    -- targs <- listArgProc id parseTokRightPar parseTokRightPar
    -- panicMode parseTokRightPar parseTokLeftPre PE.TokenRP
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
    insertType typeId (GDataType typeId [] [] []) pos
    newScopeParser
    addSymbolParser typeId (TypeContent typeId pos)
    beginEnd typeBody
    exitScopeParser
    return Nothing
    -- panicMode (verify TokImplements) parseTokId PE.Implements
    -- abstractId <- panicModeId parseLeftParent
    -- types
    -- insertType typeId (GDataType typeId [] [] []) pos
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
        return ()
      types = do
        match TokLeftParent
        t  <- identifier `sepBy` match TokComma
        match TokTokRightPar
        return t



-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
