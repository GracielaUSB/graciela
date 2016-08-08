module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST.Definition
import           AST.Instruction
import           AST.Struct
import           Graciela
import           MyParseError        as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Instruction
import           Parser.Procedure
import           Parser.Token
import           Parser.Type
import           Location
import           SymbolTable
import           Entry
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (void)
import           Data.Text           (Text)
import           Data.Maybe          (catMaybes)
import           Text.Megaparsec     (Dec, ParseError, between, choice, many,
                                      getPosition, sepBy, try, endBy, (<|>),
                                      manyTill, lookAhead, notFollowedBy)
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela Struct
abstractDataType = do
    from <- getPosition
    match TokAbstract
    abstractId <- identifier
    atypes <- parens (identifier `sepBy` match TokComma)
    match TokBegin
    symbolTable %= openScope from
    decls <- abstractDec `endBy` match TokSemicolon
    inv <- invariant
    procs <- many procedureDeclaration
    match TokEnd
    to <- getPosition
    let loc = Location (from,to)
    symbolTable %= closeScope to
    return $ Struct abstractId loc (AbstractDataType atypes decls inv procs)

abstractDec :: Graciela Instruction
abstractDec = constant <|> variable
  where 
    constant = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy` match TokComma
      match TokColon
      t  <- abstType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do 
                symbolTable %= insertSymbol id (Entry id loc (Const t None))
            ) ids
      let ids' = map (\(id,_) -> id) ids
      return $ Instruction location (Declaration t ids' [])
    variable = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy` match TokComma
      match TokColon
      t  <- abstType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do 
                symbolTable %= insertSymbol id (Entry id loc (Var t Nothing))
            ) ids
      let ids' = map (\(id,_) -> id) ids
      return $ Instruction location (Declaration t ids' [])



-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Graciela Struct
dataType = do
    from <- getPosition
    match TokType
    id <- identifier
    match TokImplements
    abstractId <- identifier
    types <- parens $ (try genericType <|> basicType') `sepBy` match TokComma
    symbolTable %= openScope from
    match TokBegin 
    decls   <- (variableDeclaration <|> constantDeclaration) `endBy` (match TokSemicolon)
    repinv  <- repInvariant
    coupinv <- coupInvariant
    procs   <- many procedure
    match TokEnd
    to <- getPosition
    symbolTable %= closeScope to
    let loc = Location(from,to)
    symbolTable %= insertSymbol id (Entry id loc (TypeEntry))
    return $ Struct id loc (DataType abstractId types decls repinv coupinv procs)

    where
      basicType' = do 
        t <- basicType
        return $ Right t
      genericType = do 
        id <- identifier
        notFollowedBy (match TokTimes) 
        return $ Left id