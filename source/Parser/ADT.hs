module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Expression     (Expression(..))
import           AST.Definition
import           AST.Instruction
import           AST.Struct
import           Graciela
import           Error
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Instruction
import           Parser.Procedure
import           Parser.Recovery
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
    withRecovery TokBegin
    symbolTable %= openScope from
    decls <- abstractDec `endBy` match TokSemicolon
    inv   <- safeAssertion invariant (NoAbstractInvariant abstractId)
    procs <- many procedureDeclaration
    withRecovery TokEnd
    to    <- getPosition
    let loc = Location (from,to)
    symbolTable %= closeScope to
    return $ Struct
        { structName = abstractId
        , structLoc  = loc
        , struct'    = AbstractDataType
         { atypes = atypes
         , decls  = decls
         , inv    = inv
         , procs  = procs}}

abstractDec :: Graciela Declaration
abstractDec = constant <|> variable
  where 
    constant = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy` match TokComma
      withRecovery TokColon
      t  <- abstractType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do 
                symbolTable %= insertSymbol id (Entry id loc (Var t Nothing){- TODO-})
            ) ids
      let ids' = map (\(id,_) -> id) ids
      return $ Declaration location t ids' []
    variable = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy` match TokComma
      withRecovery TokColon
      t  <- abstractType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do 
                symbolTable %= insertSymbol id (Entry id loc (Var t Nothing))
            ) ids
      let ids' = map (\(id,_) -> id) ids
      return $ Declaration location t ids' []



-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Graciela Struct
dataType = do
    from <- getPosition
    match TokType
    id <- identifier
    withRecovery TokImplements
    abstractId <- identifier
    types <- parens $ (try typeVar <|> basicType) `sepBy` match TokComma
    symbolTable %= openScope from
    
    withRecovery TokBegin 
    decls   <- (variableDeclaration <|> constantDeclaration) `endBy` (match TokSemicolon)

    repinv  <- safeAssertion repInvariant  (NoTypeRepInv  id)
    coupinv <- safeAssertion coupInvariant (NoTypeCoupInv id)

    procs   <- many procedure
    withRecovery TokEnd
    
    to <- getPosition
    symbolTable %= closeScope to
    let loc = Location(from,to)
    insertType id (GAbstractType id) from
    symbolTable %= insertSymbol id (Entry id loc (TypeEntry))
    return $ Struct
        { structName = id
        , structLoc  = loc
        , struct'    = DataType
         { abstract = abstractId
         , types    = types
         , decls    = decls
         , repinv   = repinv
         , coupinv  = coupinv
         , procs    = procs}}

