module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Definition
import           AST.Expression      (Expression (..))
import           AST.Instruction
import           AST.Struct
import           Entry
import           Error
import           Location
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Definition
import           Parser.Instruction
import           Parser.Monad
import           Parser.Recovery
import           Parser.Type
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (void)
import           Data.Maybe          (catMaybes)
import qualified Data.Sequence       as Seq (fromList)
import           Data.Text           (Text)
import           Text.Megaparsec     (Dec, ParseError, between, choice, endBy,
                                      getPosition, lookAhead, many, manyTill,
                                      notFollowedBy, sepBy, try, (<|>))
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Parser (Maybe Struct)
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

abstractDec :: Parser (Maybe Declaration)
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
      return $ Declaration
        { declLoc  = location
        , declType = t
        , declIds  = Seq.fromList ids' }

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
      return $ Declaration
        { declLoc  = location
        , declType = t
        , declIds  = Seq.fromList ids' }


-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Parser (Maybe Struct)
dataType = do

    match TokType
    from <- getPosition
    id <- identifier
    withRecovery TokImplements
    abstractId <- identifier
    types <- parens $ (try typeVar <|> basicType) `sepBy` match TokComma
    symbolTable %= openScope from

    withRecovery TokBegin
    decls   <- declaration `endBy` (match TokSemicolon)

    repinv  <- safeAssertion repInvariant  (NoTypeRepInv  id)
    coupinv <- safeAssertion coupInvariant (NoTypeCoupInv id)

    procs   <- many procedure
    withRecovery TokEnd

    to <- getPosition
    symbolTable %= closeScope to
    let
       loc = Location(from,to)
       fields = concat . fmap getFields $ decls
    insertType id (GDataType id fields) from
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
