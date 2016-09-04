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
-- import           Parser.Rhecovery
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (forM_, void)
import           Data.Maybe          (catMaybes)
import qualified Data.Sequence       as Seq (empty, fromList)
import           Data.Text           (Text)
import           Text.Megaparsec     (Dec, ParseError, between, choice,
                                      getPosition, lookAhead, manyTill,
                                      notFollowedBy, try, (<|>))
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Parser (Maybe Struct)
abstractDataType = do
    from <- getPosition
    match TokAbstract
    abstractId <- identifier
    atypes <- parens (identifier `sepBy` match TokComma)
    match' TokBegin
    symbolTable %= openScope from
    decls <- abstractDec `endBy` match TokSemicolon
    inv   <- safeAssertion invariant (NoAbstractInvariant abstractId)
    -- procs <- many abstractProcedure
    let procs = Seq.empty
    match' TokEnd
    to    <- getPosition
    let loc = Location (from,to)
    symbolTable %= closeScope to
    pure Struct
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
      match' TokColon
      t  <- abstractType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do
                symbolTable %= insertSymbol id (Entry id loc (Var t Nothing){- TODO-})
            ) ids
      let ids' = fst <$> ids
      pure Declaration
        { declLoc  = location
        , declType = t
        , declIds  = Seq.fromList ids' }

    variable = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy` match TokComma
      match' TokColon
      t  <- abstractType
      to <- getPosition
      let location = Location (from,to)
      mapM_ (\(id,loc) -> do
                symbolTable %= insertSymbol id (Entry id loc (Var t Nothing))
            ) ids
      let ids' = fst <$> ids
      pure Declaration
        { declLoc  = location
        , declType = t
        , declIds  = Seq.fromList ids' }


-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Parser (Maybe Struct)
dataType = do

    match TokType
    from <- getPosition
    id <- identifier
    match' TokImplements
    abstractId <- identifier
    types <- parens $ (try typeVar <|> basicType) `sepBy` match TokComma
    symbolTable %= openScope from

    match' TokBegin
    decls   <- declaration `endBy` (match TokSemicolon)

    repinv  <- safeAssertion repInv  (NoTypeRepInv  id)
    coupinv <- safeAssertion coupInv (NoTypeCoupInv id)

    procs   <- many procedure
    match' TokEnd

    to <- getPosition
    symbolTable %= closeScope to
    let
       loc = Location(from,to)
       fields = getFields =<< decls
    insertType id (GDataType id fields) from
    symbolTable %= insertSymbol id (Entry id loc TypeEntry)
    pure Struct
        { structName = id
        , structLoc  = loc
        , struct'    = DataType
         { abstract = abstractId
         , types    = types
         , decls    = decls
         , repinv   = repinv
         , coupinv  = coupinv
         , procs    = procs}}
