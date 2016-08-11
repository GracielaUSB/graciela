module Parser.Procedure
  ( listDefProc
  , function
  , procedure
  , paramType
  , procParam
  , procedureDeclaration
  ) where

-------------------------------------------------------------------------------
import           AST.Definition
import           AST.Type
import           Entry
import           Graciela
import           Location
import           Error       as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Instruction
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
-------------------------------------------------------------------------------
import           Control.Monad      (void, when)
import           Data.List          (partition)
import           Data.Functor       (($>))
import           Data.Monoid        ((<>))
import           Control.Lens       (use, (%=), (.=))
import qualified Data.Text          as T
import           Data.Maybe         (catMaybes)
import           Text.Megaparsec    ((<|>), many, notFollowedBy, sepBy,
                                     getPosition, eitherP)
-------------------------------------------------------------------------------

listDefProc :: Graciela [Definition]
listDefProc = many (function <|> procedure)

function :: Graciela Definition
function  = do
  from <- getPosition
  match TokFunc

  id      <- identifier
  params  <- parens $ functionParameters `sepBy` match TokComma
  withRecovery TokArrow
  tname   <- identifier
  t <- getType tname
  when (t == Nothing) $
    genCustomError ("El tipo `" <> T.unpack tname <> "` no existe.")
  let (Just retType) = t
  symbolTable %= openScope from
  withRecovery TokBegin
  body <- expression
  withRecovery TokEnd 
  to  <- getPosition
  let loc = Location (from, to)

  st  <- use symbolTable
  symbolTable %= closeScope to
  symbolTable %= insertSymbol id (Entry id loc (Function retType params st))

  let def = Definition 
        { defLoc   = loc
        , defName  = id
        , params   = params
        , st       = st
        , defBound = Nothing
        , def'     = FunctionDef 
          { funcBody = body
          , retType  = retType }} 
  return def
      -- <|> return (AST from from GUndef (EmptyAST))

  where
    functionParameters :: Graciela (T.Text, Type)
    functionParameters = do
        from <- getPosition
        id <- identifier
        withRecovery TokColon
        t  <- type'
        to <- getPosition
        let loc = Location(from,to)
        symbolTable %= insertSymbol id (Entry id loc (Argument In t))
        return (id, t)

procedure :: Graciela Definition
procedure = do
    from <- getPosition
    -- Parse the procedure signature. it most not be followed by an Arrow (->).
    match TokProc
    id <- safeIdentifier
    symbolTable %= openScope from
    params <- parens $ procParam `sepBy` match TokComma
    notFollowedBy $ match TokArrow
    currentProc .= Just (id, from, params)
    -- Parse the procedure's body
    withRecovery TokBegin
    decls <- declarationOrRead
    pre  <- safeAssertion precondition  (NoProcPrecondition  id)
    body <- block 
    post <- safeAssertion postcondition (NoProcPostcondition id)
    withRecovery TokEnd
    -- Get the actual symbol table and build the ast and the entry of the procedure
    st    <- use symbolTable
    to    <- getPosition
    let loc = Location(from,to)
    symbolTable %= closeScope to
    currentProc .= Nothing
    if id /= errorId
      then do 
        symbolTable %= insertSymbol id (Entry id loc (Procedure params st))
        let def = Definition 
              { defLoc   = loc
              , defName  = id
              , params   = params
              , st       = st
              , defBound = Nothing
              , def'     = ProcedureDef 
                { procDecl = decls
                , pre      = pre 
                , procBody = body
                , post     = post }} 
        return def
      else 
        return $ BadDefinition loc

paramType :: Graciela (Maybe ArgMode)
paramType =  match TokIn    $> Just In
         <|> match TokInOut $> Just InOut
         <|> match TokOut   $> Just Out
         <|> match TokRef   $> Just Ref
         <|> return Nothing


procParam :: Graciela (T.Text, Type)
procParam = do
  from  <- getPosition
  ptype <- paramType
  id    <- identifier
  withRecovery TokColon
  t     <- type'
  to    <- getPosition
  let loc = Location(from,to)
  case ptype of
    Just x | t /= GUndef -> symbolTable %= insertSymbol id (Entry id loc (Argument In t))
    _  -> genCustomError ("Se debe especificar el comportamiento de la variable `"
                           <>T.unpack id<>"` (In, Out, InOut)")
  return (id, t)


-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition
procedureDeclaration :: Graciela Definition
procedureDeclaration = do
    from <- getPosition
    match TokProc
    id <- identifier
    symbolTable %= openScope from
    params <- parens $ procParam `sepBy` match TokComma
    notFollowedBy $ match TokArrow
    pre  <- precondition
    post <- postcondition
    st   <- use symbolTable
    to   <- getPosition
    let loc = Location (from,to)
    symbolTable %= closeScope to
    symbolTable %= insertSymbol id (Entry id loc (Procedure params st))
    let def = Definition 
          { defLoc   = loc
          , defName  = id
          , params   = params
          , st       = st
          , defBound = Nothing
          , def'     = AbstractProcedureDef 
            { pre  = pre 
            , post = post }} 
    return def


