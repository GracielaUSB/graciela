module Parser.Definition
  ( listDefProc
  , function
  , procedure
  , paramMode
  , procParam
  , procedureDeclaration
  ) where

-------------------------------------------------------------------------------
import           AST.Expression
import           AST.Definition
import           AST.Instruction
import           Type
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
                                     getPosition, eitherP, try, lookAhead,
                                     manyTill)
-------------------------------------------------------------------------------


listDefProc :: Graciela [Definition]
listDefProc = many (function <|> procedure)

{- Parse a function. If something went wrong, it should return a bad definition -}
function :: Graciela Definition
function  = do
  from <- getPosition
  match TokFunc

  id     <- identifier
  params <- parens $ functionParameters `sepBy` match TokComma
  withRecovery TokArrow
  retType <- type'
  symbolTable %= openScope from
  withRecovery TokBegin
  body <- expression
  withRecovery TokEnd
  to  <- getPosition
  let location = Location (from, to)
  st  <- use symbolTable
  symbolTable %= closeScope to
  if retType == GUndef
    then return $ BadDefinition location
    else case body of
      BadExpression _ -> do
        putError (loc body) $ UnknownError "Bad expression"
        return $ BadDefinition location
      _ -> if retType /= expType body
        then do
          putError (loc body) $ BadFuncExpressionType
                { fName = id
                , fType = retType
                , eType = expType body}
          return $ BadDefinition location
        else do
          let entry = Entry
                { _entryName = id
                , _loc       = location
                , _info      = Function
                  { _funcType   = retType
                  , _funcParams = params
                  , _funcTable  = st }}
          symbolTable %= insertSymbol id entry

          let def = Definition
                { defLoc   = location
                , defName  = id
                , params   = params
                , st       = st
                , defBound = Nothing
                , def'     = FunctionDef
                  { funcBody = body
                  , retType  = retType }}
          return def

  where

    functionParameters :: Graciela (T.Text, Type)
    functionParameters = do
        from <- getPosition
        id <- identifier
        withRecovery TokColon
        retType  <- type'
        to <- getPosition
        let loc = Location(from,to)
        symbolTable %= insertSymbol id (Entry id loc (Argument In retType))
        return (id, retType)

{- Parse a procedure. If something went wrong, it should return a bad definition-}
procedure :: Graciela Definition
procedure = do
    from <- getPosition
    -- Parse the procedure signature. it must not be followed by an Arrow (->).
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

    bodyFrom <- getPosition
    body <- try block
          <|> do
            bodyTo <- getPosition
            let loc = Location (bodyFrom, bodyTo)
            putError loc $ NoProcBody id
            try $ anyToken `manyTill` lookAhead (match TokLeftPost)
            return $ BadInstruction loc

    post <- safeAssertion postcondition (NoProcPostcondition id)
    withRecovery TokEnd

    -- Get the actual symbol table and build the ast and the entry of the procedure
    st    <- use symbolTable
    to    <- getPosition
    let loc = Location(from,to)
    symbolTable %= closeScope to
    currentProc .= Nothing
    if checkExp pre && checkExp pre && checkInst body && id /= errorId
      then do
        let entry = Entry
              { _entryName = id
              , _loc       = loc
              , _info      = Procedure
                { _procParams = params
                , _procTable  = st }}
        symbolTable %= insertSymbol id entry
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
  where
    checkExp e = case e of
      BadExpression _ -> False
      _ -> True
    checkInst e = case e of
      BadInstruction _ -> False
      _ -> True
{- Gets the mode of the parameter. If fail then returns Nothing -}
paramMode :: Graciela (Maybe ArgMode)
paramMode =  match TokIn    $> Just In
         <|> match TokInOut $> Just InOut
         <|> match TokOut   $> Just Out
         <|> match TokRef   $> Just Ref
         <|> return Nothing

{- Parse a parameter and put it in the symbol table -}
procParam :: Graciela (T.Text, Type)
procParam = do
  from  <- getPosition

  ptype <- paramMode
  id    <- identifier
  withRecovery TokColon
  retType     <- type'

  to    <- getPosition
  let loc = Location(from,to)

  case ptype of
    Just x | retType /= GUndef -> symbolTable %= insertSymbol id (Entry id loc (Argument In retType))
    _  -> genCustomError ("Se debe especificar el comportamiento de la variable `"
                           <>T.unpack id<>"` (In, Out, InOut)")
  return (id, retType)



-- ProcDecl -> 'proc' Id ':' '(' ListArgProc ')' Precondition Postcondition

{- Parse declarations of procedures (only parameters, pre and post) in abstract types -}
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
    let entry = Entry
            { _entryName = id
            , _loc       = loc
            , _info      = Procedure
              { _procParams = params
              , _procTable  = st }}
    symbolTable %= insertSymbol id entry

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
