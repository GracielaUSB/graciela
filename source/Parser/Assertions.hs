module Parser.Assertions 
  ( assertions
  , precondition
  , postcondition
  , bound
  , assertion
  , invariant
  )
  where


-------------------------------------------------------------------------------
import Parser.Declarations
import Parser.Expression
import MyParseError                  as PE
import ParserState
import TokenParser
import ParserType
import Contents
import Location
import Token
import State
import Type
import AST
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP
import           Text.Parsec
-------------------------------------------------------------------------------


assertions initial final ty follow =
    try (
      do initial
         e <- expr final (follow <|> final)
         try (
           do final
              pos <- getPosition
              return $ AP.liftA2 (States ty (toLocation pos)) e (return (GEmpty))
             )
             <|> do genNewError follow PE.TokenCA
                    return Nothing
         )
         <|> do genNewError follow PE.TokenOA
                return Nothing

precondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
precondition follow recSet = assertions parseTokLeftPre parseTokRightPre Pre follow

postcondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
postcondition follow recSet = assertions parseTokLeftPost parseTokRightPost Post follow

bound :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
bound follow recSet = assertions parseTokLeftBound parseTokRightBound Bound follow

assertion :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
assertion follow recSet = assertions parseTokLeftA parseTokRightA Assertion follow

invariant :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
invariant follow recSet = assertions parseTokLeftInv parseTokRightInv Invariant follow

--repInvariant :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
repInvariant = undefined

--acInvariant :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
acInvariant = undefined








