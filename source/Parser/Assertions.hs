module Parser.Assertions 
  ( assertions
  , precondition
  , postcondition
  , bound
  , assertion
  , invariant
  , repInvariant
  , acInvariant
  )
  where


-------------------------------------------------------------------------------
import Parser.Declarations
import Parser.Expression
import MyParseError                  as PE
import ParserState
import Parser.TokenParser
import ParserType
import Contents
import Location
import Token
import State
import Type
import AST
-------------------------------------------------------------------------------
import           Control.Monad       (void, unless)
import qualified Control.Applicative as AP
import           Text.Parsec
-------------------------------------------------------------------------------

assertions :: MyParser Token -> MyParser Token 
           -> StateCond      -> MyParser Token 
           -> MyParser (Maybe (AST Type) )
assertions initial final ty follow = do
    try $do initial
            e <- expr final (follow <|> final)
            try $do final
                    pos <- getPosition
                    return $ AP.liftA2 (States ty (toLocation pos)) e (return (GEmpty))
             <|> do t <- lookAhead follow
                    genNewError (return t) PE.TokenCA
                    return Nothing
             <|> do (t:_) <- manyTill anyToken $lookAhead follow
                    genNewError (return $fst t) PE.TokenCA
                    return Nothing
     <|> do (t:_) <- manyTill anyToken final
            genNewError (return $fst t) PE.TokenOA
            return $Nothing
     <|> do (t:_) <- manyTill anyToken $lookAhead follow
            genNewError (return $fst t) PE.TokenOA
            return $Nothing
      

precondition :: MyParser Token -> MyParser (Maybe (AST Type) )
precondition follow = assertions parseTokLeftPre parseTokRightPre Pre follow

postcondition :: MyParser Token -> MyParser (Maybe (AST Type) )
postcondition follow = assertions parseTokLeftPost parseTokRightPost Post follow

bound :: MyParser Token -> MyParser (Maybe (AST Type) )
bound follow = assertions parseTokLeftBound parseTokRightBound Bound follow

assertion :: MyParser Token -> MyParser (Maybe (AST Type) )
assertion follow = assertions parseTokLeftA parseTokRightA Assertion follow

invariant :: MyParser Token -> MyParser (Maybe (AST Type) )
invariant follow = assertions parseTokLeftInv parseTokRightInv Invariant follow

repInvariant :: MyParser (Maybe (AST Type))
repInvariant = do return Nothing

acInvariant :: MyParser (Maybe (AST Type) )
acInvariant = do return Nothing








