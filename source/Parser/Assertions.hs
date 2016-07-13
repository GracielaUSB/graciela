module Parser.Assertions
  ( assertions
  , precondition
  , postcondition
  , bound
  , assertion
  , invariant
  , repInvariant
  , coupInvariant
  )
  where


-------------------------------------------------------------------------------
import Parser.Declarations
import Parser.Expression
import MyParseError                  as PE
import ParserState
import Parser.TokenParser
import Parser.ParserType
import Contents
import Location
import Token
import Graciela
import Type
import AST
-------------------------------------------------------------------------------
import           Control.Monad       (void, unless)
import qualified Control.Applicative as AP
import           Text.Parsec
-------------------------------------------------------------------------------

assertions :: Graciela Token -> Graciela Token
           -> StateCond      -> Graciela Token
           -> Graciela (Maybe (AST Type) )
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

     <|> do t <- lookAhead follow
            genNewError (return t) PE.TokenOA
            return $Nothing
     <|> do (t:_) <- manyTill anyToken final
            genNewError (return $fst t) PE.TokenOA
            return $Nothing
     <|> do (t:_) <- manyTill anyToken $lookAhead follow
            genNewError (return $fst t) PE.TokenOA
            return $Nothing


precondition :: Graciela Token -> Graciela (Maybe (AST Type) )
precondition follow = assertions parseTokLeftPre parseTokRightPre Pre follow

postcondition :: Graciela Token -> Graciela (Maybe (AST Type) )
postcondition follow = assertions parseTokLeftPost parseTokRightPost Post follow

bound :: Graciela Token -> Graciela (Maybe (AST Type) )
bound follow = assertions parseTokLeftBound parseTokRightBound Bound follow

assertion :: Graciela Token -> Graciela (Maybe (AST Type) )
assertion follow = assertions parseTokLeftA parseTokRightA Assertion follow

invariant :: Graciela Token -> Graciela (Maybe (AST Type) )
invariant follow = assertions parseTokLeftInv parseTokRightInv Invariant follow

repInvariant :: Graciela (Maybe (AST Type))
repInvariant = assertions (verify TokLeftRep) (verify TokRightRep) Representation
                          (parseEnd <|> parseProc <|> (verify TokLeftAcopl))

coupInvariant :: Graciela (Maybe (AST Type) )
coupInvariant = assertions (verify TokLeftAcopl) (verify TokRightAcopl) Couple
                          (parseEnd <|> parseProc)
