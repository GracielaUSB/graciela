module Parser.Instructions 
    ( CasesConditional(..)
    , actionsList
    , actionsListAux
    , action
    , actionAux
    , followAction
    , block
    , random
    , guardsList
    , guardsListAux
    , guard
    , functionCallOrAssign
    , idAssignListAux
    , write
    , writeln
    , abort
    , conditional
    , repetition
    , skip
    ) where

-------------------------------------------------------------------------------
import Parser.Declarations
import Parser.Expression
import Parser.Assertions
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
import           Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Control.Monad       as M
import qualified Data.Text           as T
import           Text.Parsec
import           Data.Maybe
-------------------------------------------------------------------------------

data CasesConditional = CExpression | CAction


actionsList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
actionsList follow recSet =
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
            rl <- actionsListAux follow recSet
            return $ AP.liftA2 (:) ac rl

actionsListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
actionsListAux follow recSet =
  do parseSemicolon
     ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
     rl <- actionsListAux follow recSet
     return (AP.liftA2 (:) ac rl)
     <|> do return $ return []

action :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
action follow recSet =
    do pos <- getPosition
       do  lookAhead followAction
           actionAux follow recSet
           <|> do lookAhead parseTokLeftA
                  as  <- assertion followAction (followAction <|> recSet)
                  do lookAhead followAction
                     res <- actionAux follow recSet
                     return $ AP.liftA3 (GuardAction (toLocation pos)) as res (return GEmpty)
                     <|> do genNewError follow Action
                            return Nothing
                  <|> do genNewError follow Action
                         return Nothing
           <|> do genNewError follow Action
                  return Nothing

actionAux :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
actionAux follow recSet =
        skip follow recSet
    <|> conditional CAction follow recSet
    <|> abort follow recSet
    <|> write follow recSet
    <|> writeln follow recSet
    <|> functionCallOrAssign follow recSet
    <|> random follow recSet
    <|> block follow recSet
    <|> repetition follow recSet


followAction ::  MyParser Token
followAction = (parseTokID <|> parseIf <|> parseAbort <|> parseSkip <|>
                  parseTokOpenBlock <|> parseWrite <|> parseWriteln <|> parseTokLeftInv <|> parseRandom)

block :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
block follow recSet =
    do pos <- getPosition
       parseTokOpenBlock
       newScopeParser
       dl  <- decList followAction (recSet <|> followAction)
       la  <- actionsList (parseTokCloseBlock) (parseTokCloseBlock <|> recSet)
       st  <- getCurrentScope
       exitScopeParser
       do parseTokCloseBlock
          return $ (AP.liftA2 (Block (toLocation pos) st) dl la) AP.<*> (return GEmpty)
          <|> do genNewError follow TokenCB
                 return Nothing


random :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
random follow recSet =
    do pos <- getPosition
       parseRandom
       do parseLeftParent
          do id  <- parseID
             do parseRightParent
                cont <- lookUpSymbol id
                let t = symbolType $ fromJust $ cont
                return $ return $ Ran id t (toLocation pos) GEmpty
                <|> do genNewError follow TokenRP
                       return Nothing
             <|> do genNewError follow IDError
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


guardsList :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
guardsList casec follow recSet =
    do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
       gl <- guardsListAux casec follow recSet
       return $ AP.liftA2 (:) g gl


guardsListAux :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
guardsListAux casec follow recSet =
  do parseSepGuards
     g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
     rl <- guardsListAux casec follow recSet
     return $ AP.liftA2 (:) g rl
     <|> do return $ return []


guard :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
guard CAction follow recSet     =
    do pos <- getPosition
       e <- expr (parseArrow) (recSet <|> parseArrow)
       parseArrow
       a <- action follow recSet
       return (AP.liftA2  (\f -> f (toLocation pos)) (AP.liftA2 Guard e a) (return (GEmpty)))

guard CExpression follow recSet =
    do pos <- getPosition
       e <- expr (parseArrow) (recSet <|> parseArrow)
       parseArrow
       do lookAhead parseIf
          a <-(conditional CExpression follow recSet)
          return (AP.liftA2 (\f -> f (toLocation pos)) (AP.liftA2 GuardExp e a) (return (GEmpty)))
          <|> do a <- expr follow recSet
                 return (AP.liftA2 (\f -> f (toLocation pos)) (AP.liftA2 GuardExp e a) (return (GEmpty)))


functionCallOrAssign ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
functionCallOrAssign follow recSet =
    do pos <- getPosition
       id <- parseID
       do parseLeftParent
          lexp  <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
          do parseRightParent
             sb <- getCurrentScope
             return $ (fmap (ProcCall id sb (toLocation pos)) lexp) AP.<*> (return GEmpty)
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do bl <- bracketsList (parseComma <|> parseAssign) (parseComma <|> parseAssign <|> recSet)
                 rl <- idAssignListAux parseAssign (recSet <|> parseAssign)
                 t <- lookUpConsParser id
                 parseAssign
                 do le <- listExp follow recSet
                    case bl of
                      Nothing  -> return Nothing
                      Just bl' ->
                        case bl' of
                          [] ->
                            do let idast = fmap (ID (toLocation pos) id) t
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (toLocation pos)) (return GEmpty)
                          otherwise ->
                            do let idast = (fmap (ArrCall (toLocation pos) id) bl) AP.<*>  t
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (toLocation pos)) (return GEmpty)
                 <|> do genNewError follow TokenAs
                        return Nothing

idAssignListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe ([AST Type]))
idAssignListAux follow recSet =
  do parseComma
     pos <- getPosition
     do ac <- parseID
        t  <- lookUpConsParser ac
        bl <- bracketsList (parseComma <|> parseAssign)
                (parseComma <|> parseAssign <|> recSet)
        rl <- idAssignListAux follow recSet
        case bl of
          Nothing  -> return Nothing
          Just bl' ->
            case bl' of
              [] ->
                do let ast = fmap (ID (toLocation pos) ac) t
                   return $ AP.liftA2 (:) ast rl
              otherwise ->
                do let ast = (fmap (ArrCall (toLocation pos) ac) bl) AP.<*>  t
                   return $ AP.liftA2 (:) ast rl
        <|> do genNewError follow IDError
               return Nothing
     <|> do return $ return []

write :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
write follow recSet =
    do pos <- getPosition
       parseWrite
       do parseLeftParent
          e   <- expr parseRightParent (recSet <|> parseRightParent)
          do parseRightParent
             return $ ((fmap (Write False) e) AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty))
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


writeln ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
writeln follow recSet =
    do pos <- getPosition
       parseWriteln
       do parseLeftParent
          e <- expr parseRightParent (recSet <|> parseRightParent)
          do parseRightParent
             return $ (fmap (Write True) e) AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty)
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


abort ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
abort folow recSet =
    do pos <- getPosition
       parseAbort
       return $ return $ Abort (toLocation pos) GEmpty


conditional :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
conditional casec follow recSet =
    do pos <- getPosition
       parseIf
       gl <- guardsList casec parseFi (recSet <|> parseFi)
       do parseFi
          return $ (fmap (Cond) gl) AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty)
          <|> do genNewError follow TokenFI
                 return Nothing

repetition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
repetition follow recSet =
    do pos <- getPosition
       inv <- invariant (parseTokLeftBound) (recSet <|> parseTokLeftBound)
       bou <- bound (parseDo) (parseDo <|> recSet)
       do parseDo
          gl <- guardsList CAction parseOd (recSet <|> parseOd)
          do parseOd
             return((fmap (Rept) gl) AP.<*> inv AP.<*> bou AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty))
             <|> do genNewError follow TokenOD
                    return Nothing
          <|> do genNewError follow TokEOFO
                 return Nothing

skip :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
skip follow recSet =
    do  pos <- getPosition
        parseSkip
        return $ return $ Skip (toLocation pos) GEmpty