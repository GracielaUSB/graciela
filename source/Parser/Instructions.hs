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
    , new
    , free
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
import Parser.Type
import Contents
import Location
import Token
import           Graciela
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


actionsList :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
actionsList follow recSet =
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
            rl <- actionsListAux follow recSet
            return $ AP.liftA2 (:) ac rl

actionsListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
actionsListAux follow recSet =
  do parseSemicolon
     ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
     rl <- actionsListAux follow recSet
     return (AP.liftA2 (:) ac rl)
     <|> do return $ return []

action :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
action follow recSet =
    do pos <- getPosition
       do  lookAhead followAction
           actionAux follow recSet
           <|> do lookAhead parseTokLeftA
                  as  <- assertion followAction
                  do lookAhead followAction
                     res <- actionAux follow recSet
                     return $ AP.liftA3 (GuardAction (toLocation pos)) as res (return GEmpty)
                     <|> do genNewError follow Action
                            return Nothing
                  <|> do genNewError follow Action
                         return Nothing
           <|> do genNewError follow Action
                  return Nothing

actionAux :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
actionAux follow recSet =
        skip follow recSet
    <|> conditional CAction follow recSet
    <|> abort follow recSet
    <|> write follow recSet
    <|> writeln follow recSet
    <|> new follow recSet
    <|> free follow recSet
    <|> functionCallOrAssign follow recSet
    <|> random follow recSet
    <|> block follow recSet
    <|> repetition follow recSet


followAction ::  Graciela Token
followAction = (parseTokId <|> parseIf <|> parseAbort <|> parseSkip <|>
                  parseTokOpenBlock <|> parseWrite <|> parseWriteln <|> parseTokLeftInv <|> parseRandom)

block :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
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


random :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
random follow recSet =
    do pos <- getPosition
       parseRandom
       do parseLeftParent
          do id  <- parseId
             do parseRightParent
                cont <- lookUpSymbol id
                case cont of 
                  Just (Contents _ _ _ t _ _) -> 
                    return $ return $ Ran id t (toLocation pos) GEmpty
                  Just (ArgProcCont _ _ _ t) ->
                    return $ return $ Ran id t (toLocation pos) GEmpty
                  Just (FunctionCon _ _ t _ _) ->
                    return $ return $ Ran id t (toLocation pos) GEmpty
                  _ -> 
                    return $ return $ Ran id GEmpty (toLocation pos) GError
                <|> do genNewError follow TokenRP
                       return Nothing
             <|> do genNewError follow IdError
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


guardsList :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
guardsList casec follow recSet =
    do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
       gl <- guardsListAux casec follow recSet
       return $ AP.liftA2 (:) g gl


guardsListAux :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
guardsListAux casec follow recSet =
  do parseSepGuards
     g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
     rl <- guardsListAux casec follow recSet
     return $ AP.liftA2 (:) g rl
     <|> do return $ return []


guard :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
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


functionCallOrAssign ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
functionCallOrAssign follow recSet =
    do pos <- getPosition
       id <- parseId
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
                            do let idast = fmap (Id (toLocation pos) id) t
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (toLocation pos)) (return GEmpty)
                          otherwise ->
                            do let idast = (fmap (ArrCall (toLocation pos) id) bl) AP.<*>  t
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (toLocation pos)) (return GEmpty)
                 <|> do genNewError follow TokenAs
                        return Nothing

idAssignListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe ([AST Type]))
idAssignListAux follow recSet =
  do parseComma
     pos <- getPosition
     do ac <- parseId
        t  <- lookUpConsParser ac
        bl <- bracketsList (parseComma <|> parseAssign)
                (parseComma <|> parseAssign <|> recSet)
        rl <- idAssignListAux follow recSet
        case bl of
          Nothing  -> return Nothing
          Just bl' ->
            case bl' of
              [] ->
                do let ast = fmap (Id (toLocation pos) ac) t
                   return $ AP.liftA2 (:) ast rl
              otherwise ->
                do let ast = (fmap (ArrCall (toLocation pos) ac) bl) AP.<*>  t
                   return $ AP.liftA2 (:) ast rl
        <|> do genNewError follow IdError
               return Nothing
     <|> do return $ return []

write :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
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


writeln ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
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

new :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
new follow recSet = do
    parseNew
    parseLeftParent
    _ <- parseId
    parseRightParent

    return . Just . EmptyAST $ GEmpty

free :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
free follow recSet = do
    parseFree
    parseLeftParent
    _ <- parseId
    parseRightParent

    return . Just . EmptyAST $ GEmpty

abort ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
abort folow recSet =
    do pos <- getPosition
       parseAbort
       return $ return $ Abort (toLocation pos) GEmpty


conditional :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
conditional casec follow recSet =
    do pos <- getPosition
       parseIf
       gl <- guardsList casec parseFi (recSet <|> parseFi)
       do parseFi
          return $ (fmap (Cond) gl) AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty)
          <|> do genNewError follow TokenFI
                 return Nothing

repetition :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
repetition follow recSet =
    do pos <- getPosition
       inv <- invariant parseTokLeftBound
       bou <- bound parseDo
       do parseDo
          gl <- guardsList CAction parseOd (recSet <|> parseOd)
          do parseOd
             return((fmap (Rept) gl) AP.<*> inv AP.<*> bou AP.<*> (return (toLocation pos)) AP.<*> (return GEmpty))
             <|> do genNewError follow TokenOD
                    return Nothing
          <|> do genNewError follow TokEOFO
                 return Nothing

skip :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
skip follow recSet =
    do  pos <- getPosition
        parseSkip
        return $ return $ Skip (toLocation pos) GEmpty