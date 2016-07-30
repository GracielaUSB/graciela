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
import           AST
import           Contents
import           Graciela
import           MyParseError           as PE
import           Parser.Assertions
import           Parser.Declarations
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Parser.State
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Applicative    (liftA2, liftA3)
import           Control.Monad          (liftM4)
import           Control.Monad.Identity (Identity)
import qualified Data.Text              as T
import           Text.Megaparsec
-------------------------------------------------------------------------------

data CasesConditional = CExpression | CAction


actionsList :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
actionsList follow recSet =
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
            rl <- actionsListAux follow recSet
            return $ liftA2 (:) ac rl

actionsListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
actionsListAux follow recSet =
  do parseSemicolon
     ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
     rl <- actionsListAux follow recSet
     return (liftA2 (:) ac rl)
     <|> return $ return []

action :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
action follow recSet =
    do pos <- getPosition
       do  lookAhead followAction
           actionAux follow recSet
           <|> do lookAhead parseTokLeftA
                  as  <- assertion followAction
                  do lookAhead followAction
                     res <- actionAux follow recSet
                     return $ liftA3 (GuardAction pos) as res (return GEmpty)
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
followAction =  parseTokId
            <|> parseIf
            <|> parseAbort
            <|> parseSkip
            <|> parseTokOpenBlock
            <|> parseWrite
            <|> parseWriteln
            <|> parseTokLeftInv
            <|> parseRandom


block :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
block follow recSet =
    do pos <- getPosition
       parseTokOpenBlock
       newScopeParser
       dl  <- decList followAction (recSet <|> followAction)
       la  <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
       st  <- getCurrentScope
       exitScopeParser
       do parseTokCloseBlock
          return $ liftA2 (Block pos st) dl la <*> return GEmpty
          <|> do genNewError follow TokenCB
                 return Nothing


random :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
random follow recSet =
    do pos <- getPosition
       parseRandom
       do parseLeftParent
          do id  <- parseId
             do parseTokRightPar
                cont <- lookUpSymbol id
                case cont of
                  Just (Contents _ _ _ t _ _) ->
                    return $ return $ Ran id t pos GEmpty
                  Just (ArgProcCont _ _ _ t) ->
                    return $ return $ Ran id t pos GEmpty
                  Just (FunctionCon _ _ t _ _) ->
                    return $ return $ Ran id t pos GEmpty
                  _ ->
                    return $ return $ Ran id GEmpty pos GError
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
       return $ liftA2 (:) g gl


guardsListAux :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
guardsListAux casec follow recSet =
  do parseSepGuards
     g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
     rl <- guardsListAux casec follow recSet
     return $ liftA2 (:) g rl
     <|> return $ return []


guard :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
guard CAction follow recSet     =
    do pos <- getPosition
       e <- expr parseArrow (recSet <|> parseArrow)
       parseArrow
       a <- action follow recSet
       return (liftA2  (\f -> f pos) (liftA2 Guard e a) (return GEmpty))

guard CExpression follow recSet =
    do pos <- getPosition
       e <- expr parseArrow (recSet <|> parseArrow)
       parseArrow
       do lookAhead parseIf
          a <- conditional CExpression follow recSet
          return (liftA2 (\f -> f pos) (liftA2 GuardExp e a) (return GEmpty))
          <|> do a <- expr follow recSet
                 return (liftA2 (\f -> f pos) (liftA2 GuardExp e a) (return GEmpty))


functionCallOrAssign ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
functionCallOrAssign follow recSet =
    do pos <- getPosition
       id <- parseId
       do parseLeftParent
          lexp  <- listExp (follow <|> parseTokRightPar) (recSet <|> parseTokRightPar)
          do parseTokRightPar
             sb <- getCurrentScope
             return $ fmap (ProcCall id sb pos) lexp <*> return GEmpty
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
                            do let idast = fmap (Id pos id) t
                               return $ liftM4 LAssign (liftA2 (:) idast rl) le (return pos) (return GEmpty)
                          _  ->
                            do let idast = fmap (ArrCall pos id) bl <*>  t
                               return $ liftM4 LAssign (liftA2 (:) idast rl) le (return pos) (return GEmpty)
                 <|> do genNewError follow TokenAs
                        return Nothing

idAssignListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
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
                do let ast = fmap (Id pos ac) t
                   return $ liftA2 (:) ast rl
              _  ->
                do let ast = fmap (ArrCall pos ac) bl <*>  t
                   return $ liftA2 (:) ast rl
        <|> do genNewError follow IdError
               return Nothing
     <|> return $ return []

write :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
write follow recSet =
    do pos <- getPosition
       parseWrite
       do parseLeftParent
          e   <- expr parseTokRightPar (recSet <|> parseTokRightPar)
          do parseTokRightPar
             return $ fmap (Write False) e <*> return pos <*> return GEmpty
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


writeln ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
writeln follow recSet =
    do pos <- getPosition
       parseWriteln
       do parseLeftParent
          e <- expr parseTokRightPar (recSet <|> parseTokRightPar)
          do parseTokRightPar
             return $ fmap (Write True) e <*> return pos <*> return GEmpty
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing

new :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
new follow recSet = do
    parseNew
    parseLeftParent
    _ <- parseId
    parseTokRightPar

    return . Just . EmptyAST $ GEmpty

free :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
free follow recSet = do
    parseFree
    parseLeftParent
    _ <- parseId
    parseTokRightPar

    return . Just . EmptyAST $ GEmpty

abort ::  Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
abort folow recSet =
    do pos <- getPosition
       parseAbort
       return $ return $ Abort pos GEmpty


conditional :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type) )
conditional casec follow recSet =
    do pos <- getPosition
       parseIf
       gl <- guardsList casec parseFi (recSet <|> parseFi)
       do parseFi
          return $ fmap Cond gl <*> return pos <*> return GEmpty
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
             return $ fmap Rept gl <*> inv <*> bou <*> return pos <*> return GEmpty
             <|> do genNewError follow TokenOD
                    return Nothing
          <|> do genNewError follow TokEOFO
                 return Nothing

skip :: Graciela Token -> Graciela Token -> Graciela (Maybe (AST Type))
skip follow recSet =
    do  pos <- getPosition
        parseSkip
        return $ return $ Skip pos GEmpty
