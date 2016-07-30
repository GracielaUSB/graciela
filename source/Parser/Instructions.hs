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
import           Text.Megaparsec        hiding (Token)
-------------------------------------------------------------------------------

data CasesConditional = CExpression | CAction


actionsList :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST])
actionsList follow recSet =
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> do ac <- action (follow <|> match TokSemicolon) (recSet <|> match TokSemicolon)
            rl <- actionsListAux follow recSet
            return $ liftA2 (:) ac rl

actionsListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST])
actionsListAux follow recSet =
  do match TokSemicolon
     ac <- action (follow <|> match TokSemicolon) (recSet <|> match TokSemicolon)
     rl <- actionsListAux follow recSet
     return (liftA2 (:) ac rl)
     <|> return $ return []

action :: Graciela Token -> Graciela Token -> Graciela (Maybe AST )
action follow recSet =
    do pos <- getPosition
       do  lookAhead followAction
           actionAux follow recSet
           <|> do lookAhead $ match TokLeftA
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

actionAux :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
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
followAction =  match TokId
            <|> match TokIf
            <|> match TokAbort
            <|> match TokSkip
            <|> match TokOpenBlock
            <|> match TokWrite
            <|> match TokWriteln
            <|> match TokLeftInv
            <|> match TokRandom


block :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
block follow recSet =
    do pos <- getPosition
       match TokOpenBlock
       newScopeParser
       dl  <- decList followAction (recSet <|> followAction)
       la  <- actionsList match TokCloseBlock (match TokCloseBlock <|> recSet)
       st  <- getCurrentScope
       exitScopeParser
       do match TokCloseBlock
          return $ liftA2 (Block pos st) dl la <*> return GEmpty
          <|> do genNewError follow TokenCB
                 return Nothing


random :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
random follow recSet =
    do pos <- getPosition
       match TokRandom
       do match TokLeftPar
          do id  <- identifier
             do match TokRightPar
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


guardsList :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela [AST]
guardsList casec follow recSet =
    do g  <- guard casec (match TokSepGuards <|> follow) (match TokSepGuards <|> recSet)
       gl <- guardsListAux casec follow recSet
       return (g:gl)


guardsListAux :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela [AST]
guardsListAux casec follow recSet =
  do match TokSepGuards
     g  <- guard casec (match TokSepGuards <|> follow) (recSet <|> match TokSepGuards)
     rl <- guardsListAux casec follow recSet
     return $ liftA2 (:) g rl
     <|> return $ return []


guard :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela AST
guard CAction follow recSet =
    do posFrom <- getPosition
       e <- expression
       match TokArrow
       a <- action follow recSet
       posTo < getPosition
       return $ AST posFrom posTo GEmpty (Guard e a)

guard CExpression follow recSet =
    do posFrom <- getPosition
       e <- expression
       match TokArrow
       do lookAhead (match TokIf)
          a <- conditional CExpression follow recSet
          posTo <- getPosition
          return $ AST posFrom posTo GEmpty (Guard e a)
          <|> do a <- expression
                 posTo <- getPosition
                 return $ AST posFrom posTo GEmpty (Guard e a)




functionCallOrAssign ::  Graciela Token -> Graciela Token -> Graciela (Maybe AST)
functionCallOrAssign follow recSet =
    do pos <- getPosition
       id <- identifier
       do 
          lexp  <- parens (expression `sepBy` match TokComma)
          sb <- getCurrentScope
          return $ fmap (ProcCall id sb pos) lexp <*> return GEmpty
            
        <|> do bl <- brackets (many expression)
               rl <- idAssignListAux (match TokAssign) (recSet <|> match TokAssign)
               t <- lookUpConsParser id
               match TokAssign
               do le <- many expression
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

idAssignListAux :: Graciela Token -> Graciela Token -> Graciela [AST]
idAssignListAux follow recSet =
  do match TokComma
     pos <- getPosition
     do ac <- identifier
        t  <- lookUpConsParser ac
        bl <- brackets (many expression)
        rl <- idAssignListAux follow recSet
        case bl of
          Nothing  -> return []
          Just bl' ->
            case bl' of
              [] -> do 
                posTo <- getPosition
                let  t' = case t of; Just x -> x; Nothing -> GEmpty
                let ast = AST posFrom posTo t' (Id ac)
                return (ast : rl)
              _  -> do 
                posTo <- getPosition
                let  t' = case t of; Just x -> x; Nothing -> GEmpty
                let ast = AST posFrom posTo t' (ArrCall ac bl)
                return (ast : rl)
        <|> do genNewError follow IdError
               return []
     <|> return []

write :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
write follow recSet = write' False follow

writeln :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
writeln follow recSet = write' True follow

write' ::  Bool -> Graciela Token -> Graciela (Maybe AST )
write' ln follow =
    do posFrom <- getPosition
       match TokWriteln
       do match TokLeftPar
          e <- expression
          do match TokRightPar
             posTo <- getPosition
             return $ Just $ AST posFrom posTo GEmpty (Write ln e) 
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing

new :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
new follow recSet = do
    posFrom <- getPosition
    match TokNew
    match TokLeftPar
    id <- identifier
    match TokRightPar
    posTo <- getPosition
    return $ Just $ AST posFrom posTo GEmpty (New id) 

free :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
free follow recSet = do
    posFrom <- getPosition
    match TokFree
    match TokLeftPar
    id <- identifier
    match TokRightPar
    posTo <- getPosition
    return $ Just $ AST posFrom posTo GEmpty (Free id) 

abort ::  Graciela Token -> Graciela Token -> Graciela (Maybe AST )
abort folow recSet =
    do pos <- getPosition
       match TokAbort
       return $ return $ AST pos pos GEmpty Abort


conditional :: CasesConditional -> Graciela Token -> Graciela Token -> Graciela (Maybe AST )
conditional casec follow recSet =
    do posFrom <- getPosition
       match TokIf
       gl <- guardsList casec (match TokFi) (recSet <|> match TokFi)
       do match TokFi
          posTo <- getPosition
          return $ Just $ AST posFrom posTo GEmpty (Cond gl)
          <|> do genNewError follow TokenFI
                 return Nothing

repetition :: Graciela Token -> Graciela Token -> Graciela (Maybe AST )
repetition follow recSet =
    do posFrom <- getPosition
       inv <- invariant $ match TokLeftBound
       bou <- bound     $ match TokDo
       do match TokDo
          gl <- guardsList CAction (match TokOd) (recSet <|> match TokOd)
          posTo <- getPosition
          do match TokOd
             return $ Just $ AST posFrom posTo GEmpty (Rept gl inv bou)
             <|> do genNewError follow TokenOD
                    return Nothing
          <|> do genNewError follow TokEOFO
                 return Nothing

skip :: Graciela Token -> Graciela Token -> Graciela (Maybe AST)
skip follow recSet =
    do  pos <- getPosition
        match TokSkip
        return $ return $ AST pos pos GEmpty Skip
