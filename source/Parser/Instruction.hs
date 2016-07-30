module Parser.Instruction
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
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           ParserState
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


actionsList :: Graciela Token -> Graciela [AST]
actionsList follow =
  do lookAhead follow
     genNewEmptyError
     return []
     <|> do ac <- action (follow <|> match TokSemicolon)
            rl <- actionsListAux follow
            return $ ac : rl

actionsListAux :: Graciela Token -> Graciela [AST]
actionsListAux follow =
  do match TokSemicolon
     ac <- action (follow <|> match TokSemicolon)
     rl <- actionsListAux follow
     return (ac:rl)
     <|> return []

action :: Graciela Token -> Graciela AST
action follow = do 
  posFrom <- getPosition
  do  
    lookAhead followAction
    actionAux follow
    <|> do
          lookAhead  $  match TokLeftA
          assertions <- assertion followAction
          do 
            lookAhead  followAction
            actions <- actionAux follow
            posTo   <- getPosition
            return $ AST posFrom posTo GEmpty (GuardAction assertions actions)
            <|> do  genNewError follow Action
                    return $ AST posFrom posFrom GError (EmptyAST)
         <|> do genNewError follow Action
                return $ AST posFrom posFrom GError (EmptyAST)
     <|> do genNewError follow Action
            return $ AST posFrom posFrom GError (EmptyAST)

actionAux :: Graciela Token -> Graciela AST
actionAux follow =
        skip follow
    <|> conditional CAction follow
    <|> abort follow
    <|> write follow
    <|> writeln follow
    <|> new follow
    <|> free follow
    <|> functionCallOrAssign follow
    <|> random follow
    <|> block follow
    <|> repetition follow


followAction ::  Graciela Token
followAction = identifier >>= \id -> return (TokId id)
            <|> match TokIf
            <|> match TokAbort
            <|> match TokSkip
            <|> match TokOpenBlock
            <|> match TokWrite
            <|> match TokWriteln
            <|> match TokLeftInv
            <|> match TokRandom


block :: Graciela Token -> Graciela AST
block follow =
    do posFrom <- getPosition
       match TokOpenBlock
       newScopeParser
       decls   <- decList followAction
       actions <- actionsList (match TokCloseBlock)
       st      <- getCurrentScope
       exitScopeParser
       do match TokCloseBlock
          posTo <- getPosition
          return $ AST posFrom posTo GEmpty (Block st decls actions)
          <|> do genNewError follow TokenCB
                 return $ AST posFrom posFrom GError (EmptyAST)


random :: Graciela Token -> Graciela AST
random follow =
    do posFrom <- getPosition
       match TokRandom
       do match TokLeftPar
          do id  <- identifier
             do match TokRightPar
                cont <- lookUpSymbol id
                posTo <- getPosition
                case cont of
                  Just (Contents _ _ _ t _ _) ->
                    return $ AST posFrom posTo GEmpty (Ran id t)
                  Just (ArgProcCont _ _ _ t) ->
                    return $ AST posFrom posTo GEmpty (Ran id t)
                  Just (FunctionCon _ _ t _ _) ->
                    return $ AST posFrom posTo GEmpty (Ran id t)
                  _ ->
                    return $ AST posFrom posTo GError (Ran id GError)
                <|> do genNewError follow TokenRP
                       return $ AST posFrom posFrom GError (EmptyAST)
             <|> do genNewError follow IdError
                    return $ AST posFrom posFrom GError (EmptyAST)
          <|> do genNewError follow TokenLP
                 return $ AST posFrom posFrom GError (EmptyAST)


guardsList :: CasesConditional -> Graciela Token -> Graciela [AST]
guardsList casec follow =
    do g  <- guard casec (match TokSepGuards <|> follow)
       gl <- guardsListAux casec follow
       return (g:gl)


guardsListAux :: CasesConditional -> Graciela Token -> Graciela [AST]
guardsListAux casec follow =
  do match TokSepGuards
     g  <- guard casec (match TokSepGuards <|> follow)
     rl <- guardsListAux casec follow
     return (g:rl)
     <|> return []


guard :: CasesConditional -> Graciela Token -> Graciela AST
guard CAction follow =
    do posFrom <- getPosition
       e <- expression
       match TokArrow
       a <- action follow
       posTo <- getPosition
       return $ AST posFrom posTo GEmpty (Guard e a)

guard CExpression follow =
    do posFrom <- getPosition
       e <- expression
       match TokArrow
       do lookAhead (match TokIf)
          a <- conditional CExpression follow
          posTo <- getPosition
          return $ AST posFrom posTo GEmpty (Guard e a)
          <|> do a <- expression
                 posTo <- getPosition
                 return $ AST posFrom posTo GEmpty (Guard e a)




functionCallOrAssign ::  Graciela Token -> Graciela AST
functionCallOrAssign follow =
    do posFrom <- getPosition
       id <- identifier
       do 
          args  <- parens (expression `sepBy` match TokComma)
          sb    <- getCurrentScope
          posTo <- getPosition
          return $ AST posFrom posTo GEmpty (ProcCall id sb args)
        <|> do
              bl    <- many $ brackets expression
              posId <- getPosition
              rl    <- idAssignListAux (match TokAssign)
              t     <- lookUpConsParser id
              match TokAssign
              do 
                le    <- many expression
                posTo <- getPosition
                if null bl
                  then do 
                    let aId = AST posFrom posId t (Id id)
                    return  $ AST posFrom posTo GEmpty (LAssign (aId:rl) le)
                  else do 
                    let aId = AST posFrom posId t (ArrCall id bl)
                    return  $ AST posFrom posTo GEmpty (LAssign (aId:rl) le)
               <|> do genNewError follow TokenAs
                      return $ AST posFrom posFrom GError (EmptyAST)

idAssignListAux :: Graciela Token -> Graciela [AST]
idAssignListAux follow = do 
  match TokComma
  posFrom <- getPosition
  do 
    ac <- identifier
    t  <- lookUpConsParser ac
    bl <- many $ brackets expression
    rl <- idAssignListAux follow
    posTo  <- getPosition
    if null bl
      then do 
        let ast = AST posFrom posTo t (ArrCall ac bl)
        return (ast : rl)    
      else do 
        let ast = AST posFrom posTo t (Id ac)
        return (ast : rl)
            
    <|> do genNewError follow IdError
           return []
  <|> return []

write :: Graciela Token -> Graciela AST
write = write' False

writeln :: Graciela Token -> Graciela AST
writeln = write' True

write' ::  Bool -> Graciela Token -> Graciela AST
write' ln follow =
    do posFrom <- getPosition
       match TokWriteln
       do match TokLeftPar
          e <- expression
          do match TokRightPar
             posTo <- getPosition
             return $ AST posFrom posTo GEmpty (Write ln e) 
             <|> do genNewError follow TokenRP
                    return $ AST posFrom posFrom GError (EmptyAST)
          <|> do genNewError follow TokenLP
                 return $ AST posFrom posFrom GError (EmptyAST)

new :: Graciela Token -> Graciela AST
new follow = do
    posFrom <- getPosition
    match TokNew
    match TokLeftPar
    id <- identifier
    match TokRightPar
    posTo <- getPosition
    return $ AST posFrom posTo GEmpty (New id) 

free :: Graciela Token -> Graciela AST
free follow = do
    posFrom <- getPosition
    match TokFree
    match TokLeftPar
    id <- identifier
    match TokRightPar
    posTo <- getPosition
    return $ AST posFrom posTo GEmpty (Free id) 

abort ::  Graciela Token -> Graciela AST
abort folow =
    do pos <- getPosition
       match TokAbort
       return $ AST pos pos GEmpty Abort


conditional :: CasesConditional -> Graciela Token -> Graciela AST
conditional casec follow =
    do posFrom <- getPosition
       match TokIf
       gl <- guardsList casec (match TokFi)
       do match TokFi
          posTo <- getPosition
          return $ AST posFrom posTo GEmpty (Cond gl)
          <|> do genNewError follow TokenFI
                 return $ AST posFrom posFrom GError (EmptyAST)

repetition :: Graciela Token -> Graciela AST
repetition follow =
    do posFrom <- getPosition
       inv <- invariant $ match TokLeftBound
       bou <- bound     $ match TokDo
       do match TokDo
          gl <- guardsList CAction (match TokOd)
          posTo <- getPosition
          do match TokOd
             return $ AST posFrom posTo GEmpty (Rept gl inv bou)
             <|> do genNewError follow TokenOD
                    return $ AST posFrom posFrom GError (EmptyAST)
          <|> do genNewError follow TokEOFO
                 return $ AST posFrom posFrom GError (EmptyAST)

skip :: Graciela Token -> Graciela AST
skip follow =
    do  pos <- getPosition
        match TokSkip
        return $ AST pos pos GEmpty Skip
