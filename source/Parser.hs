module Parser where

import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Control.Monad       as M
import qualified Data.Text           as T
import MyParseError                  as PE
import ParserState                  
import Declarations
import Text.Parsec
import Data.Either
import TokenParser
import Expression
import ParserType
import Location
import Token
import State
import Type
import AST


data CasesConditional = CExpression | CAction

program :: MyParser (Maybe (AST(Type)))
program = 
    do pos <- getPosition
       newScopeParser
       parseProgram
       id  <- parseID
       parseBegin
       ast  <- listDefProc followListDefProc followListDefProc
       lacc <- actionsList parseLexEnd parseLexEnd
       parseLexEnd
       parseEnd
       return (M.liftM3 (AST.Program id (getLocation pos)) ast lacc (return (MyEmpty))) 


followListDefProc :: MyParser Token
followListDefProc = followAction <|> parseTokLeftA <|> parseTokLeftInv


parseRestInputProgram :: T.Text -> MyParser (Maybe (AST(Type)))
parseRestInputProgram id = 
    do ast  <- listDefProc followListDefProc followListDefProc
       do lookAhead followListDefProc
          do try ( do lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                      parseTokCloseBlock
                      parseEnd
                      pos <- getPosition
                      return (M.liftM3 (AST.Program id (getLocation pos)) ast lacc (return (MyEmpty))) 
                  )
              <|> do genNewError parseEnd ProcOrFunc
                     return $ Nothing
         <|> do genNewError followListDefProc ProcOrFunc
                do lookAhead parseEnd
                   return $ Nothing
                   <|> do lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                          do parseTokCloseBlock
                             parseEnd
                             return $ Nothing
                             <|> do genNewError (parseEnd) (PE.TokenCB)
                                    return $ Nothing


listDefProc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
listDefProc follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do pf <- procOrFunc  follow recSet
              rl <- listDefProc follow recSet
              return (AP.liftA2 (:) pf rl)
       <|> do return $ Nothing


procOrFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
procOrFunc follow recSet =  function follow recSet 
                        <|> proc     follow recSet


followTypeFunction :: MyParser Token
followTypeFunction = parseTokOpenBlock <|> parseTokLeftBound


function :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
function follow recSet = 
   do pos <- getPosition   
      do parseFunc
         do id <- parseID
            do parseColon
               do try ( do parseLeftParent
                           newScopeParser
                           lt <- listArgFunc id parseRightParent (recSet <|> parseRightParent)
                           parseRightParent
                           parseArrow
                           t  <- myType (followTypeFunction) (recSet <|> followTypeFunction)
                           bo <- maybeBound parseBegin (recSet <|> parseBegin)
                           b  <- functionBody parseLexEnd parseLexEnd
                           sb <- getActualScope
                           exitScopeParser
                           addFunTypeParser id lt t (getLocation pos) sb
                           return(M.liftM3 (DefFun id sb (getLocation pos)) b bo (return (MyEmpty)))
                       )
                  <|> do err <- genNewError follow ProcOrFunc
                         return $ Nothing
               <|> do err <- genNewError follow Colon
                      return $ Nothing
            <|> do err <- genNewError follow IDError
                   return $ Nothing


listArgFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFunc idf follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do ar <- argFunc idf (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgFuncAux idf follow recSet
              return(AP.liftA2 (:) ar rl)


argFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
argFunc idf follow recSet = 
    do id <- parseID
       parseColon
       t  <- myType (parseRightParent <|> parseComma)
             (recSet <|> parseRightParent <|> parseComma)
       pos <- getPosition
       addFunctionArgParser idf id t (getLocation pos)
       return $ fmap ((,) id) t


listArgFuncAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFuncAux idf follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do parseComma
              ar <- argFuncAux idf (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgFuncAux idf follow recSet
              return(AP.liftA2 (:) ar rl)
          


argFuncAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
argFuncAux idf follow recSet = 
    do id <- parseID
       parseColon
       t  <- myType (parseComma <|> follow) (parseComma <|> recSet)
       pos <- getPosition
       addFunctionArgParser idf id t (getLocation pos)
       return $ fmap ((,) id) t




proc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
proc follow recSet = 
    do pos <- getPosition
       do parseProc
          id <- parseID
          parseColon
          parseLeftParent
          newScopeParser
          targs <- listArgProc id parseRightParent parseRightParent
          parseRightParent
          parseBegin
          do pre  <- precondition (parseTokOpenBlock <|> parseTokLeftBound) 
                       (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
             b    <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
             parseTokOpenBlock                                  
             la   <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
             parseTokCloseBlock
             post <- postcondition parseLexEnd (recSet <|> parseLexEnd)
             parseLexEnd
             sb   <- getActualScope
             exitScopeParser
             addProcTypeParser id targs (getLocation pos) sb
             return $ (M.liftM5 (DefProc id sb) la pre post b (return [])) AP.<*> (return MyEmpty)

             <|> do dl   <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
                    pre  <- precondition (parseTokOpenBlock <|> parseTokLeftBound) 
                              (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
                    b    <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
                    parseTokOpenBlock
                    la   <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                    parseTokCloseBlock
                    post <- postcondition parseLexEnd (recSet <|> parseLexEnd)
                    parseLexEnd
                    sb   <- getActualScope
                    exitScopeParser
                    addProcTypeParser id targs (getLocation pos) sb
                    return $ (M.liftM5 (DefProc id sb) la pre post b dl) AP.<*> (return MyEmpty)


precondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
precondition follow recSet =  
    do parseTokLeftPre
       e <- expr (parseTokRightPre) (recSet <|> parseTokRightPre)
       parseTokRightPre
       pos <- getPosition
       return(AP.liftA2 (States Pre (getLocation pos)) e (return (MyEmpty)))


postcondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
postcondition follow recSet = 
    do parseTokLeftPost
       e <- expr (parseTokRightPost) (recSet <|> parseTokRightPost)
       parseTokRightPost
       pos <- getPosition
       return(AP.liftA2 (States Post (getLocation pos)) e (return (MyEmpty)))


bound :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
bound follow recSet =
    do pos <- getPosition
       parseTokLeftBound
       e <- expr (parseTokRightBound) (recSet <|> parseTokRightBound)
       parseTokRightBound
       return(AP.liftA2 (States Bound (getLocation pos)) e (return (MyEmpty)))


assertion :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
assertion follow recSet =
    do pos <- getPosition
       parseTokLeftA
       e <- expr (parseTokRightA) (recSet <|> parseTokRightA)
       parseTokRightA
       return(AP.liftA2 (States Assertion (getLocation pos)) e (return (MyEmpty)))


invariant :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
invariant follow recSet = 
    do pos <- getPosition
       parseTokLeftInv
       e <- expr (parseTokRightInv) (recSet <|> parseTokRightInv)
       parseTokRightInv
       return(AP.liftA2 (States Invariant (getLocation pos)) e (return (MyEmpty)))


maybeBound :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) ) 
maybeBound follow recSet =
    do lookAhead follow
       return $ return $ (EmptyAST MyEmpty)
       <|> bound follow recSet


listArgProc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProc id follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do ar <- arg id (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgProcAux id follow recSet
              return (AP.liftA2 (:) ar rl)


listArgProcAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProcAux id follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do parseComma
              ar <- arg id (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgProcAux id follow recSet
              return(AP.liftA2 (:) ar rl)


argType :: MyParser Token -> MyParser Token -> MyParser (Maybe TypeArg)
argType follow recSet = 
    do parseIn 
       return (return (In))
       <|> do parseOut
              return (return (Out))
       <|> do parseInOut
              return (return InOut)


arg :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
arg pid follow recSet = 
    do at <- argType parseTokID (recSet <|> parseTokID)
       id <- parseID
       parseColon
       t <- myType follow recSet
       pos <- getPosition
       addArgProcParser id pid t (getLocation pos) at
       return $ fmap ((,) id) t


functionBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
functionBody follow recSet = 
    do pos <- getPosition
       do parseBegin
          do cif <- conditional CExpression parseLexEnd parseLexEnd
             parseLexEnd
             return (AP.liftA2 (FunBody (getLocation pos)) cif (return MyEmpty))
             <|> do e <- expr parseLexEnd parseLexEnd
                    parseLexEnd
                    return (AP.liftA2 (FunBody (getLocation pos)) e (return (MyEmpty)))


actionsList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
actionsList follow recSet = 
    do lookAhead (follow)
       genNewEmptyError
       return $ Nothing
       <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
              rl <- actionsListAux follow recSet
              return (AP.liftA2 (:) ac rl)
     

actionsListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
actionsListAux follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do parseSemicolon
              ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
              rl <- actionsListAux follow recSet
              return (AP.liftA2 (:) ac rl)
       -- Aqui paso algo raro pero ese no es mi peo
       <|> (return $ Nothing)
                                         

actionAux :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
actionAux follow recSet = 
        skip follow recSet
    <|> conditional CAction follow recSet
    <|> abort follow recSet
    <|> write follow recSet
    <|> writeln follow recSet
    <|> functionCallOrAssign follow recSet
    <|> repetition follow recSet
    <|> random follow recSet
    <|> block follow recSet


followAction ::  MyParser Token
followAction = (parseDo <|> parseTokID <|> parseIf <|> parseAbort <|> parseSkip <|> 
                parseTokOpenBlock <|> parseWrite <|> parseWriteln <|> parseTokLeftInv)


block :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
block follow recSet = 
    do pos <- getPosition
       parseTokOpenBlock
       newScopeParser
       dl  <- decList followAction (recSet <|> followAction)
       la  <- actionsList (parseTokCloseBlock) (parseTokCloseBlock <|> recSet)
       st  <- getActualScope
       exitScopeParser
       parseTokCloseBlock
       return $ (AP.liftA2 (Block (getLocation pos) st) dl la) AP.<*> (return MyEmpty)      


random :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
random follow recSet = 
    do pos <- getPosition
       parseRandom
       id  <- parseID
       lookUpSymbol id
       return $ return $ Ran id (getLocation pos) MyEmpty


guardsList :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
guardsList casec follow recSet = 
    do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
       gl <- guardsListAux casec follow recSet
       return(AP.liftA2 (:) g gl)


guardsListAux :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
guardsListAux casec follow recSet = 
    do lookAhead follow
       return $ return []
  <|>  do parseSepGuards
          g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
          rl <- guardsListAux casec follow recSet
          return (AP.liftA2 (:) g rl)
                

guard :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
guard CAction follow recSet     = 
    do pos <- getPosition
       e <- expr (parseArrow) (recSet <|> parseArrow)
       parseArrow
       a <- action follow recSet
       return (AP.liftA2  (\f -> f (getLocation pos)) (AP.liftA2 Guard e a) (return (MyEmpty)))

guard CExpression follow recSet = 
    do pos <- getPosition
       e <- expr (parseArrow) (recSet <|> parseArrow)
       parseArrow
       a <- expr follow recSet
       return (AP.liftA2 (\f -> f (getLocation pos)) (AP.liftA2 GuardExp e a) (return (MyEmpty)))


functionCallOrAssign ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
functionCallOrAssign follow recSet = 
    do pos <- getPosition
       id <- parseID
       do try (do parseLeftParent
                  lexp  <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
                  do parseRightParent
                     sb <- getActualScope
                     return $ (fmap (ProcCall id sb (getLocation pos)) lexp) AP.<*> (return MyEmpty)
               )
          <|> try ( do t <- lookUpConsParser id
                       bl <- bracketsList (parseComma <|> parseAssign) 
                               (parseComma <|> parseAssign <|> recSet)
                       rl <- idAssignListAux parseAssign (recSet <|> parseAssign)
                       parseAssign
                       le <- listExp follow recSet
                       return $ (AP.liftA2 (LAssign) (AP.liftA2 (:) (AP.liftA2 (,) (fmap ((,) id) t) bl) rl) le) 
                                AP.<*> (return (getLocation pos))
                                AP.<*> (return MyEmpty)
                  )


idAssignListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe ([((T.Text, Type), [AST(Type)])]))
idAssignListAux follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do parseComma
              ac <- parseID
              t  <- lookUpConsParser ac
              bl <- bracketsList (parseComma <|> parseAssign) 
                      (parseComma <|> parseAssign <|> recSet)
              rl <- idAssignListAux (follow) (recSet)
              return ((AP.liftA2 (:) (AP.liftA2 (,) (fmap ((,) ac) t) bl) rl))


action :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
action follow recSet = 
    do pos <- getPosition
       do  as  <- assertion followAction (followAction <|> recSet)
           res <- actionAux follow recSet
           return $ AP.liftA3 (GuardAction (getLocation pos)) as res (return MyEmpty)
           <|> actionAux follow recSet


write :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
write follow recSet = 
    do pos <- getPosition
       parseWrite
       parseLeftParent
       e   <- expr parseRightParent (recSet <|> parseRightParent)
       parseRightParent
       return $ ((fmap (Write False) e) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty))
   

writeln ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
writeln follow recSet = 
    do pos <- getPosition
       parseWriteln
       parseLeftParent
       e <- expr parseRightParent (recSet <|> parseRightParent)
       parseRightParent
       return $ (fmap (Write True) e) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty)


abort ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
abort folow recSet = 
    do pos <- getPosition
       parseAbort
       return $ return $ Abort (getLocation pos) MyEmpty


conditional :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
conditional casec follow recSet = 
    do pos <- getPosition
       parseIf
       gl <- guardsList casec parseFi (recSet <|> parseFi)
       parseFi
       return $ (fmap (Cond) gl) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty)
        

repetition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
repetition follow recSet = 
    do pos <- getPosition
       inv <- invariant (parseTokLeftBound) (recSet <|> parseTokLeftBound)
       bou <- bound (parseDo) (parseDo <|> recSet)
       parseDo
       gl <- guardsList CAction parseOd (recSet <|> parseOd)
       parseOd
       return((fmap (Rept) gl) AP.<*> inv AP.<*> bou AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty))


skip :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
skip follow recSet = 
    do  pos <- getPosition
        parseSkip
        return $ return $ Skip (getLocation pos) MyEmpty
