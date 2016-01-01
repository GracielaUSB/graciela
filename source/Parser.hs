module Parser where

import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Control.Monad       as M
import qualified Data.Text           as T
import MyParseError                  as PE
import ParserState                  
import Declarations
import Text.Parsec
import TokenParser
import Expression
import ParserType
import Data.Maybe
import Contents
import Location
import Token
import State
import Type
import AST


data CasesConditional = CExpression | CAction


program :: MyParser (Maybe (AST(Type)))
program = do pos <- getPosition
             newScopeParser
             try ( do parseProgram
                      try ( do id  <- parseID
                               try ( do parseBegin
                                        ast  <- listDefProc parseTokOpenBlock parseTokOpenBlock
                                        lacc <- block parseLexEnd parseLexEnd
                                        try ( do parseLexEnd
                                                 parseEnd
                                                 return (M.liftM3 (AST.Program id (getLocation pos)) ast lacc (return (MyEmpty))) 
                                            )
                                            <|> do genNewError parseEnd PE.LexEnd 
                                                   return Nothing
                                   )
                                   <|> do genNewError parseEnd PE.Begin 
                                          return Nothing
                          )
                          <|> do genNewError parseEnd PE.IDError 
                                 return Nothing
                 )
                 <|> do genNewError parseEnd PE.Program 
                        return Nothing

listDefProc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
listDefProc follow recSet =
    do lookAhead parseEnd
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do pf <- procOrFunc  follow recSet
              rl <- listDefProc follow recSet
              return (AP.liftA2 (:) pf rl)
       <|> do return $ Nothing


procOrFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
procOrFunc follow recSet =  
    do lookAhead parseFunc 
       function (follow <|> parseFunc <|> parseProc) recSet
    <|> do lookAhead parseProc
           proc (follow <|> parseFunc <|> parseProc) recSet
    <|> do genNewError (follow <|> parseFunc <|> parseProc) PE.ProcOrFunc 
           do lookAhead follow
              return Nothing
           <|> do lookAhead (parseProc <|> parseFunc)
                  procOrFunc follow recSet
                  return Nothing


followTypeFunction :: MyParser Token
followTypeFunction = parseTokOpenBlock <|> parseTokLeftBound


function :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
function follow recSet = 
   do pos <- getPosition   
      try ( do parseFunc
               try ( do id <- parseID
                        try ( do parseColon
                                 try ( do parseLeftParent
                                          newScopeParser
                                          lt <- listArgFunc id parseRightParent (recSet <|> parseRightParent)
                                          try ( do parseRightParent
                                                   try ( do parseArrow
                                                            t  <- myType (followTypeFunction) (recSet <|> followTypeFunction)
                                                            sb <- getActualScope
                                                            addFunTypeParser id lt t (getLocation pos) sb
                                                            b  <- functionBody follow follow
                                                            exitScopeParser
                                                            addFunTypeParser id lt t (getLocation pos) sb
                                                            return(M.liftM5 (DefFun id sb (getLocation pos)) b (return t) (Just (EmptyAST MyEmpty)) lt (return (MyEmpty)))
                                                       )
                                                       <|> do genNewError follow PE.Arrow
                                                              return Nothing
                                              )
                                              <|> do genNewError follow PE.TokenRP
                                                     return Nothing
                                     )
                                     <|> do genNewError follow PE.TokenLP
                                            return Nothing
                            )
                            <|> do genNewError follow PE.Colon 
                                   return Nothing
                   )
                   <|> do genNewError follow PE.IDError
                          return Nothing
          )
          <|> do genNewError follow PE.TokenFunc
                 return Nothing
   

listArgFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFunc idf follow recSet = 
    do lookAhead parseEnd
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do ar <- argFunc idf (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgFuncAux idf follow recSet
              return(AP.liftA2 (:) ar rl)


argFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
argFunc idf follow recSet = 
    try ( do id <- parseID
             try ( do parseColon
                      t  <- myType follow follow 
                      pos <- getPosition
                      addFunctionArgParser idf id t (getLocation pos)
                      return $ return (id, t)
                 )
                 <|> do genNewError follow PE.Colon
                        return Nothing
        )
        <|> do genNewError follow PE.IDError
               return Nothing


listArgFuncAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFuncAux idf follow recSet = 
    do lookAhead parseEnd
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> try ( do parseComma
                    ar <- argFunc idf (follow <|> parseComma) (recSet <|> parseComma)
                    rl <- listArgFuncAux idf follow recSet
                    return(AP.liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing
          
proc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
proc follow recSet = 
    do pos <- getPosition
       try ( 
          do parseProc
             try ( 
               do id <- parseID
                  try ( 
                    do parseColon
                       try ( 
                         do parseLeftParent
                            newScopeParser
                            targs <- listArgProc id parseRightParent parseRightParent
                            try ( 
                              do parseRightParent
                                 try ( 
                                   do parseBegin
                                      dl   <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
                                      pre  <- precondition parseTokOpenBlock (recSet <|> parseTokOpenBlock)
                                      la   <- block parseTokLeftPost parseTokLeftPost
                                      post <- postcondition parseLexEnd (recSet <|> parseLexEnd)
                                      try ( 
                                        do parseLexEnd
                                           sb   <- getActualScope
                                           addProcTypeParser id targs (getLocation pos) sb
                                           exitScopeParser
                                           addProcTypeParser id targs (getLocation pos) sb
                                           return $ (M.liftM5 (DefProc id sb) la pre post (Just (EmptyAST MyEmpty)) dl) AP.<*> targs AP.<*> (return MyEmpty)
                                          )
                                          <|> do genNewError follow PE.LexEnd
                                                 return Nothing
                                      )
                                      <|> do genNewError follow PE.Begin
                                             return Nothing
                                 )
                                 <|> do genNewError follow PE.TokenRP
                                        return Nothing
                            )
                            <|> do genNewError follow PE.TokenLP
                                   return Nothing
                       )
                       <|> do genNewError follow PE.Colon
                              return Nothing
                  )
                  <|> do genNewError follow PE.IDError
                         return Nothing
           )
           <|> do genNewError follow PE.ProcOrFunc
                  return Nothing

assertions initial final ty follow =
    try ( 
      do initial
         e <- expr final (follow <|> final)
         try ( 
           do final
              pos <- getPosition
              return $ AP.liftA2 (States ty (getLocation pos)) e (return (MyEmpty))
             )
             <|> do genNewError follow PE.TokenCA
                    return Nothing
         )
         <|> do genNewError follow PE.TokenOA
                return Nothing

precondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
precondition follow recSet = assertions parseTokLeftPre parseTokRightPre Pre follow

postcondition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
postcondition follow recSet = assertions parseTokLeftPost parseTokRightPost Post follow

bound :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
bound follow recSet = assertions parseTokLeftBound parseTokRightBound Bound follow

assertion :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
assertion follow recSet = assertions parseTokLeftA parseTokRightA Assertion follow

invariant :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
invariant follow recSet = assertions parseTokLeftInv parseTokRightInv Invariant follow


listArgProc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProc id follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do lookAhead parseEnd
              return Nothing
       <|> do ar <- arg id (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgProcAux id follow recSet
              return (AP.liftA2 (:) ar rl)


listArgProcAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProcAux id follow recSet = 
    do lookAhead follow
       return $ return []
       <|> do lookAhead parseEnd
              return Nothing
       <|> try ( 
             do parseComma
                ar <- arg id (follow <|> parseComma) (recSet <|> parseComma)
                rl <- listArgProcAux id follow recSet
                return(AP.liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing


argType :: MyParser Token -> MyParser Token -> MyParser (Maybe TypeArg)
argType follow recSet = 
    do lookAhead (parseIn <|> parseOut <|> parseInOut <|> parseRef)
       do parseIn 
          return (return (In))
          <|> do parseOut
                 return (return (Out))
          <|> do parseInOut
                 return (return InOut)
          <|> do parseRef
                 return (return Ref)
    <|> do genNewError follow TokenArg
           return Nothing 


arg :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
arg pid follow recSet = 
    do at <- argType parseTokID (recSet <|> parseTokID)
       try (
         do id <- parseID
            try (
              do parseColon
                 t <- myType follow recSet
                 pos <- getPosition
                 addArgProcParser id pid t (getLocation pos) at
                 return $ return (id, t)
                )
                <|> do genNewError follow PE.Colon
                       return Nothing
           )
           <|> do genNewError follow PE.IDError
                  return Nothing

functionBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
functionBody follow recSet = 
    do pos <- getPosition
       do parseBegin
          cif <- (conditional CExpression parseLexEnd parseLexEnd) <|> (expr parseLexEnd parseLexEnd)
          do parseLexEnd
             return cif
             <|> do genNewError follow PE.LexEnd
                    return Nothing
          <|> do genNewError follow PE.Begin
                 return Nothing


actionsList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
actionsList follow recSet = 
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
            rl <- actionsListAux follow recSet
            return $ AP.liftA2 (:) ac rl

actionsListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
actionsListAux follow recSet = 
  do parseSemicolon
     ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
     rl <- actionsListAux follow recSet
     return (AP.liftA2 (:) ac rl)
     <|> do return $ return []

action :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
action follow recSet = 
    do pos <- getPosition
       do  lookAhead followAction
           actionAux follow recSet
           <|> do lookAhead parseTokLeftA
                  as  <- assertion followAction (followAction <|> recSet)
                  do lookAhead followAction
                     res <- actionAux follow recSet
                     return $ AP.liftA3 (GuardAction (getLocation pos)) as res (return MyEmpty)
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
       st  <- getActualScope
       exitScopeParser
       do parseTokCloseBlock
          return $ (AP.liftA2 (Block (getLocation pos) st) dl la) AP.<*> (return MyEmpty)      
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
                return $ return $ Ran id t (getLocation pos) MyEmpty
                <|> do genNewError follow TokenRP
                       return Nothing
             <|> do genNewError follow IDError
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


guardsList :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
guardsList casec follow recSet = 
    do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
       gl <- guardsListAux casec follow recSet
       return $ AP.liftA2 (:) g gl


guardsListAux :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
guardsListAux casec follow recSet = 
  do parseSepGuards
     g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
     rl <- guardsListAux casec follow recSet
     return $ AP.liftA2 (:) g rl
     <|> do return $ return []
                

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
       do lookAhead parseIf
          a <-(conditional CExpression follow recSet) 
          return (AP.liftA2 (\f -> f (getLocation pos)) (AP.liftA2 GuardExp e a) (return (MyEmpty)))
          <|> do a <- expr follow recSet
                 return (AP.liftA2 (\f -> f (getLocation pos)) (AP.liftA2 GuardExp e a) (return (MyEmpty)))


functionCallOrAssign ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
functionCallOrAssign follow recSet = 
    do pos <- getPosition
       id <- parseID
       do parseLeftParent
          lexp  <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
          do parseRightParent
             sb <- getActualScope
             return $ (fmap (ProcCall id sb (getLocation pos)) lexp) AP.<*> (return MyEmpty)
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
                            do let idast = fmap (ID (getLocation pos) id) t
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (getLocation pos)) (return MyEmpty)
                          otherwise -> 
                            do let idast = (fmap (ArrCall (getLocation pos) id) bl) AP.<*>  t 
                               return $ M.liftM4 LAssign (AP.liftA2 (:) idast rl) le (return (getLocation pos)) (return MyEmpty)
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
                do let ast = fmap (ID (getLocation pos) ac) t
                   return $ AP.liftA2 (:) ast rl
              otherwise -> 
                do let ast = (fmap (ArrCall (getLocation pos) ac) bl) AP.<*>  t 
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
             return $ ((fmap (Write False) e) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty))
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing
   

writeln ::  MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
writeln follow recSet = 
    do pos <- getPosition
       parseWriteln
       do parseLeftParent
          e <- expr parseRightParent (recSet <|> parseRightParent)
          do parseRightParent
             return $ (fmap (Write True) e) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty)
             <|> do genNewError follow TokenRP
                    return Nothing
          <|> do genNewError follow TokenLP
                 return Nothing


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
       do parseFi
          return $ (fmap (Cond) gl) AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty)
          <|> do genNewError follow TokenFI
                 return Nothing 

repetition :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
repetition follow recSet = 
    do pos <- getPosition
       inv <- invariant (parseTokLeftBound) (recSet <|> parseTokLeftBound)
       bou <- bound (parseDo) (parseDo <|> recSet)
       do parseDo
          gl <- guardsList CAction parseOd (recSet <|> parseOd)
          do parseOd
             return((fmap (Rept) gl) AP.<*> inv AP.<*> bou AP.<*> (return (getLocation pos)) AP.<*> (return MyEmpty))
             <|> do genNewError follow TokenOD
                    return Nothing
          <|> do genNewError follow TokenDO
                 return Nothing

skip :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
skip follow recSet = 
    do  pos <- getPosition
        parseSkip
        return $ return $ Skip (getLocation pos) MyEmpty
