module Parser where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import Location
import Token
import Lexer
import AST
import Declarations
import Error
import Expression

data CasesConditional = CExpression | CAction

program :: Parsec [TokenPos] () (Either [MyParseError] AST)
program = do parseProgram
             pos <- getPosition
             id  <- parseID
             parseTokOpenBlock
             do ast  <- listDefProc (followAction <|> parseTokLeftA <|> parseTokLeftInv) (followAction <|> parseTokLeftA <|> parseTokLeftInv)
                lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                parseTokCloseBlock
                parseEnd
                return ((fmap (Program id) ast) AP.<*> lacc)

listDefProc follow recSet = do lookAhead follow
                               return (Right [])
                               <|> do pf <- procOrFunc  follow recSet
                                      rl <- listDefProc follow recSet
                                      return (verifyBinError (:) pf rl)

procOrFunc follow recSet =  do     function follow recSet 
                               <|> proc     follow recSet

function follow recSet = do parseFunc
                            id <- parseID
                            parseColon
                            parseLeftParent
                            lexp <- listArgFunc parseRightParent (recSet <|> parseRightParent)
                            parseRightParent
                            parseArrow
                            t  <- myType (parseTokOpenBlock <|> parseTokLeftBound) (recSet <|> parseTokOpenBlock <|> parseTokLeftBound)
                            bo <- bound parseTokOpenBlock (recSet <|> parseTokOpenBlock)
                            b  <- functionBody parseTokCloseBlock parseTokCloseBlock
                            return((AP.liftA3 (DefFun id) t b lexp) AP.<*> bo)

listArgFunc follow recSet = do lookAhead follow
                               return (Right [])
                               <|> do id <- parseID
                                      parseColon
                                      t  <- myType (parseRightParent <|> parseComma) (recSet <|> parseRightParent <|> parseComma)
                                      rl <- listArgFuncAux follow recSet
                                      return (verifyBinError (:) (fmap (FunArg id) t) rl)

listArgFuncAux follow recSet = do lookAhead follow
                                  return (Right [])
                                  <|> do parseComma
                                         id <- parseID
                                         parseColon
                                         t <- myType (parseComma <|> follow) (parseComma <|> recSet)
                                         rl <- listArgFuncAux follow recSet
                                         return (verifyBinError (:) (fmap (FunArg id) t) rl)

proc follow recSet = do parseProc
                        id <- parseID
                        parseColon
                        parseLeftParent
                        larg <- listArgProc parseRightParent parseRightParent
                        parseRightParent
                        parseLeftBracket
                        do        pre <- precondition (parseTokOpenBlock <|> parseTokLeftBound) (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
                                  b   <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
                                  parseTokOpenBlock                                  
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  post <- postcondition parseRightBracket (recSet <|> parseRightBracket)
                                  parseRightBracket
                                  return ((fmap (DefProc id) la) AP.<*> larg AP.<*> pre AP.<*> post AP.<*> b)

                           <|> do dcl <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
                                  pre <- precondition (parseTokOpenBlock <|> parseTokLeftBound) (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
                                  b   <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
                                  parseTokOpenBlock
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  post <- postcondition parseRightBracket (recSet <|> parseRightBracket)
                                  parseRightBracket
                                  return ((fmap (DefProcDec id) la) AP.<*> larg AP.<*> dcl AP.<*> pre AP.<*> post AP.<*> b)

precondition follow recSet =  do parseTokLeftPre
                                 e <- listExp (parseTokRightPre) (recSet <|> parseTokRightPre)
                                 parseTokRightPre
                                 return(fmap Precondition e)

postcondition follow recSet =  do parseTokLeftPost
                                  e <- listExp (parseTokRightPost) (recSet <|> parseTokRightPost)
                                  parseTokRightPost
                                  return(fmap Postcondition e)

bound follow recSet =  do parseTokLeftBound
                          e <- listExp (parseTokRightBound) (recSet <|> parseTokRightBound)
                          parseTokRightBound
                          return(fmap Bound e)

assertion follow recSet =  do parseTokLeftA
                              e <- listExp (parseTokRightA) (recSet <|> parseTokRightA)
                              parseTokRightA
                              return(fmap Assertion e)

invariant follow recSet =  do parseTokLeftInv
                              e <- listExp (parseTokRightInv) (recSet <|> parseTokRightInv)
                              parseTokRightInv
                              return(fmap Invariant e)

maybeBound follow recSet = do lookAhead follow
                              return(return(EmptyAST))
                              <|> bound follow recSet

listArgProc follow recSet = do lookAhead follow
                               return (Right [])
                               <|> do ar <- arg (follow <|> parseComma) (recSet <|> parseComma)
                                      rl <- listArgProcAux follow recSet
                                      return (verifyBinError (:) ar rl)

listArgProcAux follow recSet = do lookAhead follow
                                  return (Right [])
                                  <|> do parseComma
                                         ar <- arg (follow <|> parseComma) (recSet <|> parseComma)
                                         rl <- listArgProcAux follow recSet
                                         return (verifyBinError (:) ar rl)

argType follow recSet = do r <- parseIn <|> parseOut <|> parseInOut
                           return (Right (ArgType r))

arg follow recSet = do at <- argType parseID (recSet <|> parseID)
                       id <- parseID
                       parseColon
                       t <- myType follow recSet
                       return ((fmap (Arg id) at) AP.<*> t)

functionBody follow recSet = do pos <- getPosition
                                do parseTokOpenBlock
                                   do cif <- conditional CExpression parseTokCloseBlock parseTokCloseBlock
                                      parseTokCloseBlock
                                      return (fmap (FunBody) (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) cif))
                                      <|> do e <- expr parseTokCloseBlock parseTokCloseBlock
                                             parseTokCloseBlock
                                             return (fmap (FunBody) e)

actionsList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
actionsList follow recSet = do lookAhead (follow)
                               pos <- getPosition
                               return (Left (return (newEmptyError pos)))
                               <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
                                      rl <- actionsListAux follow recSet
                                      return (verifyBinError (:) ac rl)
                                      
actionsListAux :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
actionsListAux follow recSet = do lookAhead follow
                                  return (Right ([]))
                                  <|> do parseSemicolon
                                         ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
                                         rl <- actionsListAux follow recSet
                                         return (verifyBinError (:) ac rl)
                                         
actionAux follow recSet = skip
                      <|> conditional CAction follow recSet
                      <|> abort
                      <|> write follow recSet
                      <|> writeln follow recSet
                      <|> functionCallOrAssign follow recSet
                      <|> repetition follow recSet
                      <|> random follow recSet
                      <|> block follow recSet

followAction = (parseDo <|> parseID <|> parseIf <|> parseAbort <|> parseSkip <|> parseTokOpenBlock <|> parseWrite <|> parseWriteln)

block follow recSet = do parseTokOpenBlock
                         ld <- decList followAction (recSet <|> followAction)
                         la <- actionsList (parseTokCloseBlock) (parseTokCloseBlock <|> recSet)
                         parseTokCloseBlock
                         return (AP.liftA2 (Block) ld la)
                       
random follow recSet = do parseRandom
                          id <- parseID
                          return(return (Ran id))

guardsList casec follow recSet = do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
                                    gl <- guardsListAux casec follow recSet
                                    return(verifyBinError (:) g gl)

guardsListAux casec follow recSet = do      lookAhead follow
                                            return $ Right []
                                       <|>  do parseSepGuards
                                               g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
                                               rl <- guardsListAux casec follow recSet
                                               return (verifyBinError (:) g rl)
                
guard CAction follow recSet = do pos <- getPosition
                                 e <- expr (parseArrow) (recSet <|> parseArrow)
                                 parseArrow
                                 a <- action follow recSet
                                 return (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) ((fmap Guard e) AP.<*> a))

guard CExpression follow recSet = do pos <- getPosition
                                     e <- expr (parseArrow) (recSet <|> parseArrow)
                                     parseArrow
                                     a <- expr follow recSet
                                     return (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) ((fmap GuardExp e) AP.<*> a))

functionCallOrAssign :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] (Location -> AST))
functionCallOrAssign follow recSet = do id <- parseID
                                        do try (do parseLeftParent
                                                   lexp <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
                                                   try (do parseRightParent
                                                           return(fmap (FCall id) lexp)
                                                       )
                                                       <|> (do err <- genNewError (parseEnd) (Final)
                                                               parseEnd
                                                               return(checkError lexp err)
                                                           )
                                                )
                                           <|> try ( do bl <- bracketsList (parseComma <|> parseAssign) (parseComma <|> parseAssign <|> recSet)
                                                        rl <- idAssignListAux parseAssign (recSet <|> parseAssign)
                                                        parseAssign
                                                        le <- listExp follow recSet
                                                        return ((fmap (LAssign) (AP.liftA2 (:) (fmap ((,) id) bl) rl)) AP.<*> le)
                                                   )
                                
idAssignListAux :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [(Token, [AST])])
idAssignListAux follow recSet = do lookAhead follow
                                   return (Right ([]))
                                   <|> do parseComma
                                          ac <- parseID
                                          bl <- bracketsList (parseComma <|> parseAssign) (parseComma <|> parseAssign <|> recSet)
                                          rl <- idAssignListAux (follow) (recSet)
                                          return ((AP.liftA2 (:) (fmap ((,) ac) bl) rl))

action :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
action follow recSet = do pos <- getPosition
                          do  as  <- assertion followAction (followAction <|> recSet)
                              res <- actionAux follow recSet
                              return ((fmap GuardAction as) AP.<*> (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) res))
                              <|> do res <- actionAux follow recSet
                                     return (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) res)

write follow recSet = do pos <- getPosition
                         parseWrite
                         parseLeftParent
                         e <- expr parseRightParent (recSet <|> parseRightParent)
                         parseRightParent
                         return $ (fmap (Write False) e)
           
writeln follow recSet = do pos <- getPosition
                           parseWriteln
                           parseLeftParent
                           e <- expr parseRightParent (recSet <|> parseRightParent)
                           parseRightParent
                           return $ (fmap (Write True) e)

abort = do pos <- getPosition
           parseAbort
           return $ Right $ Abort

conditional casec follow recSet = do pos <- getPosition
                                     parseIf
                                     gl <- guardsList casec parseFi (recSet <|> parseFi)
                                     parseFi
                                     return(fmap (Cond) gl)
                 
repetition follow recSet = do pos <- getPosition
                              inv <- invariant (parseTokLeftBound) (recSet <|> parseTokLeftBound)
                              bou <- bound (parseDo) (parseDo <|> recSet)
                              parseDo
                              gl <- guardsList CAction parseOd (recSet <|> parseOd)
                              parseOd
                              return((fmap (Rept) gl) AP.<*> inv AP.<*> bou)
                               
skip :: Parsec [TokenPos] () (Either [MyParseError] (Location -> AST))
skip = do parseSkip
          return $ Right $ Skip           
