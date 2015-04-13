module Parser where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import Token
import Lexer
import AST
import Declarations
import Error
import Expression

program :: Parsec [TokenPos] () (Either [MyParseError] AST)
program = do parseProgram
             pos <- getPosition
             id  <- parseID
             parseTokOpenBlock
             do ast  <- listDefProc followAction followAction
                lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                parseTokCloseBlock
                parseEnd
                return ((fmap (ProgramNode id) ast) AP.<*> lacc)

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
                            t <- myType parseTokOpenBlock (recSet <|> parseTokOpenBlock) -- Incompleto
                            b <- functionBody parseTokCloseBlock parseTokCloseBlock
                            return(AP.liftA3 (DefFunNode id) t b lexp)

listArgFunc follow recSet = do lookAhead follow
                               return (Right [])
                               <|> do id <- parseID
                                      parseColon
                                      t  <- myType (parseRightParent <|> parseComma) (recSet <|> parseRightParent <|> parseComma)
                                      rl <- listArgFuncAux follow recSet
                                      return (verifyBinError (:) (fmap (FunArgNode id) t) rl)

listArgFuncAux follow recSet = do lookAhead follow
                                  return (Right [])
                                  <|> do parseComma
                                         id <- parseID
                                         parseColon
                                         t <- myType (parseComma <|> follow) (parseComma <|> recSet)
                                         rl <- listArgFuncAux follow recSet
                                         return (verifyBinError (:) (fmap (FunArgNode id) t) rl)

proc follow recSet = do parseProc
                        id <- parseID
                        parseColon
                        parseLeftParent
                        larg <- listArgProc parseRightParent parseRightParent
                        parseRightParent
                        parseLeftBracket
                        do        parseTokOpenBlock
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  parseRightBracket
                                  return ((fmap (DefProcNode id) la) AP.<*> larg)

                           <|> do dcl <- decListWithRead parseTokOpenBlock (parseTokOpenBlock <|> recSet)
                                  parseTokOpenBlock
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  parseRightBracket
                                  return (((fmap (DefProcDecNode id) la) AP.<*> larg) AP.<*> dcl)

decListWithRead follow recSet = do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
                                   do parseRead
                                      lid <- idList (parseSemicolon <|> parseWith) (recSet <|> parseSemicolon <|> parseWith)
                                      do parseWith
                                         id <- parseString
                                         parseSemicolon
                                         return ((fmap (DecProcReadFileNode id) ld) AP.<*> lid)
                                         <|> return ((fmap (DecProcReadSIONode) ld) AP.<*> lid)
                                      <|> return (fmap (DecProcNode) ld)


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
                           return (Right (ArgTypeNode r))

arg follow recSet = do at <- argType parseID (recSet <|> parseID)
                       id <- parseID
                       parseColon
                       t <- myType follow recSet
                       return ((fmap (ArgNode id) at) AP.<*> t)

functionBody follow recSet = do parseTokOpenBlock
                                e <- expr parseTokCloseBlock parseTokCloseBlock
                                parseTokCloseBlock
                                return (fmap (FunBodyNode) e)

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
                      <|> conditional follow recSet
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
                         return (AP.liftA2 (BlockNode) ld la)
                       
random follow recSet = do parseRandom
                          id <- parseID
                          return(return (RanNode id))

guardsList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
guardsList follow recSet = do g  <- guard follow recSet
                              gl <- guardsListAux follow recSet
                              return(verifyBinError (:) g gl)

guardsListAux :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
guardsListAux follow recSet = do      lookAhead follow
                                      return $ Right []
                                 <|>  do parseSepGuards
                                         g  <- guard (parseSepGuards) (recSet <|> parseSepGuards)
                                         rl <- guardsListAux follow recSet
                                         return (verifyBinError (:) g rl)
                
guard follow recSet = do pos <- getPosition
                         e <- expr (parseArrow) (recSet <|> parseArrow)
                         parseArrow
                         a <- action follow recSet
                         return (fmap (\f -> f (sourceLine pos) (sourceColumn pos)) ((fmap GuardNode e) AP.<*> a))

functionCallOrAssign :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] (Int -> Int -> AST))
functionCallOrAssign follow recSet = do id <- parseID
                                        do try (do parseLeftParent
                                                   lexp <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
                                                   try (do parseRightParent
                                                           return(fmap (FCallNode id) lexp)
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
                                                        return ((fmap (LAssignNode) (AP.liftA2 (:) (fmap ((,) id) bl) rl)) AP.<*> le)
                                                   )
                                
--idAssignList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [Token])
--idAssignList follow recSet = do lookAhead (follow)
--                                pos <- getPosition
--                                return (Left (return (newEmptyError pos)))
--                                <|> do ac <- parseID
--                                       rl <- idAssignListAux follow recSet
--                                       return (fmap (ac:) rl)
                                    
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
                          do  res <- actionAux follow recSet
                              return (fmap (\f -> f (sourceLine pos) (sourceColumn pos)) res)

write follow recSet = do pos <- getPosition
                         parseWrite
                         parseLeftParent
                         e <- expr parseRightParent (recSet <|> parseRightParent)
                         parseRightParent
                         return $ (fmap (WriteNode) e)
           
writeln follow recSet = do pos <- getPosition
                           parseWriteln
                           parseLeftParent
                           e <- expr parseRightParent (recSet <|> parseRightParent)
                           parseRightParent
                           return $ (fmap (WritelnNode) e)

abort = do pos <- getPosition
           parseAbort
           return $ Right $ AbortNode

conditional follow recSet = do pos <- getPosition
                               parseIf
                               gl <- guardsList parseFi (recSet <|> parseFi)
                               parseFi
                               return(fmap (CondNode) gl)
                 
repetition follow recSet = do pos <- getPosition
                              parseDo
                              gl <- guardsList parseOd (recSet <|> parseOd)
                              parseOd
                              return(fmap (ReptNode) gl)
                               
skip :: Parsec [TokenPos] () (Either [MyParseError] (Int -> Int -> AST))
skip = do parseSkip
          return $ Right $ SkipNode           
