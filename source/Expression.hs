module Expression where

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
import Error

listExp follow recSet = do  lookAhead follow
                            return (Right [])
                        <|> ( do e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                 lexp  <- listExpAux follow recSet
                                 return(verifyBinError (:) e lexp)
                            )
                        <|> (do err <- genNewError (recSet) (TokenRP)
                                return(Left(return err))
                            )

listExpAux follow recSet = do lookAhead follow
                              return(Right [])
                           <|> ( do parseComma
                                    e  <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                    lexp  <- listExpAux follow recSet
                                    return(verifyBinError (:) e lexp)
                               )
                           <|> (do err <- genNewError (recSet) (TokenRP)
                                   return(Left(return err))
                               )

expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
expr follow recSet =  do lookAhead(follow)
                         pos <- getPosition
                         return (Left (return (newEmptyError pos)))
                      
                      <|> exprPrecLvl' follow recSet

exprPrecLvl' follow recSet = do e <- expr' (follow <|> parseEqual <|> parseNotEqual <|> parseAnd <|> parseOr) (recSet <|> parseEqual <|> parseNotEqual <|> parseAnd <|> parseOr)
                                do pos <- getPosition
                                   do (lookAhead (follow) >> return e)
                                      <|> (parseEqual    AP.*> exprPrecLvl' follow recSet >>= return . (verifyBinError (EquNode (sourceLine pos) (sourceColumn pos)) e))
                                      <|> (parseNotEqual AP.*> exprPrecLvl' follow recSet >>= return . (verifyBinError (IneNode (sourceLine pos) (sourceColumn pos)) e))
                                      <|> (parseAnd      AP.*> exprPrecLvl' follow recSet >>= return . (verifyBinError (ConNode (sourceLine pos) (sourceColumn pos)) e))
                                      <|> (parseOr       AP.*> exprPrecLvl' follow recSet >>= return . (verifyBinError (DisNode (sourceLine pos) (sourceColumn pos)) e))
                                      <|> (genNewError (recSet) (Operator) >>= return . (checkError e))
 
expr' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
expr' follow recSet =  do t <- term' (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
                          do pos <- getPosition
                             do (lookAhead(follow) >> return t)
                                <|> (parsePlus  AP.*> expr' follow recSet >>=  return . (verifyBinError (SumNode (sourceLine pos) (sourceColumn pos)) t))
                                <|> (parseMinus AP.*> expr' follow recSet >>=  return . (verifyBinError (SubNode (sourceLine pos) (sourceColumn pos)) t))
                                <|> (genNewError (recSet) (Operator) >>= return . (checkError t))

term' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
term' follow recSet = do p <- factor (follow <|> parseSlash <|> parseStar) (recSet <|> parseSlash <|> parseStar)
                         do pos <- getPosition
                            do (lookAhead(follow) >> return p)
                               <|> (parseSlash AP.*> term' follow recSet >>= return . (verifyBinError (DivNode (sourceLine pos) (sourceColumn pos)) p))
                               <|> (parseStar  AP.*> term' follow recSet >>= return . (verifyBinError (MulNode (sourceLine pos) (sourceColumn pos)) p))
                               <|> (genNewError (recSet) (Operator) >>= return . (checkError p))

factor follow recSet = do p <- factor' (follow <|> parseTokAccent) (recSet <|> parseTokAccent)
                          do pos <- getPosition
                             do  (lookAhead(follow) >> return p)
                                 <|> (parseTokAccent AP.*> factor follow recSet >>= return . (verifyBinError (ExpNode (sourceLine pos) (sourceColumn pos)) p))
                                 <|> (genNewError (recSet) (Operator) >>= return . (checkError p))
                             
{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}

factor' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
factor' follow recSet = do do pos <- getPosition
                              do parseLeftParent
                                 e <- expr (parseRightParent) (parseRightParent)
                                 do  try(parseRightParent >>= return . return e)
                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError e))
                              
                                 <|> (number >>= return . Right . IntNode (sourceLine pos) (sourceColumn pos)) 
                                 <|> (do idp <- parseID
                                         do      lookAhead follow AP.*> return(Right(IDNode (sourceLine pos) (sourceColumn pos) idp))
                                             <|> do parseLeftParent
                                                    lexp <- listExp (parseEnd <|> parseRightParent) (recSet <|> parseRightParent)
                                                    (try (do parseRightParent
                                                             return (fmap (\f -> f (sourceLine pos) (sourceColumn pos)) (fmap (FCallNode idp) lexp))
                                                         )
                                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError lexp))
                                                     )
                                             <|> do blist <- bracketsList follow recSet
                                                    return(fmap (ArrCallNode (sourceLine pos) (sourceColumn pos) idp) blist)
                                     )
                                 <|> (parseMaxInt    AP.*> return(Right(MaxIntNode (sourceLine pos) (sourceColumn pos))))
                                 <|> (parseMinInt    AP.*> return(Right(MinIntNode (sourceLine pos) (sourceColumn pos))))
                                 <|> (parseMaxDouble AP.*> return(Right(MaxDouNode (sourceLine pos) (sourceColumn pos))))
                                 <|> (parseMinDouble AP.*> return(Right(MinDouNode (sourceLine pos) (sourceColumn pos))))
                                 <|> (parseBool      >>= return . Right . BoolNode (sourceLine pos) (sourceColumn pos))
                                 <|> (parseChar      >>= return . Right . CharNode (sourceLine pos) (sourceColumn pos))
                                 <|> (parseString    >>= return . Right . StringNode (sourceLine pos) (sourceColumn pos))
                                 <|> (do parseToInt
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (ToIntNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> (do parseToDouble
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (ToDoubleNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> (do parseToString
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (ToStringNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> (do parseToChar
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (ToCharNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> (do parseMinus
                                         e <- expr follow recSet
                                         return(fmap (MinusNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> ( do parseTokAbs
                                          e <- expr follow recSet
                                          return(fmap (AbsNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> ( do parseTokSqrt
                                          e <- expr follow recSet
                                          return(fmap (SqrtNode (sourceLine pos) (sourceColumn pos)) e)
                                     )
                                 <|> (genNewError recSet Number >>= return . Left . return)

bracketsList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
bracketsList follow recSet = do  lookAhead follow
                                 return(Right[])
                             <|> do parseLeftBracket
                                    e <- expr parseRightBracket parseRightBracket
                                    do try( do parseRightBracket
                                               lexp <- bracketsList follow recSet
                                               return(verifyBinError (:) e lexp)
                                               -- FALTA ARREGLAR EL CASO RARO
                                               -- Modemos levantarnos del error con un hazte el loco
                                           )
                                       <|> ( do err <- genNewError (follow <|> parseLeftBracket) (TokenRB)
                                                return(checkError e err)
                                           )
