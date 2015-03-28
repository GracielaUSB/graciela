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

data MyParseError = MyParseError { line      :: P.Line
                                 , column    :: P.Column
                                 , waitedTok :: WaitedToken
                                 , actualTok :: Token
                                 }
                  | EmptyError   { line    :: P.Line
                                 , column  :: P.Column
                                 }

               deriving (Read)

data WaitedToken =  Operator
                  | Number
                  | TokenRP
                  | TokenRB
                  | Comma
                  | Final
                  deriving(Read)

instance Show WaitedToken where
  show Operator = "operador"
  show Number   = "numero"
  show TokenRP  = "paréntesis derecho"
  show Comma    = "coma"
  show Final    = "final de archivo"
  show TokenRB  = "corchete derecho"
  
instance Show MyParseError where
  show (MyParseError line column wt at) = "Error en la línea " ++ show line ++ ", columna " ++ show column ++ ": Esperaba " ++ show wt ++ " en vez de " ++ show at
  show (EmptyError   line column)       = "Error en la línea " ++ show line ++ ", columna " ++ show column ++ ": No se permiten expresiones vacías"

program = do parseProgram
             parseLeftParent
             lexp <- listExp parseRightParent
             try (do parseRightParent
                     parseEnd
                     return(lexp)
                 )
                 <|> (do err <- genNewError (parseEnd) (Final)
                         parseEnd
                         return(checkError lexp err)
                     )
             
                    
listExp follow = do  lookAhead follow
                     return (Right [])
                 <|> ( do e     <- expr (follow <|> parseComma) (follow <|> parseComma)
                          lexp  <- listExpAux follow 
                          return(verifyBinError (:) e lexp)
                     )
                 <|> (do err <- genNewError (follow) (TokenRP)
                         return(Left(return err))
                     )

listExpAux follow = do lookAhead follow
                       return(Right [])
                    <|> ( do parseComma
                             e  <- expr (follow <|> parseComma) (follow <|> parseComma)
                             lexp  <- listExpAux follow
                             return(verifyBinError (:) e lexp)
                        )
                    <|> (do err <- genNewError (follow) (TokenRP)
                            return(Left(return err))
                        )

expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
expr follow recSet =  do lookAhead(follow)
                         pos <- getPosition
                         return (Left (return (newEmptyError pos)))

                      <|> do t <- term' (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
                             do (lookAhead(follow) >> return t)
                                <|> (parsePlus  AP.*> expr follow recSet >>=  return . (verifyBinError (SumNode) t))
                                <|> (parseMinus AP.*> expr follow recSet >>=  return . (verifyBinError (SubNode) t))
                                <|> (genNewError (recSet) (Operator) >>= return . (checkError t))
                                    
                             <?> "expresion"

term' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
term' follow recSet = do p <- factor (follow <|> parseSlash <|> parseStar) (recSet <|> parseSlash <|> parseStar)
                         do (lookAhead(follow) >> return p)
                            <|> (parseSlash AP.*> term' follow recSet >>= return . (verifyBinError (DivNode) p))
                            <|> (parseStar  AP.*> term' follow recSet >>= return . (verifyBinError (MulNode) p))
                            <|> (genNewError (recSet) (Operator) >>= return . (checkError p))
                            <?> "termino"

{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}

factor :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
factor follow recSet = do do parseLeftParent
                             e <- expr (parseRightParent) (parseRightParent)
                             do  try(parseRightParent >>= return . return e)
                                 <|> (genNewError (recSet) (TokenRP) >>= return . (checkError e))
                          
                          <|> (number >>= return . Right . IntNode) 
                          <|> (do idp <- parseID
                                  do      lookAhead follow AP.*> return(Right(IDNode idp))
                                      <|> do parseLeftParent
                                             lexp <- listExp(parseEnd <|> parseRightParent)
                                             (try (do parseRightParent
                                                      case lexp of
                                                      { Left  listErrors -> return (Left listErrors)
                                                      ; Right listExpr   -> return (Right(FCallNode idp listExpr))
                                                      }
                                                  )
                                              <|> (genNewError (recSet) (TokenRP) >>= return . (checkError lexp))
                                              )
                                      <|> do blist <- bracketsList follow recSet
                                             return(fmap (ArrCallNode idp) blist)
                              )
                          <|> (parseMaxInt    AP.*> return(Right(MaxIntNode)))
                          <|> (parseMinInt    AP.*> return(Right(MinIntNode)))
                          <|> (parseMaxDouble AP.*> return(Right(MaxDouNode)))
                          <|> (parseMinDouble AP.*> return(Right(MinDouNode)))
                          <|> (parseBool      >>= return . Right . BoolNode)
                          <|> (parseChar      >>= return . Right . CharNode)
                          <|> (parseString    >>= return . Right . StringNode)
                          <|> (do parseToInt
                                  parseLeftParent
                                  e <- expr parseRightParent parseRightParent
                                  parseRightParent
                                  case e of
                                  { Left errorList -> return $ Left  $ errorList
                                  ; Right exp      -> return $ Right $ ToIntNode exp
                                  }
                              )
                          <|> (do parseToDouble
                                  parseLeftParent
                                  e <- expr parseRightParent parseRightParent
                                  parseRightParent
                                  case e of
                                  { Left errorList -> return $ Left  $ errorList
                                  ; Right exp      -> return $ Right $ ToDoubleNode exp
                                  }
                              )
                          <|> (do parseToString
                                  parseLeftParent
                                  e <- expr parseRightParent parseRightParent
                                  parseRightParent
                                  case e of
                                  { Left errorList -> return $ Left  $ errorList
                                  ; Right exp      -> return $ Right $ ToStringNode exp
                                  }
                              )
                          <|> (do parseToChar
                                  parseLeftParent
                                  e <- expr parseRightParent parseRightParent
                                  parseRightParent
                                  case e of
                                  { Left errorList -> return $ Left  $ errorList
                                  ; Right exp      -> return $ Right $ ToCharNode exp
                                  }
                              )
                           <|> (do parseMinus
                                   e <- expr follow recSet
                                   return(fmap (MinusNode) e)
                               )
                          <|> (genNewError recSet Number >>= return . Left . return)
                          <?> "numeros"

bracketsList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
bracketsList follow recSet = do  lookAhead follow
                                 return(Right[])
                             <|> do parseLeftBracket
                                    e <- expr parseRightBracket parseRightBracket
                                    do try( do parseRightBracket
                                               lexp <- bracketsList follow recSet
                                               return(verifyBinError (:) e lexp)
                                           )
                                       <|> ( do err <- genNewError (follow <|> parseLeftBracket) (TokenRB)
                                                return(checkError e err)
                                           )


newEmptyError  pos          = EmptyError   { line = P.sourceLine pos, column = P.sourceColumn pos }             
newParseError  msg (e, pos) = MyParseError { line = P.sourceLine pos, column = P.sourceColumn pos, waitedTok = msg, actualTok = e }

genNewError :: Parsec [TokenPos] () (Token) -> WaitedToken -> Parsec [TokenPos] () (MyParseError)
genNewError laset msg = do  pos <- cleanEntry laset
                            return (newParseError msg pos)

cleanEntry :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (TokenPos)
cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(laset) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

panicMode until = manyTill parseAnyToken (lookAhead until)

parseExpr = expr parseEnd

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (xs ++ [s])
checkError _         s = Left [s] 

verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 
