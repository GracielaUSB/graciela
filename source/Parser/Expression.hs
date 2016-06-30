module Parser.Expression
  ( listExp
  , listExpAux
  , lookaheadExpr
  , followExprLevelRel
  , relaNonEquivOp
  , relaEquivOp
  , expr
  , exprLevel2
  , exprLevel3
  , exprLevel4
  , exprLevel5
  , exprLevel6
  , exprLevel7
  , exprLevel8
  , exprLevel9
  , constant
  , exprLevel10
  , rangeQuantification
  , quantification
  , parseOpCuant
  , bracketsList
  ) where

import qualified Control.Applicative as AP
import MyParseError                  as PE
import ParserState                   as PS
import Text.Parsec
import Parser.TokenParser
import Parser.ParserType
import Location
import Limits
import Token
import State
import Type
import AST


listExp :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
listExp follow recSet =
  do lookAhead lookaheadExpr
     e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
     lexp  <- listExpAux follow recSet
     return $ AP.liftA2 (:) e lexp
     <|> do return $ return []

listExpAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
listExpAux follow recSet =
  do parseComma
     e  <- expr (follow <|> parseComma) (recSet <|> parseComma)
     lexp  <- listExpAux follow recSet
     return(AP.liftA2 (:) e lexp)
     <|> do return $ return []



{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}



followExprLevelRel :: MyParser (Token)
followExprLevelRel = parseTokLT <|> parseTokGT <|> parseTokLE <|> parseTokGE


relaNonEquivOp :: MyParser (Token)
relaNonEquivOp = parseTokLT <|> parseTokLE <|> parseTokGT <|> parseTokGE


relaEquivOp :: MyParser (Token)
relaEquivOp    = parseTokEQ   <|> parseTokNE

expr :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
expr follow recSet =
  do lookAhead follow
     genNewEmptyError
     return Nothing
     <|> exprLevel2 follow recSet

parseOperatorLevel2 =
  do parseTokImplies
     return Implies
     <|> do parseTokConse
            return Conse

exprLevel2 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel2 follow recSet =
  do e  <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse)
     exprLevel2' follow e

exprLevel2' follow e =
  do pos <- getPosition
     do op <- parseOperatorLevel2
        e' <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse)
        return $ AP.liftA3 (Boolean op (toLocation pos)) e e' (return GEmpty)
        <|> return e

exprLevel3 :: MyParser Token -> MyParser (Maybe (AST Type))
exprLevel3 follow =
  do e <- exprLevel4 (follow <|> parseOr)
     exprLevel3' follow e

exprLevel3' follow e =
  do pos <- getPosition
     do parseOr
        e' <- exprLevel4 (follow <|> parseOr)
        return $ AP.liftA3 (Boolean Dis (toLocation pos)) e e' (return GEmpty)
        <|> return e

exprLevel4 :: MyParser Token -> MyParser (Maybe (AST Type))
exprLevel4 follow =
  do e <- exprLevel5 (follow <|>  parseAnd)
     exprLevel4' follow e

exprLevel4' follow e =
  do pos <- getPosition
     do parseAnd
        e' <- exprLevel5 follow
        return $ AP.liftA3 (Boolean Con (toLocation pos)) e e' (return (GEmpty))
        <|> return e


exprLevel5 :: MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel5 follow =
  do e <- exprLevel6 (follow <|> parseTokEQ <|> parseTokNE) (follow <|> parseTokEQ <|> parseTokNE)
     exprLevel5' follow e

parseOperatorLevel5 =
  do parseTokEQ
     return Equ
     <|> do parseTokNE
            return Ine

exprLevel5' follow e =
  do pos <- getPosition
     do op <- parseOperatorLevel5
        e' <- exprLevel6 (follow <|> parseTokEQ <|> parseTokNE) (follow <|> parseTokEQ <|> parseTokNE)
        return $ AP.liftA3 (Relational op (toLocation pos)) e e' (return GEmpty)
        <|> do return e


exprLevel6 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel6 follow recSet =
   do e <- exprLevel7 (follow <|> followExprLevelRel)
      exprLevel6' follow e

parseOperatorLevel6 =
  do parseTokLT
     return Less
     <|> do parseTokLE
            return LEqual
     <|> do parseTokGT
            return Greater
     <|> do parseTokGE
            return GEqual

exprLevel6' follow e =
      do pos <- getPosition
         do op <- parseOperatorLevel6
            e' <- exprLevel7 (follow <|> followExprLevelRel)
            return $ AP.liftA3 (Relational op (toLocation pos)) e e' (return GEmpty)
            <|> return e


exprLevel7 :: MyParser Token -> MyParser (Maybe (AST Type))
exprLevel7 follow =
    do t <- exprLevel8 (follow <|> parsePlus <|> parseMinus) follow
       exprLevel7' follow t

parseOperatorLevel7 =
  do parsePlus
     return Sum
     <|> do parseMinus
            return Sub

exprLevel7' follow e =
  do pos <- getPosition
     do op <- parseOperatorLevel7
        e' <- exprLevel8 (follow <|> parsePlus <|> parseMinus) follow
        r <- exprLevel7' follow e'
        return $ AP.liftA3 (Arithmetic op (toLocation pos)) e r (return GEmpty)
        <|> return e

opLevel8 = parseSlash <|> parseStar <|> parseTokMod <|> parseTokMax <|> parseTokMin

exprLevel8 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel8 follow recSet =
   do p <- exprLevel9 (follow <|> opLevel8)
      exprLevel8' follow p

parseOperatorLevel8 =
  do parseSlash
     return Div
     <|> do parseStar
            return Mul
     <|> do parseTokMod
            return Mod
     <|> do parseTokMax
            return Max
     <|> do parseTokMin
            return Min

exprLevel8' follow e =
  do pos <- getPosition
     do op <- parseOperatorLevel8
        e' <- exprLevel9 (follow <|> opLevel8)
        r <- exprLevel8' follow e'
        return $ AP.liftA3 (Arithmetic op (toLocation pos)) e r (return (GEmpty))
        <|> return e


exprLevel9 :: MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel9 follow =
   do p <- exprLevel10 (follow <|> parseTokPower)
      exprLevel9' follow p

exprLevel9' follow e =
  do pos <- getPosition
     do parseTokPower
        e' <- exprLevel10 (follow <|> parseTokPower)
        r <- exprLevel9' follow e'
        return $ AP.liftA3 (Arithmetic Exp (toLocation pos)) e r (return (GEmpty))
        <|> return e

lookaheadExpr = 
  do  parseLeftParent
  <|> parseMaxDouble
  <|> parseMaxInt
  <|> parseMinDouble
  <|> parseMinInt
  <|> parseMinus
  <|> parseToChar
  <|> parseToDouble
  <|> parseToInt
  <|> parseTokAbs
  <|> parseTokBool
  <|> parseTokChar
  <|> parseTokDouble
  <|> parseTokID
  <|> parseTokLeftPer
  <|> parseTokNot
  <|> parseTokNumber
  <|> parseTokSqrt
  <|> parseTokString
  

constant :: MyParser (Maybe (AST Type))
constant =
  do pos <- getPosition
     do n <- parseDouble
        return $ return $ Float (toLocation pos) n GFloat
        <|> do n <- number
               return $ return $ Int (toLocation pos) n GInt
        <|> do e <- parseBool
               return $ return $ Bool (toLocation pos) e GBool
        <|> do e <- parseChar
               return $ return $ Char (toLocation pos) e GChar
        <|> do e <- parseString
               return $ return $ String (toLocation pos) e GEmpty

exprLevel10 :: MyParser Token -> MyParser (Maybe (AST Type) )
exprLevel10 follow =
   do pos <- getPosition
      do parseLeftParent
         e <- expr (parseRightParent) (parseRightParent)
         do  try(parseRightParent >>= return . return e)
             <|> do genNewError (follow) (TokenRP)
                    return $ Nothing
         <|> do idp <- parseID
                t <- lookUpVarParser idp (toLocation pos)
                do parseLeftParent
                   lexp <- listExp (parseEOF <|> parseRightParent) (follow <|> parseRightParent)
                   do parseRightParent
                      sb <- getCurrentScope
                      return $ AP.liftA2 (FCallExp idp sb (toLocation pos)) lexp (return (GEmpty))
                      <|> do genNewError (follow) (TokenRP)
                             return $ Nothing
                   <|> do lookAhead parseLeftBracket
                          blist  <- bracketsList follow follow
                          return $ (AP.liftA2 (ArrCall (toLocation pos) idp) blist t)
                   <|> do return $ fmap (ID (toLocation pos) idp) t
         <|> do parseMaxInt
                return $ return $ Int (toLocation pos) maxInteger GInt
         <|> do parseMinInt
                return $ return $ Int (toLocation pos) minInteger GInt
         <|> do parseMaxDouble
                return $ return $ Float (toLocation pos) maxDouble GFloat
         <|> do parseMinDouble
                return $ return $ Float (toLocation pos) minDouble GFloat
         <|> do parseToInt
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent
                return(AP.liftA2 (Conversion ToInt (toLocation pos)) e (return (GEmpty)))
         <|> do parseToDouble
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent
                return(AP.liftA2  (Conversion ToDouble (toLocation pos)) e  (return (GEmpty)))
         <|> do parseToChar
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent
                return(AP.liftA2  (Conversion ToChar (toLocation pos)) e (return (GEmpty)))
         <|> do parseMinus
                e <- expr follow follow
                return(AP.liftA2  (Unary Minus (toLocation pos)) e (return (GEmpty)))
         <|> do parseTokAbs
                e <- expr follow follow
                return(AP.liftA2  (Unary Abs (toLocation pos)) e (return (GEmpty)))
         <|> do parseTokSqrt
                e <- expr follow follow
                return(AP.liftA2  (Unary Sqrt (toLocation pos)) e (return (GEmpty)))
         <|> do parseTokNot
                e <- expr follow follow
                return(AP.liftA2  (Unary Not (toLocation pos)) e (return (GEmpty)))
         <|> quantification follow follow
         <|> constant
         <|> do genNewError follow Number
                return $ Nothing


rangeQuantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
rangeQuantification follow recSet =
  do pos <- getPosition
     do lookAhead follow
        return $ return $ EmptyRange (toLocation pos) GBool
        <|> exprLevel3 follow


quantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
quantification follow recSet =
  do pos <- getPosition
     parseTokLeftPer
     do op <- parseOpCuant
        do id <- parseID
           do parseColon
              t <- myType parsePipe (recSet <|> parsePipe)
              newScopeParser
              v <- addCuantVar op id t (toLocation pos)
              do parsePipe
                 r <- rangeQuantification parsePipe (parsePipe <|> recSet)
                 do parsePipe
                    t <- expr parseTokRightPer (recSet <|> parseTokRightPer)
                    exitScopeParser
                    do parseTokRightPer
                       return(AP.liftA3 (Quant op id (toLocation pos)) r t (return (GEmpty)))
                       <|> do genNewError follow RightPer
                              return Nothing
                    <|> do genNewError follow Pipe
                           return Nothing
                 <|> do genNewError follow Pipe
                        return Nothing
              <|> do genNewError follow Colon
                     return Nothing
           <|> do genNewError follow IDError
                  return Nothing
        <|> do genNewError follow Cuant
               return Nothing


parseOpCuant :: MyParser (OpQuant)
parseOpCuant =
       (parseTokExist  >> return Exists)
   <|> (parseTokMax    >> return Maximum)
   <|> (parseTokMin    >> return Minimum)
   <|> (parseTokForall >> return ForAll)
   <|> (parseTokSigma  >> return Summation)
   <|> (parseTokPi     >> return Product)


bracketsList :: MyParser Token -> MyParser Token -> MyParser (Maybe ([AST Type]))
bracketsList follow recSet =
  do parseLeftBracket
     e <- expr parseRightBracket parseRightBracket
     do parseRightBracket
        lexp <- bracketsList follow recSet
        return(AP.liftA2 (:) e lexp)
        <|> do genNewError follow TokenRB
               return $ Nothing
     <|> do return $ return []
