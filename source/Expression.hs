module Expression where

import qualified Control.Applicative as AP
import MyParseError                  as PE
import ParserState                   as PS
import Text.Parsec
import TokenParser
import ParserType
import Location
import Limits
import Token
import State
import Type
import AST


listExp :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
listExp follow recSet = 
  do lookAhead lookaheadExpr
     e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
     lexp  <- listExpAux follow recSet
     return $ AP.liftA2 (:) e lexp
     <|> do return $ return []

listExpAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
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
followExprLevelRel = parseTokLess <|> parseTokGreater <|> parseTokLEqual <|> parseTokGEqual


relaNonEquivOp :: MyParser (Token)
relaNonEquivOp = parseTokLess <|> parseTokLEqual <|> parseTokGreater <|> parseTokGEqual


relaEquivOp:: MyParser (Token)
relaEquivOp    = parseEqual   <|> parseNotEqual

expr :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
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

exprLevel2 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel2 follow recSet =
  do e  <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse)
     exprLevel2' follow e
      
exprLevel2' follow e =
  do pos <- getPosition 
     do op <- parseOperatorLevel2
        e' <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse)
        return $ AP.liftA3 (Boolean op (getLocation pos)) e e' (return MyEmpty)
        <|> return e

exprLevel3 :: MyParser Token -> MyParser (Maybe (AST Type))
exprLevel3 follow = 
  do e <- exprLevel4 (follow <|> parseOr)
     exprLevel3' follow e

exprLevel3' follow e =
  do pos <- getPosition
     do parseOr
        e' <- exprLevel4 (follow <|> parseOr)
        return $ AP.liftA3 (Boolean Dis (getLocation pos)) e e' (return MyEmpty)
        <|> return e

exprLevel4 :: MyParser Token -> MyParser (Maybe (AST Type))
exprLevel4 follow = 
  do e <- exprLevel5 (follow <|>  parseAnd)
     exprLevel4' follow e

exprLevel4' follow e =
  do pos <- getPosition
     do parseAnd
        e' <- exprLevel5 follow
        return $ AP.liftA3 (Boolean Con (getLocation pos)) e e' (return (MyEmpty))
        <|> return e


exprLevel5 :: MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel5 follow =
  do e <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (follow <|> parseEqual <|> parseNotEqual)
     exprLevel5' follow e
     
parseOperatorLevel5 =
  do parseEqual
     return Equ
     <|> do parseNotEqual
            return Ine

exprLevel5' follow e = 
  do pos <- getPosition
     do op <- parseOperatorLevel5
        e' <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (follow <|> parseEqual <|> parseNotEqual)
        return $ AP.liftA3 (Relational op (getLocation pos)) e e' (return MyEmpty)
        <|> do return e


exprLevel6 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel6 follow recSet = 
   do e <- exprLevel7 (follow <|> followExprLevelRel)
      exprLevel6' follow e

parseOperatorLevel6 = 
  do parseTokLess
     return Less
     <|> do parseTokLEqual
            return LEqual
     <|> do parseTokGreater
            return Greater
     <|> do parseTokGEqual
            return GEqual

exprLevel6' follow e =
      do pos <- getPosition
         do op <- parseOperatorLevel6
            e' <- exprLevel7 (follow <|> followExprLevelRel)
            return $ AP.liftA3 (Relational op (getLocation pos)) e e' (return MyEmpty)
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
        return $ AP.liftA3 (Arithmetic op (getLocation pos)) e r (return MyEmpty)
        <|> return e

opLevel8 = parseSlash <|> parseStar <|> parseTokMod <|> parseTokMax <|> parseTokMin

exprLevel8 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
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
        return $ AP.liftA3 (Arithmetic op (getLocation pos)) e r (return (MyEmpty))
        <|> return e


exprLevel9 :: MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel9 follow = 
   do p <- exprLevel10 (follow <|> parseTokAccent)
      exprLevel9' follow p

exprLevel9' follow e =
  do pos <- getPosition
     do parseTokAccent
        e' <- exprLevel10 (follow <|> parseTokAccent)
        r <- exprLevel9' follow e'
        return $ AP.liftA3 (Arithmetic Exp (getLocation pos)) e r (return (MyEmpty))
        <|> return e

lookaheadExpr =
  do parseLeftParent
     <|> parseMaxInt
     <|> parseMinInt
     <|> parseMaxDouble
     <|> parseMinDouble
     <|> parseToInt
     <|> parseToDouble
     <|> parseToChar
     <|> parseMinus
     <|> parseTokSqrt
     <|> parseTokAbs
     <|> parseTokNot
     <|> parseTokLeftPer
     <|> parseTokDouble
     <|> parseTokNumber
     <|> parseTokBool
     <|> parseTokChar
     <|> parseTokString
     <|> parseTokID

constant :: MyParser (Maybe (AST Type)) 
constant = 
  do pos <- getPosition
     do n <- parseDouble
        return $ return $ Float (getLocation pos) n MyFloat
        <|> do n <- number
               return $ return $ Int (getLocation pos) n MyInt
        <|> do e <- parseBool
               return $ return $ Bool (getLocation pos) e MyBool
        <|> do e <- parseChar
               return $ return $ Char (getLocation pos) e MyChar     
        <|> do e <- parseString
               return $ return $ String (getLocation pos) e MyEmpty

exprLevel10 :: MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel10 follow =   
   do pos <- getPosition
      do parseLeftParent
         e <- expr (parseRightParent) (parseRightParent)
         do  try(parseRightParent >>= return . return e)
             <|> do genNewError (follow) (TokenRP)
                    return $ Nothing
         <|> do idp <- parseID
                t <- lookUpVarParser idp (getLocation pos)
                do parseLeftParent
                   lexp <- listExp (parseEnd <|> parseRightParent) (follow <|> parseRightParent)
                   do parseRightParent
                      sb <- getActualScope
                      return $ AP.liftA2 (FCallExp idp sb (getLocation pos)) lexp (return (MyEmpty))
                      <|> do genNewError (follow) (TokenRP)
                             return $ Nothing         
                   <|> do lookAhead parseLeftBracket 
                          blist  <- bracketsList follow follow
                          return $ (AP.liftA2 (ArrCall (getLocation pos) idp) blist t)
                   <|> do return $ fmap (ID (getLocation pos) idp) t
         <|> do parseMaxInt
                return $ return $ Int (getLocation pos) maxInteger MyInt
         <|> do parseMinInt
                return $ return $ Int (getLocation pos) minInteger MyInt
         <|> do parseMaxDouble
                return $ return $ Float (getLocation pos) maxDouble MyFloat
         <|> do parseMinDouble
                return $ return $ Float (getLocation pos) minDouble MyFloat
         <|> do parseToInt
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent
                return(AP.liftA2 (Convertion ToInt (getLocation pos)) e (return (MyEmpty)))   
         <|> do parseToDouble
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent 
                return(AP.liftA2  (Convertion ToDouble (getLocation pos)) e  (return (MyEmpty)))    
         <|> do parseToChar
                parseLeftParent
                e <- expr parseRightParent parseRightParent
                parseRightParent
                return(AP.liftA2  (Convertion ToChar (getLocation pos)) e (return (MyEmpty)))  
         <|> do parseMinus
                e <- expr follow follow
                return(AP.liftA2  (Unary Minus (getLocation pos)) e (return (MyEmpty)))
         <|> do parseTokAbs
                e <- expr follow follow
                return(AP.liftA2  (Unary Abs (getLocation pos)) e (return (MyEmpty)))
         <|> do parseTokSqrt
                e <- expr follow follow
                return(AP.liftA2  (Unary Sqrt (getLocation pos)) e (return (MyEmpty)))   
         <|> do parseTokNot
                e <- expr follow follow
                return(AP.liftA2  (Unary Not (getLocation pos)) e (return (MyEmpty)))
         <|> quantification follow follow
         <|> constant
         <|> do genNewError follow Number
                return $ Nothing


rangeQuantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
rangeQuantification follow recSet = 
  do pos <- getPosition
     do lookAhead follow
        return $ return $ EmptyRange (getLocation pos) MyBool
        <|> exprLevel3 follow
            

quantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
quantification follow recSet = 
  do pos <- getPosition
     parseTokLeftPer
     do op <- parseOpCuant
        do id <- parseID
           do parseColon
              t <- myType parsePipe (recSet <|> parsePipe)
              newScopeParser
              v <- addCuantVar op id t (getLocation pos)
              do parsePipe
                 r <- rangeQuantification parsePipe (parsePipe <|> recSet)
                 do parsePipe
                    t <- expr parseTokRightPer (recSet <|> parseTokRightPer)
                    exitScopeParser
                    do parseTokRightPer
                       return(AP.liftA3 (Quant op id (getLocation pos)) r t (return (MyEmpty)))
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


bracketsList :: MyParser Token -> MyParser Token -> MyParser (Maybe ([AST(Type)]))
bracketsList follow recSet = 
  do parseLeftBracket
     e <- expr parseRightBracket parseRightBracket
     do parseRightBracket
        lexp <- bracketsList follow recSet
        return(AP.liftA2 (:) e lexp)
        <|> do genNewError follow TokenRB
               return $ Nothing   
     <|> do return $ return []
