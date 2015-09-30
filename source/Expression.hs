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
    do  lookAhead follow
        return $ return $ []
    <|> do e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
           lexp  <- listExpAux follow recSet
           return(AP.liftA2 (:) e lexp)
    <|> do err <- genNewError (recSet) (TokenRP)
           return $ Nothing


listExpAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST(Type)])
listExpAux follow recSet = 
   do lookAhead follow
      return $ return $ []
   <|> do parseComma
          e  <- expr (follow <|> parseComma) (recSet <|> parseComma)
          lexp  <- listExpAux follow recSet
          return(AP.liftA2 (:) e lexp)
   <|> do err <- genNewError (recSet) (TokenRP)
          return $ Nothing
                               

expr :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
expr follow recSet =  
  do lookAhead(follow)
     return $ Nothing
  <|> exprLevel1 follow recSet


{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}


constant :: MyParser (Maybe (AST(Type)) ) 
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


followExprLevelRel :: MyParser (Token)
followExprLevelRel = parseTokLess <|> parseTokGreater <|> parseTokLEqual <|> parseTokGEqual


relaNonEquivOp :: MyParser (Token)
relaNonEquivOp = parseTokLess <|> parseTokLEqual <|> parseTokGreater <|> parseTokGEqual


relaEquivOp:: MyParser (Token)
relaEquivOp    = parseEqual   <|> parseNotEqual


verifyNonAsoc :: MyParser Token -> MyParser Token -> MyParser ()
verifyNonAsoc ops recSet = 
   do lookAhead ops
      parseAnyToken
      addNonAsocError
      return ()


exprLevel1 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel1 follow recSet = 
   do e <- exprLevel2 follow recSet
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do return e 
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel2 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel2 follow recSet =
   do e <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse) (recSet <|> parseTokImplies <|> parseTokConse)
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do parseTokImplies
                   e' <- exprLevel2 follow recSet
                   return(AP.liftA3 (Boolean Implies (getLocation pos)) e e' (return (MyEmpty)))                                    
            <|> do parseTokConse
                   e' <- exprLevel2 follow recSet
                   return(AP.liftA3 (Boolean Conse (getLocation pos)) e e' (return (MyEmpty)))      
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel3 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel3 follow recSet = 
   do e <- exprLevel4(follow <|> parseOr) (recSet <|> parseOr)
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do parseOr
                   e' <- exprLevel3 follow recSet
                   return(AP.liftA3 (Boolean Dis (getLocation pos)) e e' (return (MyEmpty))) 
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing
 

exprLevel4 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel4 follow recSet = 
   do e <- exprLevel5 (follow <|>  parseAnd) (recSet <|> parseAnd)
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do parseAnd
                   pos <- getPosition
                   e' <- exprLevel4 follow recSet
                   return(AP.liftA3 (Boolean Con (getLocation pos)) e e' (return (MyEmpty)))
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel5 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel5 follow recSet =
   do e <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do parseEqual
                   e' <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
                   do verifyNonAsoc relaEquivOp recSet 
                      exprLevel5 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational Equ (getLocation pos)) e e' (return (MyEmpty)))
            <|> do parseNotEqual
                   e' <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
                   do verifyNonAsoc relaEquivOp recSet
                      exprLevel5 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational Ine (getLocation pos)) e e' (return (MyEmpty)))              
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel6 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel6 follow recSet = 
   do e <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
      do pos <- getPosition
         do (lookAhead (follow) >> return e)
            <|> do parseTokLess
                   e' <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                   do verifyNonAsoc relaNonEquivOp recSet 
                      exprLevel6 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational Less (getLocation pos)) e e' (return (MyEmpty)))
            <|> do parseTokLEqual
                   e' <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                   do verifyNonAsoc relaNonEquivOp recSet 
                      exprLevel6 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational LEqual (getLocation pos)) e e' (return (MyEmpty)))
            <|> do parseTokGreater
                   e' <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                   do verifyNonAsoc relaNonEquivOp recSet 
                      exprLevel6 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational Greater (getLocation pos)) e e' (return (MyEmpty)))
            <|> do parseTokGEqual
                   e' <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                   do verifyNonAsoc relaNonEquivOp recSet 
                      exprLevel6 follow recSet
                      return Nothing
                      <|> return(AP.liftA3 (Relational GEqual (getLocation pos)) e e' (return (MyEmpty)))
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel7 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel7 follow recSet = 
    do t <- exprLevel8 (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
       do pos <- getPosition
          do (lookAhead(follow) >> return t)
             <|> do parsePlus
                    e <- exprLevel7 follow recSet
                    return $ AP.liftA3 (Arithmetic Sum (getLocation pos)) t e (return (MyEmpty))
             <|> do parseMinus
                    e <- exprLevel7 follow recSet
                    return $ AP.liftA3 (Arithmetic Sub (getLocation pos)) t e (return (MyEmpty))
             <|> do genNewError (recSet) (Operator)
                    return $ Nothing


exprLevel8 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel8 follow recSet = 
   do p <- exprLevel9 (follow <|> parseSlash <|> parseStar <|> parseTokMod <|> parseTokMax <|> parseTokMin) 
                      (recSet <|> parseSlash <|> parseStar <|> parseTokMod <|> parseTokMax <|> parseTokMin)
      do pos <- getPosition
         do (lookAhead(follow) >> return p)
            <|> do parseSlash
                   e <- exprLevel8 follow recSet
                   return $ AP.liftA3 (Arithmetic Div (getLocation pos)) p e (return (MyEmpty))
            <|> do parseStar
                   e <- exprLevel8 follow recSet
                   return $ AP.liftA3 (Arithmetic Mul (getLocation pos)) p e (return (MyEmpty))
            <|> do parseTokMod
                   e <- exprLevel8 follow recSet
                   return $ AP.liftA3 (Arithmetic Mod (getLocation pos)) p e (return (MyEmpty))
            <|> do parseTokMax
                   e <- exprLevel8 follow recSet
                   return $ AP.liftA3 (Arithmetic Max (getLocation pos)) p e (return (MyEmpty))
            <|> do parseTokMin
                   e <- exprLevel8 follow recSet
                   return $ AP.liftA3 (Arithmetic Min (getLocation pos)) p e (return (MyEmpty))
            <|> do genNewError (recSet) (Operator)
                   return $ Nothing


exprLevel9 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel9 follow recSet = 
   do p <- exprLevel10 (follow <|> parseTokAccent) (recSet <|> parseTokAccent)
      do pos <- getPosition
         do  (lookAhead(follow) >> return p)        
             <|> do parseTokAccent
                    e <- exprLevel9 follow recSet
                    return $ AP.liftA3 (Arithmetic Exp (getLocation pos)) p e (return (MyEmpty))
             <|> do genNewError (recSet) (Operator)
                    return $ Nothing
            

exprLevel10 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
exprLevel10 follow recSet =   
   do pos <- getPosition
      do parseLeftParent
         e <- expr (parseRightParent) (parseRightParent)
         do  try(parseRightParent >>= return . return e)
             <|> do genNewError (recSet) (TokenRP)
                    return $ Nothing
         <|> do pos' <- getPosition
                idp <- parseID
                t <- lookUpVarParser idp (getLocation pos)
                do      lookAhead follow
                        return $ fmap (ID (getLocation pos) idp) t
                    <|> do parseLeftParent
                           lexp <- listExp (parseEnd <|> parseRightParent) (recSet <|> parseRightParent)
                           do parseRightParent
                              sb <- getActualScope
                              return $ AP.liftA2 (FCallExp idp sb (getLocation pos)) lexp (return (MyEmpty))
                              <|> do genNewError (recSet) (TokenRP)
                                     return $ Nothing         
                    <|> do blist  <- bracketsList follow recSet
                           return $ (AP.liftA2 (ArrCall (getLocation pos) idp) blist t)
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
                e <- expr follow recSet
                return(AP.liftA2  (Unary Minus (getLocation pos)) e (return (MyEmpty)))
         <|> do parseTokAbs
                e <- expr follow recSet
                return(AP.liftA2  (Unary Abs (getLocation pos)) e (return (MyEmpty)))
         <|> do parseTokSqrt
                e <- expr follow recSet
                return(AP.liftA2  (Unary Sqrt (getLocation pos)) e (return (MyEmpty)))   
         <|> do parseTokNot
                e <- expr follow recSet
                return(AP.liftA2  (Unary Not (getLocation pos)) e (return (MyEmpty)))
         <|> quantification follow recSet
         <|> constant
         <|> do genNewError (recSet) (Number)
                return $ Nothing


rangeQuantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
rangeQuantification follow recSet = 
  do pos <- getPosition
     lookAhead follow
     return $ return $ EmptyRange (getLocation pos) MyBool
     <|> exprLevel3 follow recSet
            

quantification :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST(Type)) )
quantification follow recSet = 
  do pos <- getPosition
     parseTokLeftPer
     op <- parseOpCuant
     id <- parseID
     parseColon
     t <- myType parsePipe (recSet <|> parsePipe)
     newScopeParser
     v <- addCuantVar id t (getLocation pos)
     parsePipe
     r <- rangeQuantification parsePipe (parsePipe <|> recSet)
     parsePipe
     t <- expr(parseTokRightPer) (recSet <|> parseTokRightPer)
     exitScopeParser
     parseTokRightPer
     return(AP.liftA3 (Quant op id (getLocation pos)) r t (return (MyEmpty)))


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
  do lookAhead follow
     return $ return []
     <|> do parseLeftBracket
            e <- expr parseRightBracket parseRightBracket
            do parseRightBracket
               lexp <- bracketsList follow recSet
               return(AP.liftA2 (:) e lexp)
               -- FALTA ARREGLAR EL CASO RARO
               -- Modemos levantarnos del error con un hazte el loco  
               <|> do err <- genNewError (follow <|> parseLeftBracket) (TokenRB)
                      return $ Nothing   

