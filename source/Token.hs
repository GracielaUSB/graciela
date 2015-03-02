module Token where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T


--data Type     = Entero | Flotante | Booleano | Caracter | CadenaChar 
--data TypeBool = Verdadero | Falso 


data Token =   TokPlus
             | TokMinus 
             | TokStar 
             | TokSlash 
             | TokEnd 
             | TokComma 
             | TokLeftParent 
             | TokRightParent 
             | TokProgram    
             | TokPre        
             | TokPost      
             | TokBound 
             | TokFunc      
             | TokProc     
             | TokIn      
             | TokOut 
             | TokInOut       
             | TokWith             
             | TokMod    
             | TokMax         
             | TokMin      
             | TokForall      
             | TokExist    
             | TokNotExist   
             | TokSigma      
             | TokPi          
             | TokUnion      
             | TokIf         
             | TokFi        
             | TokInv       
             | TokDo         
             | TokOd         
             | TokGcd        
             | TokAbs       
             | TokSqrt      
             | TokLength     
             | TokVar      
             | TokConst     
             | TokAbort       
             | TokRandom      
             | TokSkip        
             | TokWrite    
             | TokWriteln
             | TokRead     
             | TokToInt      
             | TokToDouble   
             | TokToChar      
             | TokToString                
             | TokMIN_INT     
             | TokMIN_DOUBLE 
             | TokMAX_INT   
             | TokMAX_DOUBLE 
             | TokBool   -- { nType :: TypeBool }      
             | TokType    --{ nType :: Type     }
             | TokInteger { num   :: Integer  }
             | TokError T.Text
      deriving (Show, Read, Eq)

type TokenPos = (Token, SourcePos)


makeTokenParser x = token showTok posTok testTok
                    where
                      showTok (t, pos) = show t
                      posTok  (t, pos) = pos
                      testTok (t, pos) = if x == t then Just (t) else Nothing

verify :: Token -> Parsec ([TokenPos]) () (Token)
verify token = makeTokenParser token

parsePlus = verify TokPlus
parseMinus = verify TokMinus
parseSlash = verify TokSlash
parseStar = verify TokStar
parseComma = verify TokComma
parseLeftParent = verify TokLeftParent
parseRightParent = verify TokRightParent
parseEnd  = verify TokEnd

parseAnyToken :: Parsec ([TokenPos]) () (Token)
parseAnyToken = token showTok posTok testTok
                where
                  showTok (t, pos) = show t
                  posTok  (t, pos) = pos
                  testTok (t, pos) = Just (t)

number :: Parsec ([TokenPos]) () (Token)
number = token showTok posTok testTok
          where
            showTok (t, pos) = show t
            posTok  (t, pos) = pos
            testTok (t, pos) = case t of
                                { TokInteger n -> Just (TokInteger n)
                                ; otherwise    -> Nothing
                                }
