{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}
module Parser.Declarations where

-------------------------------------------------------------------------------
import           AST
import           Contents                       as CO
import           Graciela
import           MyParseError
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           ParserState
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Applicative            (liftA2)
import           Control.Monad                  (liftM4)
import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Identity
import qualified Data.Text                      as T
import           Text.Megaparsec
import           Text.Megaparsec.Pos            (SourcePos)
-------------------------------------------------------------------------------
-- | Se encarga del parseo de las variables y su almacenamiento en la tabla de simbolos.
decList :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
decList follow recSet =
    do pos <- getPosition
       do parseVar
          idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
          do parseColon
             t <- myType parseSemicolon recSet
             addManyUniSymParser idl t
             do parseSemicolon
                rl <- decList follow recSet
                return $ liftA2 (++) (fmap (map (\(name,pos') -> AST.Id pos' name t)) idl) rl
                <|> do genNewError follow SColon
                       return Nothing
             <|> do parseAssign
                    lexp <- consListParser parseColon (parseColon <|> recSet)
                    do parseColon
                       t <- myType parseSemicolon recSet
                       addManySymParser CO.Variable idl t lexp
                       do parseSemicolon
                          rl <- decList follow recSet
                          let idlist = fmap (map (\(id, pos) -> Id pos id t)) idl
                          return $ liftA2 (:) (liftM4 LAssign idlist lexp (return pos) (return GEmpty)) rl
                          <|> do genNewError follow SColon
                                 return Nothing
                       <|> do genNewError follow Colon
                              return Nothing
             <|> do genNewError follow AssignOrColon
                    return Nothing
          <|> do parseConst
                 idl <- idList parseAssign (recSet <|> parseAssign)
                 parseAssign
                 lexp <- consListParser parseColon (parseColon <|> recSet)
                 do parseColon
                    t <- myType parseSemicolon recSet
                    addManySymParser CO.Constant idl t lexp
                    parseSemicolon
                    rl <- decList follow recSet
                    let idlist = fmap (map (\(id, pos) -> Id pos id t)) idl
                    return $ liftA2 (:) (liftM4 LAssign idlist lexp (return pos) (return GEmpty)) rl
                    <|> do genNewError follow Colon
                           return Nothing

          <|> return $ return []


-- | Se encarga del parseo de una lista de constantes o variables con inicializacion
consListParser :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
consListParser follow recSet =
    do c <- expr (parseComma <|> follow) (parseComma <|> recSet)
       do parseComma
          l <- consListParser follow recSet
          return $ liftA2 (:) c l
          <|> return $ fmap (:[]) c


-- | Se encarga del parseo del Id de una variable
idList :: Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, SourcePos)])
idList follow recSet =
    do lookAhead follow
       genNewEmptyError
       return Nothing
       <|> do ac <- parseId
              pos <- getPosition
              rl <- idListAux follow recSet
              return (fmap ((ac, pos) :) rl)


-- | Se encarga del parseo de la lista de Id's de las variables
idListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, SourcePos)])
idListAux follow recSet = do
    parseComma
    ac <- parseId
    pos <- getPosition
    rl <- idListAux follow recSet
    return (fmap ((ac, pos) :) rl)
    <|> return $ return []


-- | Se encarga del parseo de la lectura de variables
reading :: Graciela Token -> Maybe [AST Type] -> ParsecT [TokenPos] ()
           (StateT GracielaState Identity) (Maybe [AST Type])
reading follow ld =
  do pos <- getPosition
     do parseRead
        do parseLeftParent
           lid <- idList parseTokRightPar parseTokRightPar
           ts <- verifyReadVars lid
           do parseTokRightPar
              do parseWith
                 id <- parseString
                 addFileToReadParser id
                 do parseSemicolon
                    return $ liftA2 (++) ld $ (:[]) <$> liftA2 (Read pos (Just id) ts) lid (return GEmpty)
                    <|> do genNewError follow SColon
                           return Nothing
                 <|> do parseSemicolon
                        return $ liftA2 (++) ld $ (:[]) <$> liftA2 (Read pos Nothing ts) lid (return GEmpty)
                        <|> do genNewError follow SColon
                               return Nothing
              <|> do genNewError follow TokenRP
                     return Nothing
           <|> do genNewError follow TokenLP
                  return Nothing


-- | Verifica las variables utilizadas en la lectura
decListWithRead :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
decListWithRead follow recSet =
  do lookAhead (parseConst <|> parseVar)
     ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
     do lookAhead parseRead
        reading follow ld
        <|> return ld
     <|> do lookAhead parseRead
            reading follow (return [])
     <|> return $ return []
