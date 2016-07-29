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

decList follow recSet = do
      pos <- getPosition
      do match TokVar
          idl <- idList (match TokColon <|> match TokAssign) (recSet <|> match TokColon <|> match TokAssign)
          do match TokColon
             t <- myType (match TokSemicolon) recSet
             addManyUniSymParser idl t
             do match TokSemicolon
                rl <- decList follow recSet
                return $ AP.liftA2 (++) (fmap (map (\(name,pos') -> AST.Id pos' name t)) idl) rl
                <|> do genNewError follow SColon
                       return Nothing
             <|> do match TokAssign
                    lexp <- consListParser (match TokColon) (match TokColon <|> recSet)
                    do match TokColon
                       t <- myType (match TokSemicolon) recSet
                       addManySymParser CO.Variable idl t lexp
                       do (match Semicolon)
                          rl <- decList follow recSet
                          let idlist = fmap (map (\(id, pos) -> Id pos id t)) idl
                          return $ liftA2 (:) (liftM4 LAssign idlist lexp (return pos) (return GEmpty)) rl
                          <|> do genNewError follow SColon
                                 return Nothing
                       <|> do genNewError follow Colon
                              return Nothing
             <|> do genNewError follow AssignOrColon
                    return Nothing
          <|> do match TokConst
                 idl <- idList (match TokAssign) (recSet <|> match TokAssign)
                 match TokAssign
                 lexp <- consListParser (match TokColon) (match TokColon <|> recSet)
                 do match TokColon
                    t <- myType (match TokSemicolon) recSet
                    addManySymParser CO.Constant idl t lexp
                    match TokSemicolon
                    rl <- decList follow recSet
                    let idlist = fmap (map (\(id, pos) -> Id pos id t)) idl
                    return $ liftA2 (:) (liftM4 LAssign idlist lexp (return pos) (return GEmpty)) rl
                    <|> do genNewError follow Colon
                           return Nothing
          <|> return $ return []


-- | Se encarga del parseo de una lista de constantes o variables con inicializacion
consListParser :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST Type])
consListParser follow recSet =
    do c <- expr (match TokComma <|> follow) (match TokComma <|> recSet)
       do match TokComma
          l <- consListParser follow recSet
          return $ liftA2 (:) c l
          <|> return $ fmap (:[]) c


-- | Se encarga del parseo del Id de una variable
idList :: Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, SourcePos)])
idList follow recSet =
    do lookAhead follow
       genNewEmptyError
       return $ Nothing
       <|> do ac <- identifier
              pos <- getPosition
              rl <- idListAux follow recSet
              return (fmap ((ac, pos) :) rl)


-- | Se encarga del parseo de la lista de Id's de las variables
idListAux :: Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, SourcePos)])
idListAux follow recSet = do
    match TokComma
    ac  <- identifier
    pos <- getPosition
    rl  <- idListAux follow recSet
    return (fmap ((ac, pos) :) rl)
    <|> do return $ return []




-- | Se encarga del parseo de la lectura de variables
reading :: Graciela Token -> Maybe [AST Type] -> Graciela (Maybe [AST Type])
reading follow ld = do
     pos <- getPosition
     do (match TokRead)
        do match TokLeftParent
           lid <- idList (match TokRightParent) (match TokRightParent)
           ts  <- verifyReadVars lid
           do match TokRightParent
              do match TokWith
                 id <- match TokString
                 addFileToReadParser id
                 do match TokSemicolon
                    return $ liftA2 (++) ld $ (:[]) <$> liftA2 (Read pos (Just id) ts) lid (return GEmpty)
                    <|> do genNewError follow SColon
                           return Nothing
                 <|> do match TokSemicolon
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
  do lookAhead (match TokConst <|> match TokVar)
     ld <- decList (follow <|> (match TokRead)) (recSet <|> (match TokRead))
     do lookAhead (match TokRead)
        reading follow ld
        <|> do return ld
     <|> do lookAhead (match TokRead)
            reading follow (return [])
     <|> return $ return []
