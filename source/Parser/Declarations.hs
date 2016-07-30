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
import           Entry                       as E
import           Graciela
import           MyParseError
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Parser.State
import           Token
import           Type
import           SourcePos
-------------------------------------------------------------------------------
import           Control.Applicative            (liftA2)
import           Control.Monad                  (liftM4)
import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Identity
import qualified Data.Text                      as T
import           Text.Megaparsec                hiding (Token)
-------------------------------------------------------------------------------
-- | Se encarga del parseo de las variables y su almacenamiento en la tabla de simbolos.
decList :: Graciela Token -> Graciela Token -> Graciela [AST]

decList follow recSet = do
      posFrom <- getPosition
      do  match TokVar
          idl <- idList (match TokColon <|> match TokAssign) (recSet <|> match TokColon <|> match TokAssign)
          do match TokColon
             t <- myType (match TokSemicolon) recSet
             addManyUniSymParser idl t
             do match TokSemicolon
                rl <- decList follow recSet
                let idlist = fmap (\(name,pos) -> AST pos pos GEmpty (Id name)) idl
                return $ idlist ++ rl
                <|> do genNewError follow SColon
                       return []
             <|> do match TokAssign
                    lexp <- consListParser (match TokColon) (match TokColon <|> recSet)
                    do match TokColon
                       t <- myType (match TokSemicolon) recSet
                       addManySymParser E.Variable idl t lexp
                       do match TokSemicolon
                          posTo <- getPosition
                          rl <- decList follow recSet
                          let idlist = fmap (\(name,pos) -> AST pos pos GEmpty (Id name)) idl
                          let ast    = AST posFrom posTo GEmpty (LAssign idlist lexp)
                          return $ ast : rl
                          <|> do genNewError follow SColon
                                 return []
                       <|> do genNewError follow Colon
                              return []
             <|> do genNewError follow AssignOrColon
                    return []
          <|> do match TokConst
                 idl <- idList (match TokAssign) (recSet <|> match TokAssign)
                 match TokAssign
                 lexp <- consListParser (match TokColon) (match TokColon <|> recSet)
                 do match TokColon
                    t <- myType (match TokSemicolon) recSet
                    addManySymParser E.Constant idl t lexp
                    match TokSemicolon
                    posTo <- getPosition
                    rl <- decList follow recSet
                    let idlist = fmap (\(name,pos) -> AST pos pos GEmpty (Id name)) idl
                    let ast    = AST posFrom posTo GEmpty (LAssign idlist lexp)
                    return $ ast : rl
                    <|> do genNewError follow Colon
                           return []
          <|> return []


-- | Se encarga del parseo de una lista de constantes o variables con inicializacion
consListParser :: Graciela Token -> Graciela Token -> Graciela [AST]
consListParser follow recSet =
    do c <- expression
       do match TokComma
          l <- consListParser follow recSet
          -- return (c : l)
          -- c no devuelve AST. Cuando lo haga borrar lo de abajo y descomentar lo de arriba :)
          return []
          undefined
          <|> return [] --[c]


-- | Se encarga del parseo del Id de una variable
idList :: Graciela Token -> Graciela Token -> Graciela [(T.Text, SourcePos)]
idList follow recSet =
    do lookAhead follow
       genNewEmptyError
       return []
       <|> do ac <- identifier
              pos <- getPosition
              rl <- idListAux follow recSet
              return $ (ac, pos) : rl


-- | Se encarga del parseo de la lista de Id's de las variables
idListAux :: Graciela Token -> Graciela Token -> Graciela [(T.Text, SourcePos)]
idListAux follow recSet = do
    match TokComma
    ac  <- identifier
    pos <- getPosition
    rl  <- idListAux follow recSet
    return $ (ac, pos) : rl
    <|> return []




-- | Se encarga del parseo de la lectura de variables
reading :: Graciela Token -> [AST] -> Graciela [AST]
reading follow ld = do
     posFrom <- getPosition
     do match TokRead
        do match TokLeftPar
           lid <- idList (match TokRightPar) (match TokRightPar)
           ts  <- verifyReadVars lid
           do match TokRightPar
              do match TokWith
                 id <- stringLit
                 addFileToReadParser id
                 do match TokSemicolon
                    posTo <- getPosition
                    let ast = [AST posFrom posTo GEmpty (Read (Just id) ts lid)]
                    return $ ld ++ ast
                    <|> do genNewError follow SColon
                           return []
                 <|> do match TokSemicolon
                        posTo <- getPosition
                        let ast = [AST posFrom posTo GEmpty (Read Nothing ts lid)]
                        return $ ld ++ ast
                        <|> do genNewError follow SColon
                               return []
              <|> do genNewError follow TokenRP
                     return []
           <|> do genNewError follow TokenLP
                  return []



-- | Verifica las variables utilizadas en la lectura
decListWithRead :: Graciela Token -> Graciela Token -> Graciela [AST]
decListWithRead follow recSet =
  do lookAhead (match TokConst <|> match TokVar)
     ld <- decList (follow <|> match TokRead) (recSet <|> match TokRead)
     do lookAhead (match TokRead)
        reading follow ld
        <|> return ld
     <|> do lookAhead (match TokRead)
            reading follow []
     <|> return []
