{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}
module Parser.Declaration where

-------------------------------------------------------------------------------
import           AST.Expression                 (Expression(..))
import           AST.Declaration                (Declaration(..))
import           AST.Type
import           AST.Object
import           Entry                          as E
import           Graciela
import           Location
import           Error
import           Parser.Expression
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
-------------------------------------------------------------------------------
import           Control.Lens                   (use, (%=))
import           Control.Monad                  (unless, void, when, zipWithM_)
import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Identity
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, unpack)
import           Text.Megaparsec                (sepBy1,sepBy, (<|>), try, 
                                                 getPosition, notFollowedBy)
import           Prelude                        hiding (lookup)
-------------------------------------------------------------------------------
-- | Se encarga del parseo de las variables y su almacenamiento en la tabla de simbolos.
variableDeclaration :: Graciela Declaration
variableDeclaration = try withAssign <|> withoutAssign
  where
    -- Try to parse if the declared variables are beign assigned
    withAssign = do
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy1` match TokComma
      notFollowedBy (match TokColon)
      withRecovery TokAssign
      exprs <- expression `sepBy1` match TokComma
      match TokColon
      t  <- type'
      to <- getPosition
      let location = Location(from,to)
      let len = length ids == length exprs
      if not len
        then do 
          genCustomError "La cantidad de variables es distinta a la de expresiones"
          return $ BadDeclaration location
        else do 
          zipWithM_ (checkType t) ids exprs
          return $ Declaration { 
                      declLoc   = location
                    , declType  = t
                    , declLvals = fmap fst ids
                    , declExprs = exprs
                    }
    checkType t (id,location) expr = if t == expType expr
      then do
        let entry = Entry 
              { _entryName = id 
              , _loc       = location
              , _info      = Var 
                { _varType  = t
                , _varValue = Just expr
                }
              }
        symbolTable %= insertSymbol id entry
      else genCustomError $
        "Intentando asignar una expresion de tipo `" <>
        show (expType expr) <> "` a una variable de tipo `" <>
        show  t <> "`"
    
    -- If not followed by an Assign token, then just put all the variables in the symbol table 
    withoutAssign = do 
      from <- getPosition
      match TokVar
      ids <- identifierAndLoc `sepBy1` match TokComma
      match TokColon
      t <- type'
      to <- getPosition
      let location = Location(from,to)
      (flip mapM_) ids $ \(id,loc) -> do 
                let entry = Entry {
                      _entryName = id
                    , _loc       = location
                    , _info      = Var t Nothing
                    }
                symbolTable %= insertSymbol id entry
      return Declaration 
        { declLoc   = location
        , declType  = t
        , declLvals = fmap fst ids
        , declExprs = []
        }

constantDeclaration :: Graciela Declaration
constantDeclaration = do
  from <- getPosition
  match TokConst    
  ids <- identifierAndLoc `sepBy1` match TokComma
  withRecovery TokAssign
  exprs <- expression `sepBy1` match TokComma
  withRecovery TokColon
  t  <- basicType
  to <- getPosition
  let location = Location(from,to)
  if t == GUndef
    then do
      genCustomError ("Se intenta declarar constante de tipo `" <>
                       show t <>"`, pero solo pueden ser de tipos basicos.")
      return $ BadDeclaration location
    else do
      -- Check if the length of both, constants and expressions, are the same
      let len = length ids == length exprs
      if not len
        then do 
          genCustomError "La cantidad de constantes es distinta a la de expresiones"
          return $ BadDeclaration location
        else do
          -- Check for each value, if has the correct type
          zipWithM_ (checkType t) ids exprs
          -- Get the ids' text and the assigned expressions
          let ids'  = fmap fst ids
          return $ Declaration 
                    { declLoc   = location
                    , declType  = t 
                    , declLvals = ids'
                    , declExprs = exprs
                    }
  where
    checkType t (id,location) expr = if expType expr == t
      then if constant expr 
        then do
          let entry = Entry 
                      { _entryName = id
                      , _loc       = location
                      , _info      = Var 
                          { _varType  = t
                          , _varValue = Just expr
                          }
                      }
          symbolTable %= insertSymbol id entry
        else 
          genCustomError ("Intentando asignar una expresion que no es constante `" <>
                           show (expType expr)<>"` a la constante `" <> unpack id <> "`")  
      else
        genCustomError ("Intentando asignar una expresion de tipo `" <>
                         show (expType expr)<>"` a una constante de tipo `"<>
                         show t <> "`")

-- Find an identifier and returns it's name and the location
identifierAndLoc :: Graciela (Text, Location)
identifierAndLoc  = do
  from <- getPosition
  id <- identifier
  to <- getPosition
  return (id, Location(from,to))


-- | Verifica las variables utilizadas en la lectura
-- decListWithRead :: Graciela Token -> Graciela [Instruction]
-- decListWithRead follow = do

--   return []
