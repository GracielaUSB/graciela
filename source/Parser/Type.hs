module Parser.Type
    ( basicType
    , type'
    , abstType
    ) where
--------------------------------------------------------------------------------
import           AST.Expression
import           AST.Object      
import           SymbolTable     (lookup)
import           Entry
import           Graciela
import           Parser.Token    (identifier, integerLit, match,parens)
import           Parser.Expression
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad   (void, when)
import           Control.Lens    (use)
import           Data.Text       (Text, unpack)
import           Text.Megaparsec (getPosition, lookAhead, try, (<|>))
import           Text.Megaparsec hiding (Token)
import           Prelude         hiding (lookup)
--------------------------------------------------------------------------------

basicType :: Graciela Type
basicType = do 
  tname <- identifier
  t <- getType tname
  if t `elem` [GBool, GChar, GFloat, GInt]
    then return t
    else do 
      genCustomError ("El tipo `"++unpack tname++"` no es un tipo basico.")
      return GError


type' :: Graciela Type
type' = try arrayOf <|> try type'' <|> userDefined   
  where
    -- Try to parse an array type
    arrayOf = do
        match TokArray
        match TokLeftBracket
        n <- arraySize
        match TokRightBracket
        match TokOf
        t <- type' 
        if t == GError
          then return t
          else return $ GArray n t
    type'' = do
        -- If its not an array, then try with a basic type or a pointer
        tname <- identifier
        t  <- getType tname
        t' <- isPointer t
        when (t' == GError) $ void $genCustomError ("Tipo de variable `"++unpack tname++"` no existe.")
        return t'
    isPointer :: Type -> Graciela Type 
    isPointer t = do
        match TokTimes
        isPointer (GPointer t)
      <|> return t

    -- Or ty if its a user defined Type
    userDefined = do
      id <- identifier
      match TokOf
      t <- type' 
      return (GDataType id [t] [] [])

arraySize :: Graciela Integer
arraySize = do 
  pos <- getPosition
  do  
    notFollowedBy expression
    genCustomError ("No se especifico el tamaño del arreglo.")
    return 0
    <|> do 
      e <- expression
      case exp' e of 
        IntLit value -> return value
        Obj (Object _ _ (Variable name)) -> do 
          st <- use symbolTable 
          case lookup name st of 
            Left _ -> do 
              genCustomError ("La variable `"++unpack name++"` no esta definida.")
              return 0
            Right (Entry _ _ (Const _ (I value))) -> return value
            _ -> do 
              genCustomError ("No se puede declarar un arreglo de tamaño variable.")
              return 0
        _ -> do 
          genCustomError ("El tamaño del arreglo debe definirse usando un numero o una variable constante")
          return 0

abstType :: Graciela Type
abstType =  do {match TokSet;      match TokOf; GSet      <$> basic }
        <|> do {match TokMultiset; match TokOf; GMultiset <$> basic }
        <|> do {match TokSeq;      match TokOf; GSeq      <$> basic }
        <|> do {match TokFunc; ba <- basic; match TokArrow; bb <- basic; return $ GFunc ba bb}
        <|> do {match TokRel;  ba <- basic; match TokBiArrow; bb <- basic; return $ GRel ba bb}
        <|> GTuple <$> parens (basic `sepBy` match TokComma)
        <|> basic

basic :: Graciela Type
basic = GTypeVar <$> identifier
