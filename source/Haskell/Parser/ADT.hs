{-# LANGUAGE NamedFieldPuns #-}
module Parser.ADT
    ( abstractDataType
    , dataType
    ) where

-------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Definition
import           AST.Expression      (Expression (..))
import           AST.Instruction
import           AST.Struct
import           Entry
import           Error
import           Location
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Definition
import           Parser.Instruction
import           Parser.Monad
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Data.List           (intercalate)
import           Data.Foldable       (toList)
import           Control.Monad       (void, when, foldM, zipWithM_, unless)
import           Data.Monoid         ((<>))
import           Data.Maybe          (isNothing)
import qualified Data.Map            as Map
import qualified Data.Sequence       as Seq (fromList, zip, zipWith)
import           Data.Text           (Text, unpack, pack)
import           Text.Megaparsec     (getPosition, optional, (<|>), manyTill,eof)
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------
import Debug.Trace

-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'

abstractDataType :: Parser ()
abstractDataType = do
    from <- getPosition
    match TokAbstract

    abstractName' <- safeIdentifier
    atypes <- do
        t <- optional . parens $ (typeVarDeclaration `sepBy` match TokComma)
        case t of
          Just s -> pure $ toList s
          _ -> pure []

    if isNothing abstractName'
    then 
      void $ anyToken `manyTill` (void (match TokEnd) <|> eof)
    else do 
    
      let Just abstractName = abstractName'
      currentStruct .= Just (abstractName, Nothing, atypes)
      
      match' TokBegin >>= \(Location(p,_)) -> symbolTable %= openScope p

      decls' <- sequence <$> abstractDeclaration `endBy` match' TokSemicolon
      inv'   <- invariant
      procs' <- sequence <$> many procedureDeclaration

      match' TokEnd
      to    <- getPosition
      st <- use symbolTable
      symbolTable %= closeScope to
      let loc = Location (from,to)

      case (decls', inv', procs') of
        (Just decls, Just inv, Just procs) -> do

          let struct = Struct
                  { structName  = abstractName
                  , structDecls = Seq.fromList . zip [0..] . toList $ decls
                  , structProcs = procs
                  , structLoc   = loc
                  , structSt    = st
                  , structTypes = atypes
                  , struct'     = AbstractDataType inv
                  }
          dataTypes %= Map.insert abstractName struct
        _ -> pure ()
      typesVars .= []
      currentStruct .= Nothing

-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Parser ()
dataType = do
  from <- getPosition

  match TokType
  name' <- safeIdentifier
  types <- do
      t <- optional . parens $ typeVarDeclaration `sepBy` match TokComma
      case t of
        Just s -> pure $ toList s
        _ -> pure []
        
  match' TokImplements
  abstractName' <- safeIdentifier
  absTypes <- do
      t <- optional . parens $ (typeVar <|> basicType) `sepBy` match TokComma
      case t of
        Just s -> pure $ toList s
        _ -> pure []

  if isNothing name' || isNothing abstractName'
    then 
      void $ anyToken `manyTill` (void (match TokEnd) <|>  eof)
    else do 
      let 
        Just name         = name'
        Just abstractName = abstractName'

      currentStruct .= Just (name, abstractName', types)

      match' TokBegin

      symbolTable %= openScope from
      decls'   <- sequence <$> (polymorphicDeclaration `endBy` match' TokSemicolon)
      repinv'  <- repInv
      coupinv' <- coupInv

      getPosition >>= \pos -> symbolTable %= closeScope pos

      getPosition >>= \pos -> symbolTable %= openScope pos

      currentStruct .= Just (name, Nothing, types)
      procs'   <- sequence <$> many polymorphicProcedure
      currentStruct .= Just (name, abstractName', types)

      match' TokEnd
      to <- getPosition
      st <- use symbolTable

      symbolTable %= closeScope to

      
      abstractAST <- getStruct abstractName

      case abstractAST  of
        Nothing -> putError from . UnknownError $
                      "Abstract Type `" <> show abstractName <>
                      "` does not exists."

        Just Struct {structTypes, structDecls, structProcs, struct'} -> do

          case (procs', decls', repinv', coupinv') of

            (Just procs, Just decls, Just repinv, Just coupinv) -> do

              let
                lenNeeded = length structTypes
                lenActual = length absTypes
                loc       = Location(from,to)
                fields    = concat . fmap getFields $ decls
                abstractTypes = Map.fromList $ zip structTypes absTypes 

              -- error $ show abstractTypes

              when (lenNeeded /= lenActual) $ do
                putError from . UnknownError $
                  "Type `" <> unpack name <> "` is implementing `" <>
                  unpack abstractName <> "` with only " <> show lenActual <>
                  " types (" <> intercalate "," (fmap show absTypes) <>
                  ")\n\tbut expected " <> show lenNeeded <> " types (" <>
                  intercalate "," (fmap show structTypes) <> ")"

              mapM_ (checkProc abstractTypes procs abstractName) structProcs

              let
                struct = Struct
                      { structName  = name
                      , structDecls = Seq.fromList . zip [0..] . toList $
                                          fmap snd structDecls <> decls
                      , structSt    = st
                      , structProcs = procs
                      , structLoc   = loc
                      , structTypes = types
                      , struct'     = DataType
                       { abstract = abstractName
                       , abstractTypes
                       , inv = inv struct'
                       , repinv
                       , coupinv }}
              dataTypes %= Map.insert name struct
              typesVars .= []
              currentStruct .= Nothing
            _ -> pure ()

  where
    -- Check if all abstract procedures are defined in the implementation
    checkProc types' procs abstractName abstractProc = do
      let
        name = defName abstractProc
        Location(pos,_) = defLoc abstractProc
     
      ok <- or <$> mapM (abstractProc =-=) procs

      if ok
        then return ()
        else putError pos . UnknownError $
              "The procedure named `" <> unpack name <> "` " <>
              showPos pos <> "` in the Abstract Type `" <> unpack abstractName <>
              "`\n\tneeds to be implemented inside the Type `" <>
              unpack name <> "`"
      where 
        -- Check if the both abstract and the one implementing the abstract procedure,
        -- have the same header
        (=-=) :: Definition 
              -> Definition 
              -> Parser Bool
        abstDef =-= def
          | defName def /= defName abstDef = pure False
          | otherwise = do
              let
                Location(pos1,_) = defLoc abstDef
                Location(pos2,_) = defLoc def
                params1 = toList . abstParams . def' $ abstDef
                params2 = toList . procParams . def' $ def
                
              
              if length params1 == length params2
                then 
                  zipWithM_ (checkParams pos1) params1 params2
                else
                  putError pos2 . UnknownError $
                    "The prodecure `" <> unpack (defName def) <>
                    "` does not match with the one defined at " <>
                    showPos pos1
              pure True

        checkParams :: SourcePos 
                    -> (Text,Type,ArgMode) 
                    -> (Text,Type,ArgMode) 
                    -> Parser ()
        checkParams pos (name1, t1', mode1) (name2, t2, mode2) = do
          when (name1 /= name2) $ do
              putError pos . UnknownError $ 
                "Parameter named `" <> unpack name2 <> 
                "` was declared as `" <> unpack name1 <> "`" 
          
          when (mode1 /= mode2) $ do
              putError pos . UnknownError $ 
                "Parameter named `" <> unpack name2 <> "` has mode " <> 
                show mode2 <>" but expected mode " <> show mode1
          
          let t1 = case t1' of
                  GTypeVar _ -> case t1' `Map.lookup` types' of
                    Nothing -> error $ show t1'
                    Just x -> x
                  _ -> t1'

          when (not $ t1 =:= t2) $ do
            currStruct <- use currentStruct
            
            case (currStruct, t1, t2) of
              -- If t1 and t2 are not the same, maybe its because one of then is implementing the other
              -- (e.g. Dicc implements Diccionario  => Dicc =:= Diccionario)
              (Just (dt, Just adt, _), GDataType n1 _, GDataType n2 _)
                | adt == n1 || dt == n2 -> pure ()

              _ -> do 
                putError pos . UnknownError $ 
                  "Parameter named `" <> unpack name2 <> "` has type " <>
                  show t2 <>" but expected type " <> show t1



