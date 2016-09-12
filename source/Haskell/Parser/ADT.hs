{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           AST.Type
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
--------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=), _Just, over, (.~), _3)
import           Control.Monad       (foldM, unless, void, when, zipWithM_)
import           Data.Array          ((!))
import qualified Data.Array          as Array (listArray)
import           Data.Foldable       (toList)
import           Data.Foldable       as F (concat)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as Map (empty, fromList, insert, lookup,
                                             size)
import           Data.Maybe          (isNothing)
import           Data.Monoid         ((<>))
import           Data.Sequence       (Seq, ViewL(..))
import qualified Data.Sequence       as Seq (fromList, zip, zipWith, empty, viewl)
import           Data.Text           (Text, pack, unpack)
import           Text.Megaparsec     (eof, getPosition, manyTill, optional,
                                      (<|>))
import           Text.Megaparsec.Pos (SourcePos)
-------------------------------------------------------------------------------
import           Debug.Trace

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


    match' TokBegin >>= \(Location(p,_)) -> symbolTable %= openScope p

    let typeArgs = Array.listArray (0, length atypes - 1) atypes

    currentStruct .= Just (abstractName, Nothing, Map.empty, typeArgs, Map.empty)

    decls' <- sequence <$> (abstractDeclaration `endBy` match' TokSemicolon)


    fields <- case decls' of
      Nothing    -> pure Map.empty
      Just decls -> toFields 0 abstractName decls

    currentStruct %= over _Just (_3 .~ fields)

    inv'   <- invariant
    procs' <- sequence <$> many procedureDeclaration

    match' TokEnd
    to    <- getPosition
    st <- use symbolTable
    symbolTable %= closeScope to
    
    let 
      loc = Location (from,to)
      checkProcParam Definition{defName, defLoc, def' } = do
        let 
          Location(pos,_) = defLoc
          AbstractProcedureDef{abstParams} = def'
        case Seq.viewl abstParams of 
          EmptyL -> pure ()
          (_,t,_) :< _ -> do
            let adt = GDataType abstractName Nothing 
            unless (t =:= adt) . putError pos . UnknownError $ 
              "First parameter of procedure `" <> 
              unpack defName <> "` must have type " <> show adt

    case (inv', procs') of
      (Just inv, Just procs) -> do
        mapM_ checkProcParam procs
        let struct = Struct
                { structBaseName   = abstractName
                , structFields = fields
                , structProcs  = Map.fromList $
                  (\d@Definition { defName } -> (defName, d)) <$> toList procs
                , structLoc    = loc
                , structSt     = st
                , structTypes  = atypes
                , struct'      = AbstractDataType inv
                }
        dataTypes %= Map.insert abstractName struct
      _ -> pure ()
    typeVars .= []
    currentStruct .= Nothing
    



toFields :: Integer -> Text 
         -> Seq Declaration 
         -> Parser (Map Text (Integer, Type, Maybe Expression))
toFields n name s = Map.fromList . zipWith f [n..] . toList . F.concat <$> mapM toField' s
  where
    toField' Declaration{ declLoc = Location(pos,_), declType, declIds } = do 
      when (recursiveDecl declType (GDataType name Nothing)) $ 
        putError pos . UnknownError $ "Recursive definition. A Data\
          \ Type cannot contain a field of itself."
      
      pure . zip (toList declIds) $ repeat (declType, Nothing)

    toField' Initialization{ declLoc = Location(pos,_), declType, declPairs } = do
      when (recursiveDecl declType (GDataType name Nothing)) $ 
        putError pos . UnknownError $ "Recursive definition. A Data\
          \ Type cannot contain a field of itself."
      let
        names = fmap fst  (toList declPairs)
        exprs = fmap snd  (toList declPairs)

      pure . zip names $ zip (repeat declType) $ fmap Just exprs

    f n (name, (t,e)) = (name, (n,t,e))


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

      abstractAST <- getStruct abstractName
      case abstractAST of
        Nothing -> do
          putError from . UnknownError $
            "Abstract Type `" <> show abstractName <> "` does not exist."
          void $ anyToken `manyTill` (void (match TokEnd) <|>  eof)

        Just Struct {structTypes, structFields, structProcs, struct'} -> do
          match' TokBegin

          symbolTable %= openScope from
          let typeArgs = Array.listArray (0, length types - 1) types
          currentStruct .= Just (name, abstractName', Map.empty, typeArgs, Map.empty)

          decls' <- sequence <$> (dataTypeDeclaration `endBy` match' TokSemicolon)

          fields' <- case decls' of
            Nothing -> pure Map.empty
            Just decls -> toFields (fromIntegral $ Map.size structFields) name decls

          let
            lenNeeded = length structTypes
            lenActual = length absTypes

            abstractTypes = Array.listArray (0, lenNeeded - 1) absTypes
            
            adtToDt (i,t,e) = (i, f t, e)

            f t = case t of
                GDataType _ _ -> t <> GDataType name abstractName'
                GArray s t -> GArray s (f t)
                GPointer t -> GPointer (f t)
                _ -> t

            fields = fmap adtToDt $ fillTypes abstractTypes structFields <> fields'       
                   
          currentStruct %= over _Just (_3 .~ fields)

          repinv'  <- repInv
          coupinv' <- coupInv

          getPosition >>= \pos -> symbolTable %= closeScope pos
          getPosition >>= \pos -> symbolTable %= openScope pos

          procs'   <- sequence <$> many procedure

          match' TokEnd
          to <- getPosition
          st <- use symbolTable

          symbolTable %= closeScope to
          abstractAST <- getStruct abstractName

          case (procs', decls', repinv', coupinv') of

            (Just procs, Just decls, Just repinv, Just coupinv) -> do
              {- Different number of type arguments -}
              when (lenNeeded /= lenActual) . putError from . UnknownError $
                "Type `" <> unpack name <> "` is implementing `" <>
                unpack abstractName <> "` with only " <> show lenActual <>
                " types (" <> intercalate "," (fmap show absTypes) <>
                ")\n\tbut expected " <> show lenNeeded <> " types (" <>
                intercalate "," (fmap show structTypes) <> ")"

              mapM_ (checkProc abstractTypes procs name abstractName) structProcs

              let
                struct = Struct
                      { structBaseName   = name
                      , structFields = fields
                      , structSt     = st
                      , structProcs  = Map.fromList $
                        (\d@Definition { defName } -> (defName, d)) <$> toList procs
                      , structLoc    = Location(from,to)
                      , structTypes  = types
                      , struct'      = DataType
                        { abstract   = abstractName
                        , abstractTypes
                        , inv = inv struct'
                        , repinv
                        , coupinv }}
              dataTypes %= Map.insert name struct
              typeVars .= []
              currentStruct .= Nothing
            _ -> pure ()

  where
    -- Check if all abstract procedures are defined in the implementation
    checkProc types' procs dtName abstractName abstractProc = do
      let
        name = defName abstractProc
        Location(pos,_) = defLoc abstractProc

      ok <- or <$> mapM (abstractProc =-=) procs

      unless ok .putError pos . UnknownError $
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
                then do
                  zipWithM_ (checkParams pos1) params1 params2
                  unless (null params1) $ do
                    let 
                      dt = GDataType dtName (Just abstractName)
                      (_,t,_) = head params2
                    unless (t =:= dt) $ putError pos2 . UnknownError $ 
                      "First parameter of procedure `" <> unpack (defName def) <>
                      "` must have type " <> show t

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
          when (name1 /= name2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <>
            "` was declared as `" <> unpack name1 <> "`"

          when (mode1 /= mode2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <> "` has mode " <>
            show mode2 <>" but expected mode " <> show mode1

          let
            t1 = case t1' of
              GTypeVar i _ -> types' ! i
              _ -> t1'

          unless (t1 =:= t2) $ do
                putError pos . UnknownError $
                  "Parameter named `" <> unpack name2 <> "` has type " <>
                  show t2 <>" but expected type " <> show t1

recursiveDecl :: Type -> Type -> Bool
recursiveDecl (GArray _ inner) dt = recursiveDecl inner dt
recursiveDecl t dt = t =:= dt



