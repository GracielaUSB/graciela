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
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Control.Monad       (foldM, unless, void, when, zipWithM_)
import           Data.Foldable       (toList)
import           Data.Foldable       as F (concat)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as Map (empty, fromList, insert, lookup,
                                             size)
import           Data.Maybe          (isNothing)
import           Data.Monoid         ((<>))
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq (fromList, zip, zipWith)
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

    decls' <- sequence <$> abstractDeclaration `endBy` match' TokSemicolon

    let
      fields = case decls' of
        Nothing    -> Map.empty
        Just decls -> toFields 0 decls

    currentStruct .= Just (abstractName, Nothing, fields)

    inv'   <- invariant
    procs' <- sequence <$> many procedureDeclaration

    match' TokEnd
    to    <- getPosition
    st <- use symbolTable
    symbolTable %= closeScope to
    let loc = Location (from,to)

    case (inv', procs') of
      (Just inv, Just procs) -> do

        let struct = Struct
                { structName   = abstractName
                , structFields = fields
                , structProcs  = procs
                , structLoc    = loc
                , structSt     = st
                , structTypes  = atypes
                , struct'      = AbstractDataType inv
                }
        dataTypes %= Map.insert abstractName struct
      _ -> pure ()
    typesVars .= []
    currentStruct .= Nothing


toFields :: Integer -> Seq Declaration -> Map Text (Integer, Type, Maybe Expression)
toFields n = Map.fromList . zipWith f [n..] . toList . F.concat . fmap toField'
  where
    toField' Declaration {declType, declIds} =
      zip (toList declIds) $ repeat (declType, Nothing)

    toField' Initialization { declType, declPairs} =
      let
        names = fmap fst  (toList declPairs)
        exprs = fmap snd  (toList declPairs)
      in
        zip names $ zip (repeat declType) (fmap Just exprs)

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
          decls'   <- sequence <$> (polymorphicDeclaration `endBy` match' TokSemicolon)

          let
            fields = case decls' of
              Nothing -> Map.empty
              Just decls -> toFields (fromIntegral $ Map.size structFields) decls

          currentStruct .= Just (name, abstractName', fields)

          repinv'  <- repInv
          coupinv' <- coupInv

          getPosition >>= \pos -> symbolTable %= closeScope pos

          getPosition >>= \pos -> symbolTable %= openScope pos

          currentStruct .= Just (name, Nothing, fields)
          procs'   <- sequence <$> many polymorphicProcedure
          currentStruct .= Just (name, abstractName', fields)

          match' TokEnd
          to <- getPosition
          st <- use symbolTable

          symbolTable %= closeScope to

          abstractAST <- getStruct abstractName

          case (procs', decls', repinv', coupinv') of

            (Just procs, Just decls, Just repinv, Just coupinv) -> do

              let
                lenNeeded = length structTypes
                lenActual = length absTypes
                loc       = Location(from,to)
                fields'   = fmap (\(_,x,_) -> x) (toList fields)
                abstractTypes = Map.fromList $ zip structTypes absTypes

              -- error $ show abstractTypes

              when (lenNeeded /= lenActual) . putError from . UnknownError $
                "Type `" <> unpack name <> "` is implementing `" <>
                unpack abstractName <> "` with only " <> show lenActual <>
                " types (" <> intercalate "," (fmap show absTypes) <>
                ")\n\tbut expected " <> show lenNeeded <> " types (" <>
                intercalate "," (fmap show structTypes) <> ")"

              mapM_ (checkProc abstractTypes procs abstractName) structProcs

              let
                struct = Struct
                      { structName  = name
                      , structFields = fields
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
          when (name1 /= name2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <>
            "` was declared as `" <> unpack name1 <> "`"

          when (mode1 /= mode2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <> "` has mode " <>
            show mode2 <>" but expected mode " <> show mode1

          let t1 = case t1' of
                  GTypeVar _ -> case t1' `Map.lookup` types' of
                    Nothing -> error $ show t1'
                    Just x -> x
                  _ -> t1'

          unless (t1 =:= t2) $ do
            currStruct <- use currentStruct

            case (currStruct, t1, t2) of
              -- If t1 and t2 are not the same, maybe its because one of then is implementing the other
              -- (e.g. Dicc implements Diccionario  => Dicc =:= Diccionario)
              (Just (dt, Just adt, _), GDataType n1, GDataType n2)
                | adt == n1 || dt == n2 -> pure ()

              _ ->
                putError pos . UnknownError $
                  "Parameter named `" <> unpack name2 <> "` has type " <>
                  show t2 <>" but expected type " <> show t1
