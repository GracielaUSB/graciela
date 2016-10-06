{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Struct
    ( abstractDataType
    , dataType
    ) where
-------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Definition
import           AST.Expression      (Expression' (..))
import           AST.Instruction
import           AST.Object
import qualified AST.Object          as O (Object'(loc))
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
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens        (over, use, (%=), (.=), (.~), _2, _Just, (^.))
import           Control.Monad       (foldM, unless, void, when, zipWithM_, forM_)
import           Data.Array          ((!))
import qualified Data.Array          as Array (listArray)
import           Data.Foldable       (toList)
import           Data.Foldable       as F (concat)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as Map (empty, fromList, insert, lookup,
                                             size, toList, keysSet, filter)
import           Data.Maybe          (catMaybes, isNothing, isJust)
import qualified Data.Set            as Set (fromList, difference)
import           Data.Semigroup      ((<>))
import           Data.Sequence       (Seq, ViewL (..))
import qualified Data.Sequence       as Seq (empty, fromList, viewl, zip,
                                             zipWith)
import           Data.Text           (Text, pack, unpack)
import           Prelude             hiding (lookup)
import           Text.Megaparsec     (eof, getPosition, manyTill, optional,
                                      (<|>), between)
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
    let
      typeArgs = Array.listArray (0, length atypes - 1) atypes
      abstractType = GDataType abstractName Nothing typeArgs
      Just abstractName = abstractName'

    currentStruct .= Just (abstractType, Map.empty, Map.empty)

    match' TokBegin >>= \(Location(p,_)) -> symbolTable %= openScope p
    decls' <- sequence <$> (abstractDeclaration True `endBy` match' TokSemicolon)

    cs <- use currentStruct

    let fields = cs ^. _Just . _2

    inv'   <- invariant
    procs' <- sequence <$> many (procedureDeclaration <|> functionDeclaration)

    match' TokEnd
    to    <- getPosition
    st <- use symbolTable
    symbolTable %= closeScope to

    let
      loc = Location (from,to)

    case (inv', procs') of
      (Just inv, Just procs) -> do

        let struct = Struct
                { structBaseName   = abstractName
                , structFields  = fields
                , structAFields = Map.empty
                , structProcs   = Map.fromList $
                  (\d@Definition { defName } -> (defName, d)) <$> toList procs
                , structLoc     = loc
                , structSt      = st
                , structTypes   = atypes
                , struct'       = AbstractDataType inv
                }
        dataTypes %= Map.insert abstractName struct
      _ -> pure ()
    typeVars .= []
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
      -- Get the struct of the abstract data type that is being implemented
      abstractAST <- getStruct abstractName

      case abstractAST of
        Nothing -> do
          putError from . UnknownError $
            "Abstract Type `" <> show abstractName <> "` does not exist."
          void $ anyToken `manyTill` (void (match TokEnd) <|>  eof)

        Just Struct {structTypes, structFields, structProcs, struct'} -> do
          match' TokBegin

          symbolTable %= openScope from
          let
            dtType   = GDataType name abstractName' typeArgs
            typeArgs = Array.listArray (0, length types - 1) types
            lenNeeded = length structTypes
            lenActual = length absTypes
            abstFields = fillTypes abstractTypes structFields

            abstractTypes = Array.listArray (0, lenNeeded - 1) absTypes
          currentStruct .= Just (dtType, abstFields, Map.empty)

          decls' <- sequence <$> (dataTypeDeclaration `endBy` match' TokSemicolon)
          cs <- use currentStruct
          
          let 
            allFields = cs ^. _Just . _2

            adtToDt (i,t,c,e) = (i, f t, c, e)

            f t = case t of
                GDataType _ _ _ -> t <> GDataType name abstractName' typeArgs
                GArray s t -> GArray s (f t)
                GPointer t -> GPointer (f t)
                _ -> t

          repinv'  <- repInv
          coupling .= True
          coupinv' <- coupInv
          couple'  <- optional coupleRel
          coupling .= False

          getPosition >>= \pos -> symbolTable %= closeScope pos
          getPosition >>= \pos -> symbolTable %= openScope pos

          procs   <- catMaybes . toList <$> many (procedure <|> function)
          match' TokEnd
          
          to <- getPosition
          st <- use symbolTable
          symbolTable %= closeScope to
          abstractAST <- getStruct abstractName
          mapM_ (checkProc abstractTypes procs name abstractName) structProcs

          case (decls', repinv', coupinv', couple') of

            (Just decls, Just repinv, Just coupinv, Just couple'') -> do
              {- Different number of type arguments -}
              when (lenNeeded /= lenActual) . putError from $ BadNumberOfTypeArgs
                name structTypes abstractName absTypes lenActual lenNeeded

              let
                struct = Struct
                  { structBaseName = name
                  , structFields   = allFields
                  , structAFields  = Map.empty
                  , structSt       = st
                  , structProcs    = Map.fromList $ (\d -> (defName d, d)) <$> procs
                  , structLoc      = Location(from,to)
                  , structTypes    = types
                  , struct'        = DataType
                    { abstract     = abstractName
                    , abstractTypes
                    , inv = inv struct'
                    , repinv
                    , coupinv
                    , couple = couple''}}
              dataTypes %= Map.insert name struct
              typeVars .= []
              currentStruct .= Nothing

            (Just decls, Just repinv, Just coupinv, Nothing ) -> do
              {- Different number of type arguments -}
              when (lenNeeded /= lenActual) . putError from $ BadNumberOfTypeArgs
                name structTypes abstractName absTypes lenActual lenNeeded

              let Just Struct{ structLoc, structFields } = abstractAST 
              forM_ (Map.toList structFields) $ \(name, (_, t, _,_)) -> 
                when (t =:= highLevel) . putError (pos structLoc) . UnknownError $
                  "Expected couple for abstract variable `" <> unpack name <> "`."

              let
                struct = Struct
                  { structBaseName = name
                  , structFields   = allFields
                  , structAFields  = Map.empty
                  , structSt       = st
                  , structProcs    = Map.fromList $ (\d -> (defName d, d)) <$> procs
                  , structLoc      = Location(from,to)
                  , structTypes    = types
                  , struct'        = DataType
                    { abstract     = abstractName
                    , abstractTypes
                    , inv = inv struct'
                    , repinv
                    , coupinv
                    , couple = Seq.empty}}
              dataTypes %= Map.insert name struct
              typeVars .= []
              currentStruct .= Nothing

            _ -> pure ()

  where
    -- Check if all abstract procedures are defined in the implementation
    checkProc abTypes' procs dtName abstractName abstractProc = do
      let
        name = defName abstractProc
        Location(pos,_) = defLoc abstractProc

      ok <- or <$> mapM (abstractProc =-=) procs

      unless ok .putError pos . UnknownError $
        "The procedure named `" <> unpack name <> "` " <>
        showPos pos <> "` in the Abstract Type `" <> unpack abstractName <>
        "`\n\tneeds to be implemented inside the Type `" <>
        unpack dtName <> "`."
      where
        -- Check if the both abstract and the one implementing the abstract procedure,
        -- have the same header
        (=-=) :: Definition
              -> Definition
              -> Parser Bool
        (=-=) abstDef@Definition{def' = AbstractProcedureDef{}}
              def@Definition{def' = ProcedureDef{}}
          | defName def /= defName abstDef = pure False
          | otherwise = do
              let
                Location(pos1,_) = defLoc abstDef
                Location(pos2,_) = defLoc def
                params1 = toList . abstParams . def' $ abstDef
                params2 = toList . procParams . def' $ def

              if length params1 == length params2
                then zipWithM_ (checkParams pos1) params1 params2
                else putError pos2 . UnknownError $
                    "The prodecure `" <> unpack (defName def) <>
                    "` does not match with the one defined at " <>
                    showPos pos1 <> "."
              pure True

        (=-=) abstDef@Definition{def' = AbstractFunctionDef{}}
              def@Definition{def' = ProcedureDef{}}
          | defName def /= defName abstDef = pure False
          | otherwise = do
              let
                Location(pos1,_) = defLoc abstDef
                Location(pos2,_) = defLoc def
                params1      = toList . abstFParams . def' $ abstDef
                params2      = toList . funcParams  . def' $ def
                abstractRetT = funcRetType . def' $ abstDef
                retT         = funcRetType . def' $ def

              if length params1 /= length params2
                then putError pos2 . UnknownError $
                    "The prodecure `" <> unpack (defName def) <>
                    "` does not match with the one defined at " <>
                    showPos pos1 <> "."
              else if retT /= abstractRetT
                then putError pos2 . UnknownError $
                    "The return type of function `" <> unpack (defName def) <>
                    "` does not match with the one defined at " <>
                    showPos pos1 <> ".\n\tExpected " <> show abstractRetT <> "."
              else zipWithM_ (checkFParams pos1) params1 params2
              pure True

        _ =-= _ = pure False


        checkFParams pos (name1, t1) (name2, t2) =
          checkParams pos (name1, t1, In) (name2, t2, In)

        checkParams :: SourcePos
                    -> (Text,Type,ArgMode)
                    -> (Text,Type,ArgMode)
                    -> Parser ()
        checkParams pos (name1, t1', mode1) (name2, t2, mode2) = do
          when (name1 /= name2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <>
            "` was declared as `" <> unpack name1 <> "`."

          when (mode1 /= mode2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <> "` has mode " <>
            show mode2 <>" but expected mode " <> show mode1 <> "."

          let
            t1 = case t1' of
              GTypeVar i _ -> abTypes' ! i
              _ -> t1'

          unless (t1 =:= t2) . putError pos . UnknownError $
            "Parameter named `" <> unpack name2 <> "` has type " <>
            show t2 <>" but expected type " <> show t1 <> "."

coupleRel :: Parser (Seq Instruction)
coupleRel = do
  loc <- match TokWhere
  between (match' TokLeftBrace) (match' TokRightBrace) $ aux (pos loc)
  where 
    aux pos = do
      insts' <- sequence <$> assign `sepBy` match TokSemicolon
      case insts' of
        Just insts | not (null insts) -> do
          Just (GDataType{abstName = Just abstName}, _, _) <- use currentStruct
          Just (Struct{structFields})  <- getStruct abstName
          let 
            auxInsts = concat $ (toList . assignPairs . inst') <$> (toList insts)

          assigned <- Set.fromList <$> mapM (check structFields) auxInsts

          let 
            needed = Map.keysSet . Map.filter (\(_,t,_,_) -> t =:= highLevel) $ structFields
            left   = needed `Set.difference` assigned

          when (not (null left)) . forM_ left $ \name -> putError pos . UnknownError $ 
              "The variable `" <> unpack name <> "` has to be coupled."
          
          pure insts
        Just _ -> putError pos (UnknownError "Empty coupling relation.") >> pure Seq.empty
        _ -> pure Seq.empty      

    check fields (obj@Object{ O.loc = Location(a,b) }, expr) = case obj of 
        Object{ objType, obj' = Member{ fieldName }} -> do
          unless (isJust (fieldName `Map.lookup` fields) && objType =:= highLevel) .
            putError a . UnknownError $ "Unexpected coupling for variable `" <> unpack fieldName <> "`."
          pure fieldName
        Object{ objType, obj' = Variable{ name }} -> do
          putError a . UnknownError $ "Unexpected coupling for variable `" <> unpack name <> "`."
          pure name

recursiveDecl :: Type -> Type -> Bool
recursiveDecl (GArray _ inner) dt = recursiveDecl inner dt
recursiveDecl t dt = t =:= dt
