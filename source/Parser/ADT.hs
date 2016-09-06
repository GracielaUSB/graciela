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
import           Control.Monad       (void, when, foldM, zipWithM, unless)
import           Data.Monoid         ((<>))
import           Data.Maybe          (catMaybes)
import qualified Data.Map            as Map
import qualified Data.Sequence       as Seq (fromList, zip, zipWith)
import           Data.Text           (Text, unpack, pack)
import           Text.Megaparsec     (getPosition, optional, (<|>))
import           Text.Megaparsec.Pos (SourcePos)
import Debug.Trace
-------------------------------------------------------------------------------


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


    when (abstractName' == Nothing) $ fail ""

    let Just abstractName = abstractName'
    currentStruct .= Just (abstractName, Nothing, atypes)

    match' TokBegin
    
    symbolTable %= openScope from
    decls' <- sequence <$> abstractDeclaration `endBy` match TokSemicolon
    inv'   <- invariant
    procs' <- sequence <$> many procedureDeclaration
    

    match' TokEnd
    
    to    <- getPosition
    symbolTable %= closeScope to
    let loc = Location (from,to)   

    st <- use symbolTable

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

    when (name' == Nothing) $ fail ""
    let Just name = name';
    currentStruct .= Just (name, abstractName', types)   

    match' TokBegin
    
    symbolTable %= openScope from
    symbolTable %= openScope from
    decls'   <- sequence <$> (polymorphicDeclaration `endBy` match TokSemicolon)
    repinv'  <- repInv
    coupinv' <- coupInv
    close <- getPosition
    symbolTable %= closeScope close

    procsPos <- getPosition
    symbolTable %= openScope procsPos
    

    procs'   <- sequence <$> many procedure

    match' TokEnd
    to <- getPosition
    st <- use symbolTable
    symbolTable %= closeScope to
    symbolTable %= closeScope to
    symbolTable %= closeScope to

    case abstractName' of 
      Just abstractName -> do 
        abstractAST <- getStruct abstractName
        case abstractAST  of 
          Nothing -> putError from $ UnknownError $
                        "Abstract Type `" <> show abstractName <> 
                        "` does not exists."

          Just Struct {structTypes,structDecls,structProcs, struct'} -> do 

            case (procs', decls', repinv', coupinv') of 
            
              (Just procs, Just decls, Just repinv, Just coupinv) -> do
            
                let
                  lenNeeded = length structTypes
                  lenActual = length absTypes
                  loc       = Location(from,to)
                  fields    = concat . fmap getFields $ decls
                  abstractTypes = Map.fromList $ zip absTypes structTypes

                when (lenNeeded /= lenActual) $ do
                  putError from $ UnknownError $ 
                    "Type `" <> unpack name <> "` is implementing `" <> 
                    unpack abstractName <> "` with only " <> show lenActual <> 
                    " types (" <> intercalate "," (fmap show absTypes) <> 
                    ")\n\tbut expected " <> show lenNeeded <> " types (" <>
                    intercalate "," (fmap show structTypes) <> ")"

                mapM_ (checkProc procs abstractName) structProcs

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
                         , repinv   
                         , coupinv }}
                dataTypes %= Map.insert name struct
                typesVars .= []
                currentStruct .= Nothing
              _ -> pure ()
      _ -> pure ()
 
  where
    checkProc procs abstractName proc= do
      let 
        name = defName proc
        Location(pos,_) = defLoc proc 
      ok <- or <$> mapM (\x -> proc =-= x) procs
      if ok
        then return ()
        else putError pos $ UnknownError $
              "The procedure named `" <> unpack name <> "` " <> 
              showPos pos <> "` in the Type `" <> 
              unpack abstractName <>
              "`\n\tneeds to be implemented inside Type `" <> 
              unpack name <> "`" 

    (=-=) :: Definition -> Definition -> Parser Bool
    def1 =-= def2
      | defName def1 /= defName def2 = pure False  
      | otherwise = do
          let 
            Location(pos1,_) = defLoc def1
            Location(pos2,_) = defLoc def2 
            params1 = toList . abstParams . def' $ def1
            params2 = toList . procParams . def' $ def2
          ok <- and <$> zipWithM checkParams params1 params2
          unless ( ok ) $ putError pos2 . UnknownError $ 
                "The prodecure `" <> unpack (defName def1) <> 
                "` does not match with the one defined at " <> 
                showPos pos1
          pure True
            
    checkParams :: (Text,Type,ArgMode) -> (Text,Type,ArgMode) -> Parser Bool
    checkParams (name1, t1, mode1) (name2, t2, mode2) = do
      if name1 /= name2
        then pure False
      else if mode1 /= mode2
        then pure False
      else if not (t1 =:= t2)
        then do
          currStruct <- use currentStruct
          case (currStruct, t1, t2) of
            (Just (dt, Just adt, _), GDataType n1 _, GDataType n2 _)
              | adt == n1 && dt == n2 -> pure True
            _ -> pure False
        else pure True
      

    checkParams _ _ = pure False
