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
import           Control.Monad       (void, when, foldM)
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
    currentStruct .= Just (abstractName, atypes)

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
                , structDecls = Seq.zip (Seq.fromList [0..]) decls
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
        t <- optional . parens $ (typeVar <|> basicType) `sepBy` match TokComma
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
    currentStruct .= Just (name, types)
    symbolTable %= openScope from

    

    match' TokBegin
    
    symbolTable %= openScope from
    decls'   <- sequence <$> polymorphicDeclaration `endBy` (match TokSemicolon)
    repinv'  <- repInv
    coupinv' <- coupInv
    procs'   <- sequence <$> many procedure

    match' TokEnd
    to <- getPosition
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
                
                st <- use symbolTable
                symbolTable %= closeScope to



                let 
                  struct = Struct
                        { structName  = name
                        , structDecls = Seq.zip (Seq.fromList [0..]) 
                                            (fmap snd structDecls <> decls)
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
              showPos pos <> " in the Type `" <> 
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
          when (checkParams (def' def1) (def' def2)) $ 
            putError pos1 . UnknownError $ 
                "The prodecure `" <> unpack (defName def1) <> 
                " does not match with the one defined at " <> 
                showPos pos2
          pure True
          

    checkParams (AbstractProcedureDef  params1) (ProcedureDef _ _ params2) = 
      and $ Seq.zipWith (\a b -> a == b) params1 params2

    checkParams _ _ = False



