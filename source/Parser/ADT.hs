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
import           Graciela
import           Location
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Definition
import           Parser.Instruction
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens        (use, (%=), (.=))
import           Data.List           (intercalate)
import           Control.Monad       (void, when)
import           Data.Monoid         ((<>))
import           Data.Maybe          (catMaybes)
import qualified Data.Map            as Map
import qualified Data.Sequence       as Seq (fromList)
import           Data.Text           (Text, unpack, pack)
import           Text.Megaparsec     (Dec, ParseError, between, choice, endBy,
                                      getPosition, lookAhead, many, manyTill,
                                      notFollowedBy, sepBy, try, (<|>), optional)
import           Text.Megaparsec.Pos (SourcePos)
import Debug.Trace
-------------------------------------------------------------------------------


-- AbstractDataType -> 'abstract' Id AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: Graciela ()
abstractDataType = do
    from <- getPosition
    match TokAbstract
    abstractId <- identifier
    atypes <- parens (typeVarDeclaration `sepBy` match TokComma)
    currentStruct .= Just (abstractId, atypes)

    withRecovery TokBegin
    
    symbolTable %= openScope from
    decls <- abstractDeclaration `endBy` match TokSemicolon
    inv   <- safeAssertion invariant (NoAbstractInvariant abstractId)
    procs <- many procedureDeclaration
    

    withRecovery TokEnd
    
    to    <- getPosition
    symbolTable %= closeScope to
    let loc = Location (from,to)

    st <- use symbolTable
    let struct = Struct
            { structName  = abstractId
            , structDecls = zip [0..] decls
            , structProcs = procs
            , structLoc   = loc
            , structSt    = st
            , structTypes = atypes
            , struct'     = AbstractDataType inv 
            }

    typesVars .= []
    currentStruct .= Nothing
    
    dataTypes %= Map.insert abstractId struct


-- dataType -> 'type' Id 'implements' Id Types 'begin' TypeBody 'end'
dataType :: Graciela ()
dataType = do
    from <- getPosition
    match TokType
    id <- identifier
    types <- return . concat =<< (optional . parens $ typeVarDeclaration `sepBy` match TokComma)
    withRecovery TokImplements
    abstractId <- identifier
    absTypes <- return . concat =<< (optional . parens $ (typeVar <|> basicType) `sepBy` match TokComma)
    currentStruct .= Just (id, types)
    symbolTable %= openScope from

    abstractAST <- getStruct abstractId

    case abstractAST of 
      Nothing -> do
        fail $ "Abstract Type `" <> show abstractId <> "` does not exists."

      Just Struct {structTypes, structDecls, structProcs} -> do 
        let 
          lenNeeded = length structTypes
          lenActual = length absTypes
        when (lenNeeded /= lenActual) $ do
          putError (Location(from,from)) $ UnknownError $ 
            "Type `" <> unpack id <> "` is implementing `" <> unpack abstractId <> 
            "` with only " <> show lenActual <> " types (" <> 
            intercalate "," (fmap show absTypes) <> ")\n\tbut expected " <> 
            show lenNeeded <> " types (" <> intercalate "," (fmap show structTypes) <> ")"

        withRecovery TokBegin
        
        symbolTable %= openScope from
        decls   <- polymorphicDeclaration `endBy` (match TokSemicolon)
        repinv  <- safeAssertion repInvariant  (NoTypeRepInv  id)
        coupinv <- safeAssertion coupInvariant (NoTypeCoupInv id)
        procs   <- many procedure

        withRecovery TokEnd
        to <- getPosition

        let
          loc    = Location(from,to)
          fields = concat . fmap getFields $ decls
          checkProc = \proc -> do
              let 
                name = defName proc
                Location(pos,_) = defLoc proc 
              if foldr ((||) . (\x -> (defName x) == name)) False procs
                then return ()
                else putError loc $ UnknownError $
                      "The procedure named `" <> unpack name <> "` " <> 
                      showPos' pos <> " in the Type `" <> unpack abstractId <>
                      "`\n\tneeds to be implemented inside Type `" <> unpack id <> "`" 
        mapM_ checkProc structProcs
        
        symbolTable %= closeScope to

        let struct =  Struct
                { structName  = id
                , structDecls = zip [0..] (fmap snd structDecls <> decls)
                , structProcs = procs                
                , structLoc   = loc
                , structTypes = types
                , struct'     = DataType
                 { abstract = abstractId
                 , repinv
                 , coupinv}}
        dataTypes %= Map.insert id struct
        typesVars .= []
        currentStruct .= Nothing

