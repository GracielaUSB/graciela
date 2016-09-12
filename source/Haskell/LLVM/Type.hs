{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module LLVM.Type
  ( floatType
  , intType
  , charType
  , pointerType
  , voidType
  , boolType
  , stringType
  , toLLVMType
  , sizeOf
  , llvmName
  )
where
--------------------------------------------------------------------------------
import           AST.Expression             (Expression)
import           AST.Struct                 (Struct (..))
import           AST.Type                   as T (Type (..), fillType)
import           LLVM.Monad
import           LLVM.State                 (currentStruct, substitutionTable,
                                             structs, fullDataTypes, pendingDataTypes
                                             , moduleDefs)
--------------------------------------------------------------------------------
import           Control.Lens               (use, (%=))
import           Data.Array                 ((!))
import           Data.Foldable              (toList)
import           Data.List                  (intercalate, sortOn)
import qualified Data.Map                   as Map (lookup, alter)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Sequence              ((|>))
import           Data.Text                  (Text, pack, unpack)
import           Data.Word                  (Word32, Word64)
import qualified LLVM.General.AST.AddrSpace as LLVM (AddrSpace (..))
import           LLVM.General.AST           (Definition (..))
import           LLVM.General.AST.Name      (Name (..))
import           LLVM.General.AST.Type      (double, i1, i16, i32, i8, ptr)
import qualified LLVM.General.AST.Type      as LLVM (Type (..))
import           System.Info                (arch)
--------------------------------------------------------------------------------

floatType :: LLVM.Type
floatType = double

intType :: LLVM.Type
intType = i32

charType :: LLVM.Type
charType = i8

pointerType :: LLVM.Type
pointerType = i8

voidType :: LLVM.Type
voidType = LLVM.VoidType

boolType :: LLVM.Type
boolType   = i1

stringType :: LLVM.Type
stringType = ptr i8


toLLVMType :: T.Type -> LLVM LLVM.Type
toLLVMType  T.GInt         = pure intType
toLLVMType  T.GFloat       = pure floatType
toLLVMType  T.GBool        = pure boolType
toLLVMType  T.GChar        = pure charType
toLLVMType  GString        = pure stringType
toLLVMType (T.GPointer  t) = do
  inner <- toLLVMType t
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)

toLLVMType (T.GArray sz t) = do
  inner <- toLLVMType t
  pure $ LLVM.ArrayType (fromIntegral sz)  inner

toLLVMType (GFullDataType n t) = do
  fdts <- use fullDataTypes
  pdt  <- use pendingDataTypes
  substs <- use substitutionTable
  let
    t' = case substs of
      [] -> t
      (subst:_) -> fmap (fillType subst) t
  case n `Map.lookup` fdts of 
    Nothing -> do 
      Just ast@Struct{structFields} <- (n `Map.lookup`) <$> use structs
      case n `Map.lookup` pdt of
        Just (_, typeArgs) | t' `elem` typeArgs -> pure ()        
        _ -> pendingDT t' ast

    Just (s, typeArgs) | t' `elem` typeArgs -> pure () 
    Just (s, typeArgs) -> pendingDT t' s
        


  types <- mapM toLLVMType t'
  pure . LLVM.NamedTypeReference . Name . llvmName n . toList $ types


  where
    pendingDT t' s@Struct{ structFields } = do
      type' <- Just . LLVM.StructureType True <$>
                mapM  (toLLVMType . fillType t' . fillType t .(\(_,x,_) -> x)) 
                  (sortOn (\(i,_,_) -> i) . toList $ structFields)
      let
        fAlter = \case
          Nothing               -> Just (s, [t'])
          Just (struct, types0) -> Just (struct, t' : types0 )

      pendingDataTypes %= Map.alter fAlter n
      ltypes <- mapM toLLVMType t'
      moduleDefs %= (|> TypeDefinition (Name . llvmName n . toList $ ltypes) type')

toLLVMType (GDataType name _) = do
  maybeStruct <- use currentStruct
  case maybeStruct of
    Nothing -> error "Esto no deberia ocurrir :D"
    Just struct -> do
      types <- mapM toLLVMType (structTypes struct)
      pure . LLVM.NamedTypeReference . Name . llvmName name . toList $ types

toLLVMType (GTypeVar i _) = do
  substs <- use substitutionTable
  case substs of
    [] -> error "internal error: subsitituting without substitution table."
    (subst:_) -> toLLVMType $ subst ! i


toLLVMType GAny            = error "internal error: GAny is not a valid type"

-- Unsupported Types
toLLVMType (GSet      _ ) = pure . ptr $ i8
toLLVMType (GMultiset _ ) = pure . ptr $ i8
toLLVMType (GFunc   _ _ ) = pure . ptr $ i8
toLLVMType (GRel    _ _ ) = pure . ptr $ i8
toLLVMType (GSeq      _ ) = pure . ptr $ i8
toLLVMType (GTuple    _ ) = pure . ptr $ i8
toLLVMType t = error $ show t



sizeOf :: T.Type -> LLVM Integer
sizeOf T.GBool         = pure 1
sizeOf T.GChar         = pure 1
sizeOf T.GInt          = pure 4
sizeOf T.GFloat        = pure 8
sizeOf (T.GArray sz t) = (fromIntegral sz *) <$> sizeOf t
sizeOf (T.GPointer t)  = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GSet      _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GMultiset _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GSeq      _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GFunc     _ _) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GRel      _ _) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GTuple    _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (T.GFullDataType name typeArgs) = getStructSize name typeArgs
sizeOf (T.GDataType name _) = do 
  typeargs <- head <$> use substitutionTable
  getStructSize name  typeargs
sizeOf t@(GTypeVar _ _) = do
  substs <- use substitutionTable
  case substs of
      [] -> error $ "internal error unknow convertion for type " <> show t
      (subst:_) -> sizeOf (fillType subst t)
sizeOf t = error $ "internal error: getting size of an unknow type " <> show t


getStructSize name typeArgs = do
  structs' <- use structs
  case name `Map.lookup` structs' of
    Just Struct { structFields, structTypes } -> do

      let
        types' = fmap (fillType typeArgs . (\(_,x,_) -> x)) $ 
          toList structFields 

      sum <$> mapM sizeOf types'

    Nothing -> error $ "internal error: getting size of an\
      \ unknow data type `" <> unpack name <> "`"


llvmName :: Text -> [LLVM.Type] -> String
llvmName name types = unpack name <> (('-' :) . intercalate "-" . fmap show') types
  where
    show' t
      | t == i1     = "b"
      | t == i8     = "c"
      | t == i32    = "i"
      | t == double = "f"
      | otherwise   = error $ show t
