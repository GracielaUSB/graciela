{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Type
  ( floatType
  , intType
  , charType
  , pointerType
  , ptrInt
  , voidType
  , boolType
  , stringType
  , tupleType
  , iterator
  , toLLVMType
  , sizeOf
  , llvmName
  ) where
--------------------------------------------------------------------------------
import           AST.Expression             (Expression)
import           AST.Struct                 (Struct (..))
import           AST.Type                   as T (Type (..), fillType, (=:=))
import           Common
import           LLVM.Monad
import           LLVM.State                 (currentStruct, fullDataTypes,
                                             moduleDefs, pendingDataTypes,
                                             structs, substitutionTable)
--------------------------------------------------------------------------------
import           Control.Lens               (use, (%=))
import           Data.Array                 ((!))
import           Data.Foldable              (toList)
import           Data.Functor               (($>))
import           Data.List                  (intercalate, sortOn)
import qualified Data.Map                   as Map (alter, lookup)
import           Data.Maybe                 (fromMaybe)
import           Data.Sequence              ((|>))
import           Data.Text                  (Text, pack, unpack)
import           Data.Word                  (Word32, Word64)
import           LLVM.General.AST           (Definition (..))
import qualified LLVM.General.AST.AddrSpace as LLVM (AddrSpace (..))
import           LLVM.General.AST.Name      (Name (..))
import           LLVM.General.AST.Type      (double, i1, i32, i64, i8, ptr)
import qualified LLVM.General.AST.Type      as LLVM (Type (..))
import           System.Info                (arch)
--------------------------------------------------------------------------------

floatType, intType, charType, pointerType, ptrInt, voidType, boolType :: LLVM.Type
floatType   = double
intType     = i32
charType    = i8
pointerType = ptr i8
ptrInt      = if arch == "x86_64" then i64 else i32
voidType    = LLVM.VoidType
boolType    = i1
stringType  = ptr i8
tupleType   = LLVM.StructureType
                { LLVM.isPacked = True
                , LLVM.elementTypes = [i64,i64] }
iterator    = LLVM.StructureType
                { LLVM.isPacked = False
                , LLVM.elementTypes = [i64, ptr i8, ptr i8]
                }


toLLVMType :: T.Type -> LLVM LLVM.Type
toLLVMType  T.GInt           = pure intType
toLLVMType  T.GFloat         = pure floatType
toLLVMType  T.GBool          = pure boolType
toLLVMType  T.GChar          = pure charType
toLLVMType  GString          = pure stringType
toLLVMType (T.GPointer GAny) = pure . ptr $ i8
toLLVMType (T.GPointer  t)   = do
  inner <- toLLVMType t
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)
toLLVMType (GSet      _) = pure . ptr $ i8
toLLVMType (GMultiset _) = pure . ptr $ i8
toLLVMType (GFunc   _ _) = pure . ptr $ i8
toLLVMType (GRel    _ _) = pure . ptr $ i8
toLLVMType (GSeq      _) = pure . ptr $ i8
toLLVMType GAny          = internal "GAny is not a valid type"

toLLVMType (T.GArray dims t) = do
  inner <- toLLVMType t
  let arrT = iterate (LLVM.ArrayType 1) inner !! length dims
  pure LLVM.StructureType
    { LLVM.isPacked     = True
    , LLVM.elementTypes = (toList dims $> i32) <> [ptr arrT] }


toLLVMType (GTypeVar i _) = do
  substs <- use substitutionTable
  case substs of
    []        -> internal "subsitituting without substitution table."
    (subst:_) -> toLLVMType $ subst ! i

toLLVMType (GTuple  a b) = do
  a' <- toLLVMType a
  b' <- toLLVMType b
  pure tupleType
toLLVMType (GFullDataType n t) = do
  fdts <- use fullDataTypes
  pdt  <- use pendingDataTypes
  substs <- use substitutionTable
  let
    t' = case substs of
      []        -> t
      (subst:_) -> fmap (fillType subst) t
  case n `Map.lookup` fdts of
    Nothing -> do
      Just ast@Struct{} <- (n `Map.lookup`) <$> use structs
      case n `Map.lookup` pdt of
        Just (_, typeArgs) | t' `elem` typeArgs -> pure ()
        _                  -> pendingDT t' ast

    Just (s, typeArgs) | t' `elem` typeArgs -> pure ()
    Just (s, typeArgs) -> pendingDT t' s



  types <- mapM toLLVMType t'
  pure . LLVM.NamedTypeReference . Name . llvmName n . toList $ types


  where
    pendingDT t' s@Struct{ structFields, structAFields } = do
      let
        fields = toList structFields <> toList structAFields
      type' <- Just . LLVM.StructureType True <$>
                mapM  (toLLVMType . fillType t' . fillType t .(\(_,x,_,_) -> x))
                  (sortOn (\(i,_,_,_) -> i) $ fields)
      let
        fAlter = \case
          Nothing               -> Just (s, [t'])
          Just (struct, types0) -> Just (struct, t' : types0 )

      pendingDataTypes %= Map.alter fAlter n
      ltypes <- mapM toLLVMType t'
      moduleDefs %= (|> TypeDefinition (Name . llvmName n . toList $ ltypes) type')

toLLVMType t@(GDataType name _ typeArgs) = do
  maybeStruct <- use currentStruct
  case maybeStruct of
    Nothing | t =:= GATypeVar -> internal $ show t <> "   Esto no deberia ocurrir :D"
    Just struct -> do
      types <- mapM toLLVMType (structTypes struct)
      pure . LLVM.NamedTypeReference . Name . llvmName name . toList $ types

    _ -> do
      types <- mapM toLLVMType typeArgs
      pure . LLVM.NamedTypeReference . Name . llvmName name . toList $ types

toLLVMType t = error $ show t


sizeOf :: T.Type -> LLVM Integer
sizeOf T.GBool         = pure 1
sizeOf T.GChar         = pure 1
sizeOf T.GInt          = pure 4
sizeOf T.GFloat        = pure 8
-- sizeOf (T.GArray sz t) = (fromIntegral sz *) <$> sizeOf t
sizeOf (T.GArray sz t) = do
  dimSize <- sizeOf T.GInt
  ptrSize <- sizeOf $ T.GPointer GAny
  pure $ (fromIntegral . length $ sz) * dimSize + ptrSize
sizeOf (T.GPointer  t)  = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GSet      _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GMultiset _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GSeq      _  ) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GFunc     _ _) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GRel      _ _) = pure $ if arch == "x86_64" then 8 else 4
sizeOf (GTuple    _ _) = pure 16
sizeOf (T.GFullDataType name typeArgs) = getStructSize name typeArgs
sizeOf (T.GDataType name _ _) = do
  typeargs <- head <$> use substitutionTable
  getStructSize name  typeargs
sizeOf t@(GTypeVar _ _) = do
  substs <- use substitutionTable
  case substs of
      [] -> error $ "internal error unknow conversion for type " <> show t
      (subst:_) -> sizeOf (fillType subst t)
sizeOf t = error $ "internal error: getting size of an unknow type " <> show t


getStructSize name typeArgs = do
  structs' <- use structs
  case name `Map.lookup` structs' of
    Just Struct { structFields, structAFields, structTypes } -> do

      let
        fields = toList structFields <> toList structAFields
        types' = fillType typeArgs . (\(_,x,_,_) -> x) <$> fields

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
