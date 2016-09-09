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
import           AST.Type                   as T (Type (..))
import           LLVM.Monad
import           LLVM.State                 (currentStruct, substitutionTable)
--------------------------------------------------------------------------------
import           Control.Lens               (use)
import           Data.Array                 ((!))
import           Data.Foldable              (toList)
import           Data.List                  (intercalate)
import qualified Data.Map                   as Map (lookup)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack, unpack)
import           Data.Word                  (Word32, Word64)
import qualified LLVM.General.AST.AddrSpace as LLVM (AddrSpace (..))
import           LLVM.General.AST.Name      (Name (..))
import           LLVM.General.AST.Type      (double, i1, i16, i32, i8, ptr)
import qualified LLVM.General.AST.Type      as LLVM (Type (..))
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
stringType = LLVM.PointerType i8 (LLVM.AddrSpace 0)


toLLVMType :: T.Type -> LLVM LLVM.Type
toLLVMType  T.GInt         = pure intType
toLLVMType  T.GFloat       = pure floatType
toLLVMType  T.GBool        = pure boolType
toLLVMType  T.GChar        = pure charType
toLLVMType  GString        = pure $ ptr i8
toLLVMType (T.GPointer  t) = do
  inner <- toLLVMType t
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)

toLLVMType (T.GArray sz t) = do
  inner <- toLLVMType t
  pure $ LLVM.ArrayType (fromIntegral sz)  inner

toLLVMType (GFullDataType n t) = do
  types <- mapM toLLVMType t
  pure . LLVM.NamedTypeReference . Name . llvmName n . toList $ types

toLLVMType (GDataType name) = do
  maybeStruct <- use currentStruct
  case maybeStruct of
    Nothing -> error "Esto no deberia ocurrir :D"
    Just struct -> do
      types <- mapM toLLVMType (structTypes struct)
      pure . LLVM.NamedTypeReference . Name . llvmName name . toList $ types

toLLVMType (GTypeVar i) = do
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

sizeOf :: T.Type -> Integer
sizeOf T.GInt          = 4
sizeOf T.GBool         = 4
sizeOf T.GChar         = 4
sizeOf T.GFloat        = 8
sizeOf (T.GArray sz t) = fromIntegral sz * sizeOf t
sizeOf (T.GPointer t)  = 4


llvmName :: Text -> [LLVM.Type] -> String
llvmName name types = unpack name <> (('-' :) . intercalate "-" . fmap show') types
  where
    show' t
      | t == i1     = "b"
      | t == i8     = "c"
      | t == i32    = "i"
      | t == double = "f"
      | otherwise   = error $ show t
