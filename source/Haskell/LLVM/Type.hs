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
  )
where
--------------------------------------------------------------------------------
import           AST.Expression             (Expression)
import           AST.Struct                 (Struct (..))
import           AST.Type                   as T (Type (..), llvmName)
import           LLVM.Monad
import           LLVM.State                 (currentStruct, substitutionTable)
--------------------------------------------------------------------------------
import           Control.Lens               (use)
import qualified Data.Map                   as Map (lookup)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (unpack)
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

toLLVMType (GFullDataType n t) =
  pure . LLVM.NamedTypeReference . Name . unpack $ llvmName n t

toLLVMType (GDataType name _) = do
  maybeStruct <- use currentStruct
  case maybeStruct of
    Nothing -> error "Esto no deberia ocurrir :D"
    Just struct -> do
      let types = structTypes struct
      pure . LLVM.NamedTypeReference . Name . unpack $ llvmName name types

toLLVMType var@(GTypeVar _) = do
  substs <- use substitutionTable
  case substs of
    [] -> error "internal error: subsitituting without substitution table."
    (subst:_) -> toLLVMType $
      fromMaybe (error "internal error: substituting an unavailable type var")
        (var `Map.lookup` subst)


toLLVMType GAny            = error "internal error: GAny is not a valid type"

-- Unsupported Types
-- toLLVMType t               = pure $ LLVM.ArrayType 123 i32

sizeOf :: T.Type -> Integer
sizeOf T.GInt          = 4
sizeOf T.GBool         = 4
sizeOf T.GChar         = 4
sizeOf T.GFloat        = 8
sizeOf (T.GArray sz t) = fromIntegral sz * sizeOf t
sizeOf (T.GPointer t)  = 4
