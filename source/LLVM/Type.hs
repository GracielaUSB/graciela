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
import           AST.Struct                 (Struct(..))
import           LLVM.State
import           Type                       as T (Type (..), llvmName)
--------------------------------------------------------------------------------
import           Control.Lens               (use)
import           Data.Word                  (Word32, Word64)
import           Data.Text                  (unpack)
import           LLVM.General.AST.Name                   (Name(..))
import qualified LLVM.General.AST.AddrSpace as LLVM (AddrSpace (..))
import           LLVM.General.AST.Type      (double, i1, i16, i32, i8)
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
toLLVMType  T.GInt         = pure $ intType
toLLVMType  T.GFloat       = pure $ floatType
toLLVMType  T.GBool        = pure $ boolType
toLLVMType  T.GChar        = pure $ charType
toLLVMType (T.GPointer  t) = do
  inner <- toLLVMType t
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)

toLLVMType (T.GArray sz t) = do 
  inner <- toLLVMType t
  pure $ LLVM.ArrayType (fromIntegral sz)  inner


toLLVMType (GFullDataType n t) = 
  pure $ LLVM.NamedTypeReference $ Name (unpack $ llvmName n t)

toLLVMType (GDataType name _) = do
  maybeStruct <- use currentStruct
  case maybeStruct of
    Nothing -> error "Esto no deberia ocurrir :D"
    Just struct -> do
      let types = structTypes struct
      pure $ LLVM.NamedTypeReference $ Name (unpack $ llvmName name types)

toLLVMType GAny            = error "GAny is not a valid type"

-- Unsupported Types
toLLVMType t               = pure $ LLVM.ArrayType (fromIntegral 123)   i32


sizeOf :: T.Type -> Integer
sizeOf T.GInt          = 4
sizeOf T.GBool         = 4
sizeOf T.GChar         = 4
sizeOf T.GFloat        = 8
sizeOf (T.GArray sz t) = (fromIntegral sz) * sizeOf t
sizeOf (T.GPointer t)  = 4





