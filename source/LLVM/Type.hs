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
import           Type                       as T (Type (..))
--------------------------------------------------------------------------------
import           Data.Word                  (Word32, Word64)
import qualified LLVM.General.AST.AddrSpace as LLVM (AddrSpace (..))
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


toLLVMType :: T.Type -> LLVM.Type
toLLVMType  T.GInt          = intType
toLLVMType  T.GFloat        = floatType
toLLVMType  T.GBool         = boolType
toLLVMType  T.GChar         = charType
toLLVMType GString          = ptr i8
toLLVMType (T.GPointer  t)  = LLVM.PointerType (toLLVMType t) (LLVM.AddrSpace 0)
toLLVMType (T.GArray sz t)  = LLVM.ArrayType (fromIntegral sz)  (toLLVMType t)

toLLVMType (GDataType _ ts) = LLVM.StructureType False (fmap toLLVMType ts)
toLLVMType GAny             = error "GAny is not a valid type"

toLLVMType _                = error "Unsupported type"


sizeOf :: T.Type -> Integer
sizeOf T.GInt          = 4
sizeOf T.GBool         = 4
sizeOf T.GChar         = 4
sizeOf T.GFloat        = 4
sizeOf (T.GArray sz t) = (fromIntegral sz) * sizeOf t
sizeOf (T.GPointer t)  = 4
