module LLVM.Type
    ( floatType
    , intType
    , charType
    , pointerType
    , voidType
    , boolType
    , stringType
    , toLLVMType
    )
where
--------------------------------------------------------------------------------
import           AST.Expression             (Expression)
import           Type                       as T (Type (..))
--------------------------------------------------------------------------------
import           Data.Word                  (Word32, Word64)
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
pointerType = i32

voidType :: LLVM.Type
voidType = LLVM.VoidType

boolType :: LLVM.Type
boolType   = i1

stringType :: LLVM.Type
stringType = LLVM.PointerType i16 (LLVM.AddrSpace 0)


toLLVMType :: T.Type -> LLVM.Type
toLLVMType T.GInt          = intType
toLLVMType T.GFloat        = floatType
toLLVMType T.GBool         = boolType
toLLVMType T.GChar         = charType
toLLVMType (T.GArray sz t) = LLVM.ArrayType (fromIntegral sz) (toLLVMType t)
