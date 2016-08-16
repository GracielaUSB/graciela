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
import           AST.Type              as T (Type, Type'(..))
import           AST.Expression        (Expression)
--------------------------------------------------------------------------------
import qualified LLVM.General.AST.AddrSpace  as LLVM (AddrSpace(..))
import qualified LLVM.General.AST.Type       as LLVM (Type(..))
import           LLVM.General.AST.Type       (i1, i8, i16, i32, double)
import           Data.Word                   (Word64, Word32)
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
toLLVMType T.GInt         = intType
toLLVMType T.GFloat       = floatType
toLLVMType T.GBool        = boolType
toLLVMType T.GChar        = charType
toLLVMType (T.GArray e t) = LLVM.ArrayType (constantToWord64 e) (toLLVMType t)

-- Calculate constant expressions
constantToWord64 :: Expression -> Word64
constantToWord64 e = 10
