module LLVM.Type
	( floatType 
	, intType 
	, charType 
	, pointerType 
	, voidType 
	, boolType 
	, doubleType 
	, stringType 
	, toType 
	)
where
--------------------------------------------------------------------------------
import           AST.Type              (Type, Type'(..))
import           AST.Type              (Expression)
--------------------------------------------------------------------------------
import qualified LLVM.General.AST.Type  as LLVM (Type)
import           LLVM.General.AST.Type  (IntegerType, i1, i8, i32, double)
import           Data.Word              (Word64, Word32)
--------------------------------------------------------------------------------

floatType :: LLVM.Type
floatType = double

intType :: LLVM.Type
intType = i32

charType :: LLVM.Type
charType = IntegerType 9

pointerType :: LLVM.Type
pointerType = i8

voidType :: LLVM.Type
voidType = VoidType

boolType :: LLVM.Type
boolType   = i1

stringType :: LLVM.Type
stringType = PointerType i16 (AddrSpace 0)


toLLVMType :: Type -> LLVM.Type
toLLVMType T.GInt         = intType
toLLVMType T.GFloat       = floatType
toLLVMType T.GBool        = boolType
toLLVMType T.GChar        = charType
toLLVMType (T.GArray e t) = ArrayType (constantToWord64 e) t

-- Calculate constant expressions
constantToWord64 :: Expression -> Word64
constantToWord64 e = 10
