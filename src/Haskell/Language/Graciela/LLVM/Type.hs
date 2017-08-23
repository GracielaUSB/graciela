{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.LLVM.Type
  ( constantOperand
  , floatType
  , intType
  , charType
  , lintType
  , pointerType
  , ptrInt
  , voidType
  , boolType
  , stringType
  , tupleType
  , iterator
  , fill
  , toLLVMType
  , sizeOf
  , llvmName
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Expression (Expression)
import           Language.Graciela.AST.Struct     (Struct (..))
import           Language.Graciela.AST.Type       as T (Type (..), fillType,
                                                        (=:=))
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State     (currentStruct, fullDataTypes,
                                                   moduleDefs, structs,
                                                   substitutionTable)
--------------------------------------------------------------------------------
import           Control.Lens                     (use, (%=))
import           Data.Array                       ((!))
import           Data.Foldable                    (toList)
import           Data.List                        (intercalate, sortOn)
import qualified Data.Map                         as Map (alter, lookup)
import           Data.Maybe                       (fromMaybe)
import           Data.Sequence                    ((|>))
import           Data.Text                        (Text)
import           Data.Word                        (Word32, Word64)
import           LLVM.General.AST                 (Definition (..))
import qualified LLVM.General.AST.AddrSpace       as LLVM (AddrSpace (..))
import qualified LLVM.General.AST.Constant        as C
import qualified LLVM.General.AST.Float           as C (SomeFloat(Double))
import           LLVM.General.AST.Name            (Name (..))
import           LLVM.General.AST.Operand         (Operand(..))
import           LLVM.General.AST.Type            (double, i32, i64,
                                                   ptr)
import qualified LLVM.General.AST.Type            as LLVM (Type (..))
import           System.Info                      (arch)
--------------------------------------------------------------------------------


boolSize, charSize, intSize, lintSize :: Word32 
boolSize = 1
charSize = 8
intSize  = 32
lintSize = 64

floatType, intType, charType, pointerType, ptrInt, voidType, boolType :: LLVM.Type
floatType   = double
boolType    = LLVM.IntegerType boolSize
charType    = LLVM.IntegerType charSize
intType     = LLVM.IntegerType intSize
lintType    = LLVM.IntegerType lintSize
pointerType = pointerType
ptrInt      = if arch == "x86_64" then i64 else i32
voidType    = LLVM.VoidType
stringType  = pointerType
tupleType   = LLVM.StructureType
                { LLVM.isPacked = True
                , LLVM.elementTypes = [lintType, lintType] }
iterator    = LLVM.StructureType
                { LLVM.isPacked = False
                , LLVM.elementTypes = [lintType, pointerType, pointerType]
                }

constantOperand :: T.Type -> (Either Integer Double) -> Operand
constantOperand GBool  (Left n) | n `elem` [0,1]       = ConstantOperand $ C.Int boolSize n
constantOperand GChar  (Left n) | 0 <= n && n <= 2^8-1 = ConstantOperand $ C.Int charSize n
constantOperand GInt   (Left n)  = ConstantOperand $ C.Int intSize n
constantOperand I64    (Left n)  = ConstantOperand $ C.Int intSize n
constantOperand GFloat (Right n) = ConstantOperand . C.Float . C.Double $ n
constantOperand _ _ = error "Internal error: LLVM/Type.hs: bad `constantOperand`\n"

fill :: T.Type -> LLVM T.Type
fill t = do
  subst <- use substitutionTable
  pure $ case subst of
    ta:_ -> fillType ta t
    []   -> t

toLLVMType :: T.Type -> LLVM LLVM.Type
toLLVMType  T.GInt           = pure intType
toLLVMType  T.GFloat         = pure floatType
toLLVMType  T.GBool          = pure boolType
toLLVMType  T.GChar          = pure charType
toLLVMType  GString          = pure stringType
toLLVMType (T.GPointer GAny) = pure pointerType
toLLVMType (T.GPointer  t)   = do
  inner <- toLLVMType t
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)
toLLVMType (GSet      _) = pure pointerType
toLLVMType (GMultiset _) = pure pointerType
toLLVMType (GFunc   _ _) = pure pointerType
toLLVMType (GRel    _ _) = pure pointerType
toLLVMType (GSeq      _) = pure pointerType
toLLVMType GAny          = internal "GAny is not a valid type"

toLLVMType (T.GArray dims t) = do
  inner <- toLLVMType t
  let arrT = LLVM.ArrayType 1 inner
  -- let arrT = iterate (LLVM.ArrayType 1) inner !! length dims
  pure LLVM.StructureType
    { LLVM.isPacked     = True
    , LLVM.elementTypes = reverse $ ptr arrT : (toList dims $> i32) }

toLLVMType t@(GTypeVar i _) = do
  subst <- use substitutionTable
  let
    t' = case subst of
      ta:_ -> fillType ta t
      []   -> internal $ "No substitution table" <> show subst
  toLLVMType t'

toLLVMType (GTuple  a b) = do
  a' <- toLLVMType a
  b' <- toLLVMType b
  pure tupleType

-- Abstract Data Type
toLLVMType (GDataType _ Nothing t) = do
  use currentStruct >>= \case
    Just Struct {structBaseName = n, structTypes} -> do
      t' <- mapM fill structTypes
      pure . LLVM.NamedTypeReference . Name . llvmName n $ t'
    Nothing -> internal $ "Trying to get llvm type of abstract data type"


-- Data Type
toLLVMType (GDataType n _ t) = do
  t' <- mapM fill t
  pure . LLVM.NamedTypeReference . Name . llvmName n . toList $ t'

toLLVMType t = internal $ "Could not translate type " <> show t <> " to a llvm type"

sizeOf :: T.Type -> LLVM Integer
sizeOf T.GBool         = pure 1
sizeOf T.GChar         = pure 1
sizeOf T.GInt          = pure 4
sizeOf T.GFloat        = pure 8
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
sizeOf (T.GDataType name _ typeargs) = do
  getStructSize name typeargs
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


llvmName :: Text -> [T.Type] -> String
llvmName name types = unpack name <> (('-' :) . intercalate "-" . fmap show') types
  where
    show' t = case t of
      GBool  -> "b"
      GChar  -> "c"
      GInt   -> "i"
      GFloat -> "f"
      GPointer t' -> "p_" <> show' t'
      t' -> internal $ "Can not create a llvm name with type " <> show t'
