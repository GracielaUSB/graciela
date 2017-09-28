{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.LLVM.Type
  ( constantOperand
  , floatType
  , intType
  , charType
  , lintType
  , pointerType
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
import           LLVM.General.AST.Type            (double, i8, i32, i64,
                                                   ptr)
import qualified LLVM.General.AST.Type            as LLVM (Type (..))
import           System.Info                      (arch)
--------------------------------------------------------------------------------


boolSize, charSize, intSize, lintSize, ptrSize :: Word32 
boolSize = 1
charSize = 1
intSize  = 4
lintSize = 8
ptrSize  = if arch == "x86_64" then 8 else 4 

floatType, intType, charType, pointerType, voidType, boolType :: LLVM.Type
floatType   = double
boolType    = LLVM.IntegerType boolSize
charType    = LLVM.IntegerType (charSize * 8)
intType     = LLVM.IntegerType (intSize  * 8)
lintType    = LLVM.IntegerType (lintSize * 8)
pointerType = ptr i8
voidType    = LLVM.VoidType
stringType  = ptr charType
tupleType   = LLVM.StructureType
                { LLVM.isPacked = True
                , LLVM.elementTypes = [lintType, lintType] }
iterator    = LLVM.StructureType
                { LLVM.isPacked = False
                , LLVM.elementTypes = [lintType, pointerType, pointerType]
                }

constantOperand :: T.Type -> (Either Integer Double) -> Operand
constantOperand GBool     (Left n) | n `elem` [0,1]       = ConstantOperand $ C.Int boolSize n
constantOperand GChar     (Left n) | 0 <= n && n <= 2^8-1 = ConstantOperand $ C.Int (charSize * 8) n
constantOperand GInt      (Left n)  = ConstantOperand $ C.Int (intSize * 8) n
constantOperand (GEnum _) (Left n)  = ConstantOperand $ C.Int (intSize * 8) n
constantOperand I64       (Left n)  = ConstantOperand $ C.Int (lintSize * 8) n
constantOperand GFloat    (Right n) = ConstantOperand . C.Float . C.Double $ n

constantOperand t b = internal $ "LLVM/Type.hs: bad `constantOperand`\n" <> show t <> " -- " <> show b

fill :: T.Type -> LLVM T.Type
fill t = do
  subst <- use substitutionTable
  pure $ case subst of
    ta:_ -> fillType ta t
    []   -> t
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
toLLVMType :: T.Type -> LLVM LLVM.Type
toLLVMType t@(T.GTypeVar i _) = do
  subst <- use substitutionTable
  let
    t' = case subst of
      ta:_ -> fillType ta t
      []   -> internal $ "No substitution table" <> show subst
  toLLVMType t'

toLLVMType (T.GTuple  a b) = do
  a' <- toLLVMType a
  b' <- toLLVMType b
  pure tupleType

-- Abstract Data Type
toLLVMType (T.GDataType _ Nothing t) = do
  use currentStruct >>= \case
    Just Struct {structBaseName = n, structTypes} -> do
      t' <- mapM fill structTypes
      pure . LLVM.NamedTypeReference . Name . llvmName n $ t'
    Nothing -> internal $ "Trying to get llvm type of abstract data type"

-- Data Type
toLLVMType (T.GDataType n _ t) = do
  t' <- mapM fill t
  pure . LLVM.NamedTypeReference . Name . llvmName n . toList $ t'

toLLVMType (T.GPointer t) = do 
  inner <- toLLVMType t  
  pure $ LLVM.PointerType inner (LLVM.AddrSpace 0)

toLLVMType (T.GArray dims t) = do
    inner <- toLLVMType t
    let arrT = LLVM.ArrayType 1 inner
    pure $ LLVM.StructureType
      { LLVM.isPacked     = True
      , LLVM.elementTypes = reverse $ ptr arrT : (toList dims $> i32) }

toLLVMType (GAlias _ t) = toLLVMType t
toLLVMType t = pure $ case t of 
  T.GInt        -> intType
  T.GEnum _     -> intType
  T.GFloat      -> floatType
  T.GBool       -> boolType
  T.GChar       -> charType
  T.GString     -> stringType
  T.GPointer  _ -> pointerType
  T.GSet      _ -> pointerType
  T.GMultiset _ -> pointerType
  T.GFunc   _ _ -> pointerType
  T.GRel    _ _ -> pointerType
  T.GSeq      _ -> pointerType
  T.GAny        -> internal "GAny is not a valid type"
  
  _ -> internal $ "Could not translate type " <> show t <> " to a llvm type"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
sizeOf :: T.Type -> LLVM Integer
sizeOf (T.GArray sz t) = do
  dimSize <- sizeOf T.GInt
  pure $ (fromIntegral . length $ sz) * dimSize + (fromIntegral ptrSize)
sizeOf t@(GTypeVar _ _) = do
  substs <- use substitutionTable
  case substs of
      [] -> error $ "internal error unknow conversion for type " <> show t
      (subst:_) -> sizeOf (fillType subst t)
sizeOf (T.GDataType name _ typeargs) = getStructSize name typeargs
  where 
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

sizeOf (GAlias _ t) = sizeOf t

sizeOf t = pure $ fromIntegral $ case t of 
   T.GBool         -> boolSize
   T.GChar         -> charSize
   T.GInt          -> intSize
   T.GEnum _       -> intSize
   T.I64           -> lintSize
   T.GFloat        -> 8
   (T.GPointer  t) -> ptrSize
   (GSet      _  ) -> ptrSize
   (GMultiset _  ) -> ptrSize
   (GSeq      _  ) -> ptrSize
   (GFunc     _ _) -> ptrSize
   (GRel      _ _) -> ptrSize
   (GTuple    _ _) -> ptrSize * 2
   _ -> error $ "internal error: getting size of an unknow type " <> show t

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
llvmName :: Text -> [T.Type] -> String
llvmName name types = unpack name <> (('-' :) . intercalate "-" . fmap show') types
  where
    show' t = case t of
      GBool   -> "b"
      GChar   -> "c"
      GInt    -> "i"
      GFloat  -> "f"
      GEnum _ -> "i"
      GPointer t' -> "p_" <> show' t'
      GAlias n t -> show' t
      GDataType n _ t -> unpack n <> "-" <> (intercalate "-"  . fmap show' $ toList t)
      GArray _ t -> "[" <> show' t <> "]"
      t' -> internal $ "Can not create a llvm name with type " <> show t'
