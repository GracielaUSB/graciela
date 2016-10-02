{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Declaration
  ( declaration
  ) where
--------------------------------------------------------------------------------
import           AST.Declaration                    (Declaration (..))
import           AST.Expression                     (Expression' (..))
import           AST.Type                           (Expression, Type (..),
                                                     isDataType, (=:=))
import qualified AST.Type                           as G (Type)
import           LLVM.Abort
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (.=))
import           Control.Monad                      (foldM, void, when,
                                                     zipWithM_)
import           Data.Foldable                      (toList)
import           Data.Functor                       (($>))
import           Data.Semigroup                     ((<>))
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton)
import           Data.Text                          (Text, unpack)
import           Data.Word
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import qualified LLVM.General.AST.Float             as LLVM (SomeFloat (Double))
import           LLVM.General.AST.Instruction       (Instruction (..),
                                                     Named (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type              (Type (..), i32, ptr)
--------------------------------------------------------------------------------
import           Debug.Trace

declaration :: Declaration -> LLVM ()
declaration Declaration { declType, declIds } =
  mapM_ (alloc declType) declIds

declaration Initialization { declType, declPairs } =
  mapM_ (initialize declType) declPairs

{- Allocate a variable -}
alloc :: G.Type -> Text -> LLVM ()
alloc t@GArray { dimensions, innerType } lval = do
  name <- insertVar lval

  dims <- mapM expression dimensions
  innerSize <- sizeOf innerType
  num <- foldM numAux (ConstantOperand (C.Int 32 innerSize)) dims

  inner <- toLLVMType innerType
  garrT <- toLLVMType t

  addInstruction $ name := Alloca
    { numElements   = Nothing
    , alignment     = 4
    , allocatedType = garrT
    , metadata      = [] }

  iarr <- newUnLabel
  addInstruction $ iarr := Alloca
    { numElements   = Just num
    , alignment     = 4
    , allocatedType = inner
    , metadata      = [] }


  let arrT = iterate (ArrayType 1) inner !! length dimensions

  iarrCast <- newUnLabel
  addInstruction $ iarrCast := BitCast
    { operand0 = LocalReference (ptr inner) iarr
    , type'    = ptr arrT
    , metadata = [] }

  arrPtr <- newUnLabel

  addInstruction $ arrPtr := GetElementPtr
    { inBounds = False
    , address  = LocalReference garrT name
    , indices  = ConstantOperand . C.Int 32 <$> [0, fromIntegral (length dimensions)]
    , metadata = [] }

  addInstruction $ Do Store
    { volatile       = False
    , address        = LocalReference (ptr arrT) arrPtr
    , value          = LocalReference (ptr arrT) iarrCast
    , maybeAtomicity = Nothing
    , alignment      = 4
    , metadata       = [] }

  void $ foldM (sizeAux (LocalReference garrT name)) 0 dims

  where
    numAux operand0 operand1 = do
      result <- newUnLabel
      addInstruction $ result := Mul
        { operand0
        , operand1
        , nsw = False
        , nuw = False
        , metadata = [] }
      pure $ LocalReference i32 result

    sizeAux ref n value = do
      dimPtr <- newLabel "dimPtr"
      addInstruction $ dimPtr := GetElementPtr
        { inBounds = False
        , address  = ref
        , indices  =
          [ ConstantOperand (C.Int 32 0)
          , ConstantOperand (C.Int 32 n) ]
        , metadata = [] }

      addInstruction $ Do Store
        { volatile       = False
        , address        = LocalReference i32 dimPtr
        , value
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      pure $ n + 1


alloc gtype lval = do
  name <- insertVar lval
  t    <- toLLVMType gtype

  addInstruction $ name := Alloca
    { allocatedType = t
    , numElements   = Nothing
    , alignment     = 4
    , metadata      = [] }

  case gtype of
    GFullDataType { typeName, typeArgs } -> do
      types' <- mapM toLLVMType $ toList typeArgs
      let
        name'  = llvmName typeName types'
      cast <- newLabel "cast"

      addInstruction $ cast := BitCast
              { operand0 = LocalReference t name
              , type'    = ptr t
              , metadata = [] }

      addInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType $ "init" <> name'
        , arguments          = [(LocalReference (ptr t) cast,[])]
        , functionAttributes = []
        , metadata           = [] }

      pushbackInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType $ "destroy" <> name'
        , arguments          = [(LocalReference (ptr t) cast,[])]
        , functionAttributes = []
        , metadata           = [] }

    _ | gtype =:= GOneOf [GInt, GChar, GFloat, GBool, GPointer GAny] -> do

      defaultValue <- value gtype
      addInstruction $ Do Store

          { volatile = False
          , address  = LocalReference t name
          , value    = defaultValue
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = []
          }

    _ -> pure ()

  where
    value t = case t of
      GBool          -> pure . ConstantOperand $ C.Int 1 0
      GChar          -> pure . ConstantOperand $ C.Int 8 0
      GInt           -> pure . ConstantOperand $ C.Int 32 0
      GFloat         -> pure . ConstantOperand . C.Float $ LLVM.Double 0
      t@(GPointer _) -> ConstantOperand . C.Null  <$> toLLVMType t


{- Store an expression in a variable memory -}
initialize :: G.Type -> (Text, Expression) -> LLVM ()
initialize gtype (lval, expr) = do
  name <- insertVar lval
  t    <- toLLVMType gtype

  addInstruction $ name := Alloca
    { allocatedType = t
    , numElements   = Nothing
    , alignment     = 4
    , metadata      = [] }

  value <- expression' expr
  -- The store is an unamed instruction, so get the next instruction label
  addInstruction $ Do Store
    { volatile = False
    , address  = LocalReference t name
    , value    = value
    , maybeAtomicity = Nothing
    , alignment = 4
    , metadata  = [] }
