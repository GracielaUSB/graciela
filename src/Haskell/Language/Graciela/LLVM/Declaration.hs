{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Graciela.LLVM.Declaration
  ( declaration
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration  (Declaration (..))
import           Language.Graciela.AST.Expression   (Expression,
                                                     Expression' (..))
import           Language.Graciela.AST.Struct       (Struct (..), Struct' (..))
import           Language.Graciela.AST.Type         (Type (..), (=:=))
import qualified Language.Graciela.AST.Type         as T (Type)
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort
import           Language.Graciela.LLVM.Expression
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (.=))
import qualified Data.Array                         as Array (listArray)
import           Data.Foldable                      (toList)
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton)
import           Data.Text                          (Text, unpack)
import           Data.Word
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import qualified LLVM.General.AST.Float             as LLVM (SomeFloat (Double))
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.General.AST.Instruction       (Instruction (..),
                                                      Named (..),
                                                      Terminator (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type              (Type (..), ptr)
--------------------------------------------------------------------------------

declaration :: Declaration -> LLVM ()
declaration Declaration { declType, declIds } =
  mapM_ (alloc declType) declIds

declaration Initialization { declType, declPairs } =
  mapM_ (initialize declType) declPairs

{- Allocate a variable -}
alloc :: T.Type -> Text -> LLVM ()
alloc t@GArray { dimensions, innerType } lval  = do
  name <- insertVar lval
  garrT <- toLLVMType t

  addInstruction $ name := Alloca
    { numElements   = Nothing
    , alignment     = 4
    , allocatedType = garrT
    , metadata      = [] }

  doArray t (LocalReference garrT name) 

  where
    multiplicate' :: Operand -> Operand -> LLVM Operand
    multiplicate' operand0 operand1 = do
      result <- newUnLabel
      addInstruction $ result := Mul
        { operand0
        , operand1
        , nsw = False
        , nuw = False
        , metadata = [] }
      pure $ LocalReference intType result

    sizeAux :: Operand -> Integer -> Operand -> LLVM Integer
    sizeAux ref n value = do
      dimPtr <- newLabel "dimPtr"
      addInstruction $ dimPtr := GetElementPtr
        { inBounds = False
        , address  = ref
        , indices  =
          [ constantOperand GInt (Left 0)
          , constantOperand GInt (Left n) ]
        , metadata = [] }

      addInstruction $ Do Store
        { volatile       = False
        , address        = LocalReference intType dimPtr
        , value
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      pure $ n + 1

    doArray :: T.Type -> Operand -> LLVM ()
    doArray t@GArray {dimensions, innerType} (LocalReference garrT name) = do 
      dims <- mapM expression dimensions
      num <- foldM multiplicate' (constantOperand GInt (Left 1)) dims

      inner <- toLLVMType innerType
      iarr <- newUnLabel

      addInstruction $ iarr := Alloca
        { numElements   = Just num
        , alignment     = 4
        , allocatedType = inner
        , metadata      = [] }

      
      case innerType of 
        GArray{} -> doIArray innerType (LocalReference inner iarr) num
        _ -> pure ()


      let arrT = ArrayType 1 inner
      -- let arrT = iterate (ArrayType 1) inner !! length dimensions

      iarrCast <- newUnLabel
      addInstruction $ iarrCast := BitCast
        { operand0 = LocalReference (ptr inner) iarr
        , type'    = ptr arrT
        , metadata = [] }

      arrPtr <- newUnLabel

      addInstruction $ arrPtr := GetElementPtr
        { inBounds = False
        , address  = LocalReference garrT name
        , indices  = constantOperand GInt . Left <$> [0, fromIntegral (length dimensions)]
        , metadata = [] }

      addInstruction $ Do Store
        { volatile       = False
        , address        = LocalReference (ptr arrT) arrPtr
        , value          = LocalReference (ptr arrT) iarrCast
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }


      void $ foldM (sizeAux (LocalReference garrT name)) 0 dims
      

    doIArray :: T.Type -> Operand -> Operand -> LLVM ()
    doIArray arrT@GArray{} iarr@(LocalReference t _) size = do 
      i <- newUnLabel
      loop <- newLabel "initInternalArrays"

      addInstruction $ i := Alloca
        { allocatedType = intType
        , numElements   = Nothing
        , alignment     = 4
        , metadata      = [] }

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference intType i
        , value    = constantOperand GInt $ Left 0
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = []
        }
      
      terminate Br
        { dest      = loop
        , metadata' = [] }
      
      (loop #)
      currentIte <- newUnLabel
      addInstruction $ currentIte := Load
        { volatile       = False
        , address        = LocalReference intType i
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      checkRange <- newUnLabel
      addInstruction $ checkRange := ICmp
        { iPredicate = SLE
        , operand0   = LocalReference intType currentIte
        , operand1   = size
        , metadata   = [] }

      done    <- newLabel "done"
      notDone <- newLabel "notDone"

      terminate CondBr
        { condition = LocalReference boolType checkRange
        , trueDest  = notDone
        , falseDest = done
        , metadata' = [] }

      (notDone #)
      currentArr <- newLabel "currentArr"
      addInstruction $ currentArr := GetElementPtr
        { inBounds = False
        , address  = iarr
        , indices  =
          [ LocalReference intType currentIte ]
          
        , metadata = [] }

      doArray arrT (LocalReference t currentArr)

      nextIte <- newLabel "qNextIte"
      addInstruction $ nextIte := Add
        { nsw = False
        , nuw = False
        , operand0 = LocalReference intType currentIte
        , operand1 = constantOperand GInt $ Left 1
        , metadata = [] }

      addInstruction $ Do Store
        { volatile = False
        , address  = LocalReference intType i
        , value    = LocalReference intType nextIte
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = []
        }

      terminate Br
        { dest      = loop
        , metadata' = [] }

      (done #)

      


alloc gtype lval = do
  name <- insertVar lval
  t    <- toLLVMType gtype

  addInstruction $ name := Alloca
    { allocatedType = t
    , numElements   = Nothing
    , alignment     = 4
    , metadata      = [] }

  let 
    doDTInit typeName typeArgs = do
      t' <- mapM fill (toList typeArgs)
      types' <- mapM toLLVMType t'
      let
        name'  = llvmName typeName t'
      cast <- newLabel "cast"

      addInstruction $ cast := BitCast
              { operand0 = LocalReference t name
              , type'    = ptr t
              , metadata = [] }

      let
        structArg = LocalReference (ptr t) cast
        dinamicAllocFlag = constantOperand GBool . Left $ 0
      addInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType $ "init" <> name'
        , arguments          = (,[]) <$> [structArg, dinamicAllocFlag]
        , functionAttributes = []
        , metadata           = [] }
  case gtype of
    GDataType { typeName, dtTypeArgs } -> doDTInit typeName dtTypeArgs
    GAlias _ GDataType { typeName, dtTypeArgs } -> doDTInit typeName dtTypeArgs

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
      GBool          -> pure . constantOperand GBool  . Left  $ 0
      GChar          -> pure . constantOperand GChar  . Left  $ 0
      GInt           -> pure . constantOperand GInt   . Left  $ 0
      GEnum _        -> pure . constantOperand GInt   . Left  $ 0
      GFloat         -> pure . constantOperand GFloat . Right $ 0
      t@(GPointer _) -> ConstantOperand . C.Null <$> toLLVMType t


{- Store an expression in a variable memory -}
initialize :: T.Type -> (Text, (Expression,Bool)) -> LLVM ()
initialize gtype (lval, (expr,_)) = do
  cs <- use currentStruct
  let
    type' = case cs of
      Just Struct{structBaseName, structTypes, struct' = DataType{abstract}} ->
        let
          ta = Array.listArray (0, length structTypes - 1) structTypes
          dt = GDataType
            { typeName = structBaseName
            , abstName = Just abstract
            , dtTypeArgs = ta }
        in
          if gtype =:= dt
            then gtype <> dt
            else gtype
      _ -> gtype

  name <- insertVar lval
  t    <- toLLVMType type'

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
