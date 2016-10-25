{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}

module LLVM.Struct
  ( defineStruct
  ) where
--------------------------------------------------------------------------------
import           AST.Declaration                    (Declaration (..))
import           AST.Expression                     (Expression (..))
import qualified AST.Instruction                    as G (Instruction)
import           AST.Struct                         (Struct (..), Struct' (..))
import           AST.Type
import           Common
import           LLVM.Abort                         (abort, abortString)
import qualified LLVM.Abort                         as Abort (Abort (..))
import           LLVM.Definition
import           LLVM.Expression
import           LLVM.Instruction                   (instruction, copyArray)
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                       (warn)
import qualified LLVM.Warning                       as Warning (Warning (..))
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens                       (makeLenses, use, (%=),
                                                     (+=), (.=), _head)
import           Data.List                          (sortOn)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map

import           Data.Sequence                      (Seq, ViewR ((:>)), viewr,
                                                     (|>))
import qualified Data.Sequence                      as Seq
import           Data.Text                          (Text, unpack, pack)
import           LLVM.General.AST                   (BasicBlock (..),
                                                     Definition (..),
                                                     Parameter (..),
                                                     Terminator (..),
                                                     functionDefaults)
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import qualified LLVM.General.AST.Float             as LLVM (SomeFloat (Double))
import           LLVM.General.AST.Global            (Global (basicBlocks, name, parameters, returnType),
                                                     functionDefaults)
import           LLVM.General.AST.Instruction       (Instruction (..),
                                                     Named (..),
                                                     Terminator (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type              hiding (void)
import qualified LLVM.General.AST.Type              as LLVM
--------------------------------------------------------------------------------

data Invariant = Invariant | RepInvariant | CoupInvariant deriving (Eq)

defineStruct :: Text -> (Struct, [TypeArgs]) -> LLVM ()
defineStruct structBaseName (ast, typeMaps) = case ast of

  Struct {structBaseName,structTypes, structFields, structAFields, structProcs, struct'} -> case struct' of
    DataType {abstract, abstractTypes, inv, repinv, coupinv, couple} ->
      forM_ typeMaps $ \typeMap -> do
        substitutionTable .= [typeMap]
        currentStruct .= Just ast

        let
          fields = toList structFields <> toList structAFields
        type' <- Just . StructureType True <$>
          mapM (toLLVMType . (\(_,x,_,_) -> x)) (sortOn (\(i,_,_,_) -> i) fields)

        types <- mapM toLLVMType structTypes
        let
          name  = llvmName structBaseName types
          structType = LLVM.NamedTypeReference (Name name)

        moduleDefs %= (|> TypeDefinition (Name name) type')

        defaultConstructor name structType typeMap
        defaultDestructor name structType typeMap
        defaultCopy name structType typeMap
        defineStructInv CoupInvariant name structType coupinv
        defineStructInv Invariant name structType inv
        defineStructInv RepInvariant name structType repinv
        defineCouple couple name structType

        mapM_ definition structProcs

        currentStruct .= Nothing

defaultConstructor :: String -> LLVM.Type -> TypeArgs -> LLVM ()
defaultConstructor name structType typeMap = do
  let
    procName = "init" <> name
  proc <- newLabel procName

  (proc #)

  Just Struct { structFields, structAFields } <- use currentStruct

  openScope
  selfName <- insertVar "_self"
  dAllocName <- insertVar "dinamicAlloc"

  let
    self   = LocalReference structType selfName
    dAlloc = LocalReference boolType dAllocName
    fields = toList structFields <> toList structAFields
  forM_ fields $ \(field, t, _, expr) -> do
    let
      filledT = fillType typeMap t
    case filledT of
      t | t =:= GOneOf [GInt, GChar, GFloat, GBool, GPointer GAny] -> do

        member <- newLabel $ "member" <> show field

        addInstruction $ member := GetElementPtr
            { inBounds = False
            , address  = self
            , indices  = ConstantOperand . C.Int 32 <$> [0, field]
            , metadata = []}
        
        defaultValue <- case expr of
          Nothing -> value filledT
          Just e  -> expression' e


        t' <- toLLVMType filledT
        addInstruction $ Do Store
            { volatile = False
            , address  = LocalReference t' member
            , value    = defaultValue
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = []
            }
      t@GArray { dimensions, innerType } -> do
        name <- newUnLabel
        
        dims <- mapM expression dimensions
        
        innerSize <- sizeOf innerType
        numD <- foldM numAux (ConstantOperand (C.Int 32 innerSize)) dims
        numS <- foldM numAux (ConstantOperand (C.Int 32 1)) dims

        inner <- toLLVMType innerType
        garrT <- toLLVMType t

        addInstruction $ name := GetElementPtr
            { inBounds = False
            , address  = self
            , indices  = ConstantOperand . C.Int 32 <$> [0, field]
            , metadata = []}

        iarr <- newUnLabel
    
        dAllocTrue  <- newLabel "useMalloc"
        dAllocFalse <- newLabel "allocOnStack"
        endLabel    <- newLabel "endArrayAlloc"
        -- Create the conditional branch
        terminate CondBr
          { condition = dAlloc
          , trueDest  = dAllocTrue
          , falseDest = dAllocFalse
          , metadata' = [] }


        let arrT = iterate (ArrayType 1) inner !! length dimensions

        (dAllocTrue #)

        addInstruction $ iarr := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(numD, [])]
          , functionAttributes = []
          , metadata           = [] }

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

        terminate Br
          { dest      = endLabel
          , metadata' = [] }


        (dAllocFalse #)

        iarr <- newUnLabel
        addInstruction $ iarr := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocTCString
          , arguments          = [(numD, [])]
          , functionAttributes = []
          , metadata           = [] }

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

        terminate Br
          { dest      = endLabel
          , metadata' = [] }

        (endLabel #)
      _ -> pure ()
    pure ()

  terminate $ Ret Nothing []
  closeScope

  blocks' <- use blocks
  blocks .= Seq.empty

  let 
    selfParam   = Parameter (ptr structType) selfName []
    dAllocParam = Parameter boolType dAllocName []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([selfParam, dAllocParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }

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

    value t = case t of
      GBool          -> pure . ConstantOperand $ C.Int 1 0
      GChar          -> pure . ConstantOperand $ C.Int 8 0
      GInt           -> pure . ConstantOperand $ C.Int 32 0
      GFloat         -> pure . ConstantOperand . C.Float $ LLVM.Double 0
      t@(GPointer _) -> ConstantOperand . C.Null  <$> toLLVMType t

defaultDestructor :: String -> LLVM.Type -> TypeArgs -> LLVM ()
defaultDestructor name structType typeMap = do
  let
    procName = "destroy" <> name
  proc <- newLabel procName

  (proc #)

  Just Struct { structFields, structAFields } <- use currentStruct

  openScope
  selfName <- insertVar "_self"

  let
    self = LocalReference structType selfName
    fields = toList structFields <> toList structAFields

  forM_ fields $ \(field, t, _, expr) -> do
    let
      filledT = fillType typeMap t
    case filledT of
      t@GArray { dimensions, innerType } -> do
        garrT <- toLLVMType t
        inner <- toLLVMType innerType
        let iarrT = iterate (ArrayType 1) inner !! length dimensions

        arrStruct <- newLabel "arrStruct"

        addInstruction $ arrStruct := GetElementPtr
            { inBounds = False
            , address  = self
            , indices  = ConstantOperand . C.Int 32 <$> [0, field]
            , metadata = []}

        arrPtr <- newLabel "arrPtr"
        addInstruction $ arrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference garrT arrStruct
          , indices  = ConstantOperand . C.Int 32 <$> [0, fromIntegral (length dimensions)]
          , metadata = [] }

        iarr <- newLabel "freeArrInternal"
        addInstruction $ iarr := Load
          { volatile  = False
          , address   = LocalReference (ptr iarrT) arrPtr
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        iarrCast <- newLabel "freeArrInternalCast"
        addInstruction $ iarrCast := BitCast
          { operand0 = LocalReference (ptr iarrT) iarr
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = [(LocalReference pointerType iarrCast, [])]
          , functionAttributes = []
          , metadata = [] }

      _ -> pure ()

  terminate $ Ret Nothing []
  closeScope

  blocks' <- use blocks
  blocks .= Seq.empty

  let selfParam = Parameter (ptr structType) selfName []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([selfParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }

defaultCopy :: String -> LLVM.Type -> TypeArgs -> LLVM ()
defaultCopy name structType typeMap = do
  let
    procName = "copy" <> name
  proc <- newLabel procName

  (proc #)
  Just Struct { structFields, structAFields } <- use currentStruct

  openScope
  sourceStructName <- insertVar "sourceStruct"
  destStructName   <- insertVar "destStruct"

  symTable . _head %= Map.insert (pack "_self") sourceStructName

  let
    sourceStruct = LocalReference structType sourceStructName
    destStruct   = LocalReference structType destStructName
    fields = toList structFields <> toList structAFields

  forM_ fields $ \(field, t, _, expr) -> do
    let
      filledT = fillType typeMap t
    type' <- toLLVMType filledT
    sourcePtr   <- newLabel "sourcePtr"
    destPtr     <- newLabel "destPtr"

    addInstruction $ sourcePtr := GetElementPtr
        { inBounds = False
        , address  = sourceStruct
        , indices  = ConstantOperand . C.Int 32 <$> [0, field]
        , metadata = []}

    addInstruction $ destPtr := GetElementPtr
        { inBounds = False
        , address  = destStruct
        , indices  = ConstantOperand . C.Int 32 <$> [0, field]
        , metadata = []}

    case filledT of
      t@GArray { dimensions, innerType } -> do
        
        destArr      <- newLabel "destArr"

        copyArray t 
          (LocalReference (ptr type') sourcePtr)  
          (LocalReference (ptr type') destPtr)

      t | t =:= GADataType -> do

        types <- mapM toLLVMType (toList . typeArgs $ t)
        let 
          postfix = llvmName (typeName t) types  

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType $ "copy" <> postfix
          , arguments          = (,[]) <$> [ LocalReference (ptr type') sourcePtr
                                           , LocalReference (ptr type') destPtr ]
          , functionAttributes = []
          , metadata           = [] }

      _ -> do   
        sourceValue <- newLabel "sourceArr"
        addInstruction $ sourceValue := Load
          { volatile  = False
          , address   = LocalReference (ptr type') sourcePtr
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference (ptr type') destPtr
          , value    = LocalReference type' sourceValue
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = []
          }  
        pure ()

  terminate $ Ret Nothing []
  closeScope

  blocks' <- use blocks
  blocks .= Seq.empty

  let sourceParam = Parameter (ptr structType) sourceStructName []
  let destParam   = Parameter (ptr structType) destStructName   []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([sourceParam, destParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }




defineCouple :: Seq G.Instruction
             -> String
             -> LLVM.Type
             -> LLVM ()
defineCouple insts name t = do
  let
    procName = "couple-" <> name

  proc <- newLabel $ "proc" <> procName
  (proc #)

  openScope
  name' <- insertVar "_self"
  mapM_ instruction insts

  terminate $ Ret Nothing []
  closeScope

  blocks' <- use blocks
  blocks .= Seq.empty

  let
    selfParam    = Parameter (ptr t) name' []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([selfParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }




defineStructInv :: Invariant
                -> String
                -> LLVM.Type
                -> Expression
                -> LLVM ()
defineStructInv inv name t expr@ Expression {loc = Location(pos,_)} = do

    let
      procName = (<> name) (case inv of
          CoupInvariant -> "coupInv-"
          Invariant     -> "inv-"
          RepInvariant  -> "repInv-")

    proc <- newLabel $ "proc" <> procName
    (proc #)

    openScope
    name' <- insertVar "_self"
    -- Evaluate the condition expression
    condInv <- expression' expr
    -- Create both label
    trueLabel  <- newLabel "condTrue"
    falseLabel <- newLabel "condFalse"

    precondTrue  <- newLabel "precondTrue"
    precondFalse <- newLabel "precondFalse"
    -- Create the conditional branch
    terminate CondBr
      { condition = condInv
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = [] }
    -- Set the false label to the warning, then continue normally
    (falseLabel #)

    terminate CondBr
      { condition = LocalReference boolType (Name "cond")
      , trueDest  = precondTrue
      , falseDest = precondFalse
      , metadata' = [] }

    (precondTrue #)
    case inv of
      CoupInvariant -> abort Abort.CoupInvariant pos
      Invariant     -> abort Abort.Invariant pos
      RepInvariant  -> abort Abort.RepInvariant pos

    (precondFalse #)

    case inv of
      CoupInvariant -> warn Warning.CoupInvariant pos
      Invariant     -> warn Warning.Invariant pos
      RepInvariant  -> warn Warning.RepInvariant pos

    terminate Br
        { dest      = trueLabel
        , metadata' = [] }

    -- And the true label to the next instructions
    (trueLabel #)

    terminate $ Ret Nothing []
    closeScope

    blocks' <- use blocks
    blocks .= Seq.empty

    let
      selfParam    = Parameter (ptr t) name' []
      precondParam = Parameter boolType (Name "cond") []

    addDefinition $ GlobalDefinition functionDefaults
          { name        = Name procName
          , parameters  = ([selfParam, precondParam],False)
          , returnType  = voidType
          , basicBlocks = toList blocks' }
