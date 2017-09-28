{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}

module Language.Graciela.LLVM.Struct
  ( defineStruct
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration  (Declaration (..))
import           Language.Graciela.AST.Definition   (Definition(..))
import           Language.Graciela.AST.Expression   (Expression (..))
import qualified Language.Graciela.AST.Instruction  as G (Instruction (..),
                                                          Instruction' (..))
import qualified Language.Graciela.AST.Object       as O
import           Language.Graciela.AST.Struct       (Struct (..), Struct' (..))
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort       (abort, abortString)
import qualified Language.Graciela.LLVM.Abort       as Abort (Abort (..))
import           Language.Graciela.LLVM.Definition
import           Language.Graciela.LLVM.Expression
import           Language.Graciela.LLVM.Instruction (copyArray, instruction)
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.Object
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.LLVM.Warning     (warn)
import qualified Language.Graciela.LLVM.Warning     as Warning (Warning (..))
import           Language.Graciela.Location
import           Language.Graciela.Treelike
--------------------------------------------------------------------------------
import           Control.Lens                       (makeLenses, use, (%=),
                                                     (+=), (.=), _head)
import           Data.List                          (sortOn)
import qualified Data.Map.Strict                    as Map

import           Data.Sequence                      ((|>))
import qualified Data.Sequence                      as Seq
import           Data.Text                          (Text, pack, unpack)
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

defineStruct :: Text -> (Struct, Map TypeArgs Bool) -> LLVM ()

defineStruct structBaseName (ast, typeMaps) = case ast of

  Struct { structBaseName, structTypes, structFields, structLoc 
         , structAFields, structProcs, struct'} -> case struct' of
    DataType {abstract, abstractTypes, inv, repinv, coupinv, couple} ->
      forM_ (Map.toList typeMaps) $ \(typeMap, fromOtherModule) -> do

        asserts <- use evalAssertions
        substitutionTable .= [typeMap]
        currentStruct .= Just ast
        let
          fields = toList structFields <> toList structAFields
        type' <- Just . StructureType True <$>
          mapM (toLLVMType . (\(_,x,_,_) -> x)) (sortOn (\(i,_,_,_) -> i) fields)

        types <- mapM fill structTypes

        let
          -- SourcePos f _ _ = 
          name  = llvmName structBaseName types
          structType = LLVM.NamedTypeReference (Name name)

        moduleDefs %= (|> TypeDefinition (Name name) type')
        traceM $ "\t"<> name <> if fromOtherModule then " (from Other module)" else ""

        currentStruct .= Nothing
        coupling .= True
        defineGetters fromOtherModule couple name structType -- While building getters, currentStruct must be Nothing
        coupling .= False
        currentStruct .= Just ast

        defaultConstructor fromOtherModule name structType typeMap
        defaultDestructor fromOtherModule name structType typeMap (pos structLoc)
        defaultCopy fromOtherModule name structType typeMap
        when (asserts) $ do
          defineStructInv fromOtherModule CoupInvariant name structType coupinv
          defineStructInv fromOtherModule Invariant name structType inv
          defineStructInv fromOtherModule RepInvariant name structType repinv

        -- defineCouple couple name structType

        forM_ structProcs $ \def -> 
          definition (def {isDecl = fromOtherModule})

        currentStruct .= Nothing
        substitutionTable .= []
    s -> pure ()






defaultConstructor :: Bool -- is a declaration or a definition
                   -> String 
                   -> LLVM.Type 
                   -> TypeArgs 
                   -> LLVM ()
defaultConstructor True name structType _ = do
  let
    procName = "init" <> name
    selfParam   = Parameter (ptr structType) (Name "_self") []
    dAllocParam = Parameter boolType (Name "dinamicAlloc") []

  addDefinitions $ [defineFunction procName [selfParam, dAllocParam] voidType]


defaultConstructor _ name structType typeMap = do
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
            , indices  = constantOperand GInt . Left <$> [0, field]
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
            , indices  = constantOperand GInt . Left <$> [0, field]
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

        let arrT = ArrayType 1 inner
        -- let arrT = iterate (ArrayType 1) inner !! length dimensions

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

        terminate Br
          { dest      = endLabel
          , metadata' = [] }


        (dAllocFalse #)

        iarr <- newUnLabel
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

        terminate Br
          { dest      = endLabel
          , metadata' = [] }

        (endLabel #)
      t | t =:= highLevel -> do
        member <- newLabel $ "member" <> show field
        newCollection <- newLabel $ "newCollection"

        let
          funStr = case t of
            GSet GTuple{}      -> "_newSetPair"
            GSet _             -> "_newSet"
            GMultiset GTuple{} -> "_newMultisetPair"
            GMultiset _        -> "_newMultiset"
            GSeq GTuple{}      -> "_newSequencePair"
            GSeq _             -> "_newSequence"
            GFunc _ _          -> "_newFunction"
            GRel _ _           -> "_newRelation"

        addInstruction $ newCollection := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType funStr
          , arguments          = []
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ member := GetElementPtr
            { inBounds = False
            , address  = self
            , indices  = constantOperand GInt . Left <$> [0, field]
            , metadata = []}

        addInstruction $ Do Store
          { volatile       = False
          , address        = LocalReference (pointerType) member
          , value          = LocalReference (pointerType) newCollection
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

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
      pure $ LocalReference intType result

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
        , address        = LocalReference intType dimPtr
        , value
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      pure $ n + 1

    value t = case t of
      t | t =:= GOneOf [GInt, GChar, GBool] -> pure . constantOperand t . Left $ 0
      GFloat         -> pure . constantOperand GFloat . Right $ 0
      t@(GPointer _) -> ConstantOperand . C.Null  <$> toLLVMType t



defaultDestructor :: Bool -- is a declaration or a definition
                  -> String 
                  -> LLVM.Type 
                  -> TypeArgs
                  -> SourcePos 
                  -> LLVM ()
defaultDestructor True name structType _ _= do
  let
    procName = "destroy" <> name
    selfParam   = Parameter (ptr structType) (Name "_self") []

  addDefinitions $ [defineFunction procName [selfParam] voidType]

defaultDestructor _ name structType typeMap pos = do
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
    SourcePos f l c = pos
    line = constantOperand GInt . Left . fromIntegral $ unPos l
    col  = constantOperand GInt . Left . fromIntegral $ unPos c
  filePath <- getFilePathOperand f
  forM_ fields $ \(field, t, _, expr) -> do
    let
      filledT = fillType typeMap t
    case filledT of
      t@GArray { dimensions, innerType } -> do
        garrT <- toLLVMType t
        inner <- toLLVMType innerType
        let iarrT = ArrayType 1 inner
        -- let iarrT = iterate (ArrayType 1) inner !! length dimensions

        arrStruct <- newLabel "arrStruct"

        addInstruction $ arrStruct := GetElementPtr
            { inBounds = False
            , address  = self
            , indices  = constantOperand GInt . Left <$> [0, field]
            , metadata = []}

        arrPtr <- newLabel "arrPtr"
        addInstruction $ arrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference garrT arrStruct
          , indices  = constantOperand GInt . Left <$> [0, fromIntegral (length dimensions)]
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
          , arguments          = (,[]) <$> [LocalReference pointerType iarrCast, filePath, line, col]
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

defaultCopy :: Bool -> String -> LLVM.Type -> TypeArgs -> LLVM ()
defaultCopy True name structType _ = do
  let
    procName = "copy" <> name
    source = Parameter (ptr structType) (Name "source") []
    dest   = Parameter (ptr structType) (Name "dest")   []

  addDefinitions $ [defineFunction procName [source, dest] voidType]

defaultCopy _ name structType typeMap = do
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
        , indices  = constantOperand GInt . Left <$> [0, field]
        , metadata = []}

    addInstruction $ destPtr := GetElementPtr
        { inBounds = False
        , address  = destStruct
        , indices  = constantOperand GInt . Left <$> [0, field]
        , metadata = []}

    case filledT of
      t@GArray { dimensions, innerType } -> do

        destArr      <- newLabel "destArr"

        copyArray t
          (LocalReference (ptr type') sourcePtr)
          (LocalReference (ptr type') destPtr)

      t | t =:= GADataType -> do

        types <- mapM fill (toList . typeArgs $ t)

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

  let 
    sourceParam = Parameter (ptr structType) sourceStructName []
    destParam   = Parameter (ptr structType) destStructName   []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([sourceParam, destParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }







defineGetters :: Bool -- is a declaration or a definition
              -> Seq G.Instruction
              -> String
              -> LLVM.Type
              -> LLVM ()
defineGetters True insts name t = do
  forM_ insts $ \x -> case G.inst' x of
    G.Assign {G.assignPairs} -> forM_ assignPairs $ \(lval,expr) -> do
        let
          varName = case O.obj' lval of
            O.Member{O.fieldName} -> fieldName
            _              -> internal $ "Could not build the getter of a non member lval"

          procName = "get_" <> unpack varName <> "-" <> name
          selfParam = Parameter (ptr t) (Name "_self") []
        
        addDefinitions $ [defineFunction procName [selfParam] (pointerType)]



defineGetters _ insts name t = do

  forM_ insts $ \x -> case G.inst' x of
    G.Assign {G.assignPairs} -> forM_ assignPairs $ \(lval,expr) -> do
        let
          varName = case O.obj' lval of
            O.Member{O.fieldName} -> fieldName
            _              -> internal $ "Could not build the getter of a non member lval"

          procName = "get_" <> unpack varName <> "-" <> name

        proc <- newLabel $ "proc" <> procName
        (proc #)

        openScope
        name' <- insertVar "_self"

        value <- expression' expr
        doGet .= False
        ref <- objectRef lval
        doGet .= True
        type' <- toLLVMType . O.objType $ lval
        addInstruction $ Do Store
          { volatile       = False
          , address        = ref
          , value
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        loadResult <- newLabel "loadGetter"
        addInstruction $ loadResult := Load
          { volatile  = False
          , address   = ref
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        terminate $ Ret (Just $ LocalReference (pointerType) loadResult) []
        closeScope

        blocks' <- use blocks
        blocks .= Seq.empty

        let
          selfParam    = Parameter (ptr t) name' []

        addDefinition $ GlobalDefinition functionDefaults
              { name        = Name procName
              , parameters  = ([selfParam],False)
              , returnType  = pointerType
              , basicBlocks = toList blocks' }


defineStructInv :: Bool -- is a declaration or a definition
                -> Invariant
                -> String
                -> LLVM.Type
                -> Expression
                -> LLVM ()
defineStructInv True inv name t _ = do
  let
    procName = (<> name) (case inv of
        CoupInvariant -> "coupInv-"
        Invariant     -> "inv-"
        RepInvariant  -> "repInv-")
    selfParam    = Parameter (ptr t) (Name "_self") []
    precondParam = Parameter boolType (Name "cond") []

  addDefinitions $ [defineFunction procName [selfParam, precondParam] voidType]

defineStructInv _ inv name t expr@ Expression {loc = Location(pos,_)} = do

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
    Invariant     -> abort Abort.RepInvariant pos
    RepInvariant  -> abort Abort.RepInvariant pos

  (precondFalse #)

  case inv of
    CoupInvariant -> warn Warning.CoupInvariant pos
    Invariant     -> warn Warning.RepInvariant pos
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
