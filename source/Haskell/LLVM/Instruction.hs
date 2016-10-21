{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}

module LLVM.Instruction where
--------------------------------------------------------------------------------
import           AST.Expression                     (Expression (..),
                                                     Expression' (..))
import           AST.Instruction                    (Guard, Instruction (..),
                                                     Instruction' (..))
import qualified AST.Instruction                    as G (Instruction)
import           AST.Object                         hiding (indices)
import           AST.Struct                         (Struct (..))
import           AST.Type                           as T
import           Common
import           LLVM.Abort                         (abort)
import qualified LLVM.Abort                         as Abort (Abort (..))
import           LLVM.Boolean
import           LLVM.Declaration
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.Object                        (objectRef)
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                       (warn)
import qualified LLVM.Warning                       as Warning (Warning (Manual))
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (-=), (.=))
import           Data.Maybe                         (fromMaybe)
import           Data.Sequence                      (ViewR ((:>)))
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton, viewr,
                                                            zip, (|>))
import           Data.Text                          (pack, unpack)
import           Data.Word
import           LLVM.General.AST                   (BasicBlock (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import           LLVM.General.AST.Instruction       (FastMathFlags (..),
                                                     Instruction (..),
                                                     Named (..),
                                                     Terminator (..))
import qualified LLVM.General.AST.Instruction       as LLVM (Instruction)
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (CallableOperand,
                                                     Operand (..))
import           LLVM.General.AST.Type              hiding (void)
--------------------------------------------------------------------------------

guard :: Name -> Name -> Guard -> LLVM Name
guard finish checkLabel (expr, decls, insts) = do
  (checkLabel #)

  yes <- newLabel "instGuardYes"
  no  <- newLabel "instGuardNo"

  condition <- boolean yes no expr

  (yes #)
  openScope
  mapM_ declaration decls
  mapM_ instruction insts
  terminate Br
    { dest      = finish
    , metadata' = [] }

  closeScope
  pure no


instruction :: G.Instruction -> LLVM ()
instruction i@Instruction {instLoc=Location(pos, _), inst'} = case inst' of
  Abort -> do
    abort Abort.Manual pos
    newLabel "unreachable" >>= (#)

  Warn -> do
    warn Warning.Manual pos

  Assertion expr -> do
    -- Create both labels
    trueLabel  <- newLabel "assertTrue"
    falseLabel <- newLabel "assertFalse"
    -- Evaluate the condition expression using short-circuit
    boolean trueLabel falseLabel expr
    -- Set the false label to the abort
    -- And the true label to the next instructions
    (falseLabel #)
    abort Abort.Assert pos

    (trueLabel #)


  Assign { assignPairs } -> do
    -- get the values first
    values <- mapM expression' exprs
    -- then store them
    zipWithM_ assign' lvals values
    -- this way, things like `a, b := b, a` just work (tm).

    where
      (lvals, exprs) = unzip . toList $ assignPairs

      assign' lval value = do
        ref <- objectRef lval False
        type' <- toLLVMType . objType $ lval
        addInstruction $ Do Store
          { volatile       = False
          , address        = ref
          , value
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }


  Conditional { cguards } -> do
    entry  <- newLabel "ifExpEntry"
    finish <- newLabel "ifExpFinish"

    terminate Br
      { dest      = entry
      , metadata' = [] }

    abortLabel <- foldM (guard finish) entry cguards

    (abortLabel #)
    abort Abort.If pos

    (finish #)


  Block decls insts -> do
    block <- newLabel "blockEntry"
    exit <- newLabel "blockExit"
    terminate Br
      { dest      = block
      , metadata' = [] }

    (block #)
    openScope
    mapM_ declaration decls
    mapM_ instruction insts
    closeScope
    terminate Br
      { dest      = exit
      , metadata' = [] }

    (exit #)

  ProcedureCall { pName, pArgs, pStructArgs, pRecursiveCall, pRecursiveProc } -> do
    args <- toList <$> mapM createArg pArgs

    pName' <- case pStructArgs of
      Just (structBaseName, typeArgs) -> do
        llvmName (pName <> pack "-" <> structBaseName) <$>
          mapM toLLVMType (toList typeArgs)
      _ -> pure . unpack $ pName

    recArgs <- fmap (,[]) <$> if pRecursiveCall
      then do
        boundOperand <- fromMaybe (internal "boundless recursive function 2.") <$> use boundOp
        pure [ConstantOperand $ C.Int 1 1, boundOperand]
      else if pRecursiveProc
        then pure [ConstantOperand $ C.Int 1 0, ConstantOperand $ C.Int 32 0]
        else pure []

    addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType pName'
      , arguments          = recArgs <> args
      , functionAttributes = []
      , metadata           = [] }
    where
      -- Out and InOut arguments need to be passed as pointers to, so the address has to be casted
      -- If it is not an Out or InOut argument, then just pass a constant value.
      -- only basic types or pointers (because a pointer is just an integer) can be passed as a constant value.
      createArg (e, mode) = do
        subst <- use substitutionTable
        let type' = case subst of
              t:_ -> fillType t (expType e)
              []  -> expType e

        (, []) <$> if mode == In && (type' =:= basicT)
          then expression' e
          else do
            label <- newLabel "argCast"
            ref   <- objectRef (theObj . exp' $ e) False

            type' <- ptr <$> (toLLVMType . expType $ e)
            addInstruction $ label := BitCast
              { operand0 = ref
              , type'    = type'
              , metadata = [] }
            pure $ LocalReference type' label
      basicT = T.GOneOf [T.GBool,T.GChar,T.GInt,T.GFloat]

  Free { idName, freeType } -> do
    labelLoad  <- newLabel "freeLoad"
    labelCast  <- newLabel "freeCast"
    labelNull  <- newLabel "freeNull"
    ref        <- objectRef idName False
    type'      <- toLLVMType (T.GPointer freeType)

    case freeType of
      GArray { dimensions, innerType } -> do
        addInstruction $ labelLoad := Load
          { volatile  = False
          , address   = ref
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        iarrPtr <- newLabel "freeArrInternalPtr"
        addInstruction $ iarrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference type' labelLoad
          , indices  = ConstantOperand . C.Int 32 <$> [0, fromIntegral (length dimensions)]
          , metadata = [] }

        inner <- toLLVMType innerType

        let iarrT = iterate (ArrayType 1) inner !! length dimensions

        iarr <- newLabel "freeArrInternal"
        addInstruction $ iarr := Load
          { volatile  = False
          , address   = LocalReference (ptr iarrT) iarrPtr
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
          , metadata           = [] }

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference type' labelLoad
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = [(LocalReference pointerType labelCast, [])]
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = ConstantOperand $ C.Null type'
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

      _ -> do

        addInstruction $ labelLoad := Load
          { volatile  = False
          , address   = ref
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        let call name = Do Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable voidType $ "destroy" <> name
              , arguments          = [(LocalReference type' labelLoad, [])]
              , functionAttributes = []
              , metadata           = [] }

        case freeType of
          GFullDataType n t -> do
            types <- mapM toLLVMType (toList t)
            addInstruction $ call (llvmName n types)

          GDataType n t _ -> do
            subst:_ <- use substitutionTable
            types <- mapM toLLVMType $ toList subst
            addInstruction $ call (llvmName n types)

          _ -> pure ()

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference type' labelLoad
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = [(LocalReference pointerType labelCast, [])]
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = ConstantOperand $ C.Null type'
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

  New { idName, nType } -> do
    ref   <- objectRef idName False-- The variable that is being mallocated
    type' <- ptr <$> toLLVMType nType

    case nType of
      T.GArray { dimensions, innerType } -> do
        structSize <- sizeOf nType

        structCall <- newLabel "newArrStruct"
        addInstruction $ structCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(ConstantOperand $ C.Int 32 structSize, [])]
          , functionAttributes = []
          , metadata           = [] }

        structCast <- newLabel "newArrStructCast"
        addInstruction $ structCast := BitCast
          { operand0 = LocalReference pointerType structCall
          , type'
          , metadata = [] }

        dims <- mapM expression dimensions
        innerSize  <- sizeOf innerType
        bytes <- foldM bytesAux (ConstantOperand (C.Int 32 innerSize)) dims

        iarrCall <- newLabel "newArrInternal"
        addInstruction $ iarrCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(bytes, [])]
          , functionAttributes = []
          , metadata           = [] }

        inner <- toLLVMType innerType

        let iarrT = iterate (ArrayType 1) inner !! length dimensions

        iarrCast <- newLabel "newArrInternalCast"
        addInstruction $ iarrCast := BitCast
          { operand0 = LocalReference pointerType iarrCall
          , type'    = ptr iarrT
          , metadata = [] }

        arrPtr <- newUnLabel
        addInstruction $ arrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference type' structCast
          , indices  = ConstantOperand . C.Int 32 <$> [0, fromIntegral (length dimensions)]
          , metadata = [] }

        addInstruction $ Do Store
          { volatile       = False
          , address        = LocalReference (ptr iarrT) arrPtr
          , value          = LocalReference (ptr iarrT) iarrCast
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        void $ foldM (sizeAux (LocalReference type' structCast)) 0 dims

        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = LocalReference type' structCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

          where
            bytesAux operand0 operand1 = do
              result <- newUnLabel
              addInstruction $ result := Mul
                { operand0
                , operand1
                , nsw = False
                , nuw = False
                , metadata = [] }
              pure $ LocalReference i32 result

            sizeAux structRef n value = do
              dimPtr <- newLabel "dimPtr"
              addInstruction $ dimPtr := GetElementPtr
                { inBounds = False
                , address  = structRef
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

      _ -> do
        labelCall <- newLabel "newCall"
        labelCast <- newLabel "newCast"

        typeSize <- sizeOf nType

        addInstruction $ labelCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(ConstantOperand $ C.Int 32 typeSize, [])]
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference pointerType labelCall
          , type'    = type'
          , metadata = [] }

          -- Store the casted pointer in the variable
        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = LocalReference type' labelCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        let call name = Do Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable voidType $ "init" <> name
              , arguments          = [(LocalReference type' labelCast, [])]
              , functionAttributes = []
              , metadata           = [] }

        case nType of
          GFullDataType n t -> do
            types <- mapM toLLVMType (toList t)
            addInstruction $ call (llvmName n types)

          GDataType n t _ -> do
            subst:_ <- use substitutionTable
            types <- mapM toLLVMType $ toList subst
            addInstruction $ call (llvmName n types)

          _ -> pure ()


  Write { ln, wexprs } -> do
    operands <- mapM expression' wexprs
    mapM_ write (Seq.zip operands (expType <$> wexprs))

    when ln . addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType lnString
      , arguments          = []
      , functionAttributes = []
      , metadata           = [] }
    where
      write (operand, t') = do
        -- Build the operand of the expression

        st <- use substitutionTable
        let
          t = case st of
            ta:_ -> fillType ta t'
            _    -> t'

        let
        -- Call the correct C write function
          fun = callable voidType $ case t of
            GBool   -> writeBString
            GChar   -> writeCString
            GFloat  -> writeFString
            GInt    -> writeIString
            GString -> writeSString
            GAny    -> writeIString
            _         -> error
              "internal error: attempted to write non-basic type."
        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = fun
          , arguments          = [(operand, [])]
          , functionAttributes = []
          , metadata           = []}


  Read { file, vars } -> mapM_ readVar vars
    where
      readVar var = do
        st <- use substitutionTable

        let
          t' = objType var
          t = case st of
            (ta:_) -> fillType ta t'
            _      -> t'

        (args, fread) <- case file of
          Nothing -> pure . ([],) $ case t of
            T.GChar  -> readCharStd
            T.GFloat -> readFloatStd
            T.GInt   -> readIntStd
            T.GBool  -> readBoolStd
            _        -> internal "Unsupported read " <> show t

          Just file' -> do
            let
              fileRef = ConstantOperand . C.GlobalReference (ptr i8) . Name $
                        "__" <> unpack file'

            filePtr <- newLabel "filePtr"
            addInstruction $ filePtr := Load
              { volatile  = False
              , address   = fileRef
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }

            let filePtrOp = LocalReference (ptr i8) filePtr

            pure . ([(filePtrOp,[])], ) $ case t of
                T.GChar  -> readFileChar
                T.GFloat -> readFileFloat
                T.GInt   -> readFileInt
                T.GBool  -> readFileBool
                _        -> error ":D no se soporta este tipo: " <> show t

        type' <- toLLVMType t

        readResult <- newLabel "readCall"
        -- Call the C read function
        addInstruction $ readResult := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable type' fread
          , arguments          = args
          , functionAttributes = []
          , metadata           = [] }

        -- Get the reference of the variable's memory
        objRef <- objectRef var False
        -- Store the value saved at `readResult` in the variable memory
        addInstruction $ Do Store
          { volatile = False
          , address  = objRef
          , value    = LocalReference type' readResult
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }


  Repeat { rguards, rinv, rbound } -> do
    begin     <- newLabel "doBegin"
    again     <- newLabel "doAgain"
    checkGte0 <- newLabel "doCheckGte0"
    n         <- newLabel "doN"

    terminate Br
      { dest      = begin
      , metadata' = [] }

    (begin #)
    addInstruction $ n := Alloca
      { allocatedType = intType
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = [] }

    boundVal0 <- expression rbound
    Just begin' <- use blockName
    terminate Br
      { dest      = checkGte0
      , metadata' = [] }

    (again #)
    boundVal1 <- expression rbound
    oldBound <- newLabel "doOldBound"
    addInstruction $ oldBound := Load
      { volatile = False
      , address = LocalReference intType n
      , maybeAtomicity = Nothing
      , alignment = 4
      , metadata = [] }
    ltOld <- newLabel "doLtOld"
    addInstruction $ ltOld := ICmp
      { iPredicate = SLT
      , operand0   = boundVal1
      , operand1   = LocalReference intType oldBound
      , metadata   = [] }
    noLtOld <- newLabel "doNoLtOld"

    Just again' <- use blockName
    terminate CondBr
      { condition = LocalReference boolType ltOld
      , trueDest  = checkGte0
      , falseDest = noLtOld
      , metadata' = [] }

    (noLtOld #)
    abort Abort.NondecreasingBound pos

    (checkGte0 #)
    boundVal <- newLabel "doBound"
    addInstruction $ boundVal := Phi
      { type' = intType
      , incomingValues =
        [ (boundVal0, begin')
        , (boundVal1, again') ]
      , metadata = [] }
    gte0 <- newLabel "doGte0"
    addInstruction $ gte0 := ICmp
      { iPredicate = SGE
      , operand0   = LocalReference intType boundVal
      , operand1   = ConstantOperand $ C.Int 32 0
      , metadata   = [] }

    yesGte0 <- newLabel "doGte0Yes"
    noGte0  <- newLabel "doGte0No"
    terminate CondBr
      { condition = LocalReference boolType gte0
      , trueDest  = yesGte0
      , falseDest = noGte0
      , metadata' = [] }

    (noGte0 #)
    abort Abort.NegativeBound pos

    (yesGte0 #)
    yesInv <- newLabel "doInvYes"
    noInv  <- newLabel "doInvNo"

    boolean yesInv noInv rinv

    (noInv #)
    abort Abort.Invariant pos

    (yesInv #)
    addInstruction $ Do Store
      { volatile       = False
      , address        = LocalReference intType n
      , value          = LocalReference intType boundVal
      , maybeAtomicity = Nothing
      , alignment      = 4
      , metadata       = [] }

    firstGuard <- newLabel "doGuards"
    terminate Br
      { dest      = firstGuard
      , metadata' = [] }

    exit <- foldM (guard again) firstGuard rguards

    (exit #)

  Skip -> pure ()

  _ -> internal $
    "I don't know how to generate code for:" <> (drawTree . toTree $ i)
