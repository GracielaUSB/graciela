{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module Language.Graciela.LLVM.Definition where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration   (Declaration(..))
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression    (Expression (..))
import qualified Language.Graciela.AST.Instruction   as G (Instruction)
import qualified Language.Graciela.AST.Object        as O
import           Language.Graciela.AST.Struct        (Struct (..), Struct' (..))
import           Language.Graciela.AST.Type          ((=:=))
import qualified Language.Graciela.AST.Type          as T
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort        (abort, abortString)
import qualified Language.Graciela.LLVM.Abort        as Abort (Abort (..))
import           Language.Graciela.LLVM.Boolean
import           Language.Graciela.LLVM.Declaration  (declaration)
import           Language.Graciela.LLVM.Expression
import           Language.Graciela.LLVM.Instruction
import           Language.Graciela.LLVM.Object
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.LLVM.Warning      (warn, warnString)
import qualified Language.Graciela.LLVM.Warning      as Warning (Warning (Post, Pre))
import           Language.Graciela.Location
import qualified Language.Graciela.Location          as L (pos)
import           Language.Graciela.Parser.Config
import           Language.Graciela.Treelike
--------------------------------------------------------------------------------
import           Control.Lens                        (use, (%=), (&), (.=))
import           Data.Array                          ((!))
import           Data.Foldable                       (toList)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromJust, fromMaybe,
                                                      isJust)
import qualified Data.Sequence                       as Seq (empty, fromList)
import           Data.Text                           (Text)
import           Data.Word                           (Word32)
import           LLVM.General.AST                    (BasicBlock (..),
                                                      Named (..),
                                                      Parameter (..),
                                                      Terminator (..),
                                                      functionDefaults)
import qualified LLVM.General.AST                    as LLVM (Definition (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention  as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant           as C
import           LLVM.General.AST.Global             (Global (..),
                                                      functionDefaults)
import           LLVM.General.AST.Instruction
import           LLVM.General.AST.IntegerPredicate   (IntegerPredicate (EQ, SGE, SLT))
import           LLVM.General.AST.Linkage            (Linkage (Private))
import           LLVM.General.AST.Name               (Name (..))
import           LLVM.General.AST.Operand            (MetadataNode (..),
                                                      Operand (..))
import           LLVM.General.AST.ParameterAttribute (ParameterAttribute (..))
import           LLVM.General.AST.Type               (Type (..), double, i1,
                                                      i32, i64, i8, ptr)
import qualified LLVM.General.AST.Type               as LLVM (Type)
import           LLVM.General.AST.Visibility         (Visibility (Default))
import           Prelude                             hiding (Ordering (EQ))
--------------------------------------------------------------------------------

{- Given the instruction block of the main program, construct the main LLVM function-}
mainDefinition :: G.Instruction -> [String] -> LLVM ()
mainDefinition block files = do
  main <- newLabel "main"

  (main #)
  mapM_ openFile files

  addInstruction $ Do Call
    { tailCallKind       = Nothing
    , callingConvention  = CC.C
    , returnAttributes   = []
    , function           = callable voidType initTrashCollectorString
    , arguments          = []
    , functionAttributes = []
    , metadata           = [] }

  addInstruction $ Do Call
    { tailCallKind       = Nothing
    , callingConvention  = CC.C
    , returnAttributes   = []
    , function           = callable voidType openScopeString
    , arguments          = []
    , functionAttributes = []
    , metadata           = [] }

  instruction block

  addInstruction $ Do Call
    { tailCallKind       = Nothing
    , callingConvention  = CC.C
    , returnAttributes   = []
    , function           = callable voidType freeTrashCollectorString
    , arguments          = []
    , functionAttributes = []
    , metadata           = [] }

  mapM_ closeFile files

  terminate $ Ret (Just . ConstantOperand $ C.Int 32 0) []

  blocks' <- use blocks
  blocks .= Seq.empty
  addDefinition $ LLVM.GlobalDefinition functionDefaults
    { name        = Name "main"
    , parameters  = ([], False)
    , returnType  = i32
    , basicBlocks = toList blocks'
    }

  where
    openFile file = do
      let
        fileRef = ConstantOperand . C.GlobalReference pointerType . Name $
                  "__" <> file

      fileLabel <- newLabel "file"
      strs <- use stringIds
      let Just i = (pack file) `Map.lookup` strs
      string <- (!i) <$> use stringOps
      addInstruction $ fileLabel := Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable pointerType openFileStr
        , arguments          = [(string,[])]
        , functionAttributes = []
        , metadata           = [] }

      addInstruction $ Do Store
          { volatile = False
          , address  = fileRef
          , value    = LocalReference pointerType fileLabel
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

    closeFile file = do
      let
        fileRef = ConstantOperand . C.GlobalReference pointerType . Name $
                  "__" <> file
      filePtr <- newLabel "filePtr"
      addInstruction $ filePtr := Load
        { volatile  = False
        , address   = fileRef
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      addInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType closeFileStr
        , arguments          = [(LocalReference pointerType filePtr,[])]
        , functionAttributes = []
        , metadata           = [] }



{- Translate a definition from Graciela AST to LLVM AST -}
definition :: Definition -> LLVM ()
definition Definition { defName, def', pre, post, bound, defLoc = Location (pos, _to), isDecl }
  = if isDecl then do 
      use evalAssertions >>= \asserts -> do 
        cs <- use currentStruct
        pName <- case cs of
          Nothing -> do
            pure $ unpack defName

          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            let postFix = llvmName ("-" <> structBaseName) t'
            pure $ unpack defName <> postFix

        case def' of
          FunctionDef { funcRetType, funcParams, funcRecursive  } -> do
              
            params  <- mapM makeParam' . toList $ funcParams
            retType <- toLLVMType funcRetType
            params' <- recursiveParams (funcRecursive && asserts) -- recursion is verified if the assertions are enabled
            addDefinitions  [defineFunction pName (params' <> params) retType]

          ProcedureDef { procParams, procRecursive } -> do
            params  <- mapM (makeParam False) . toList $ procParams
            params' <- recursiveParams (procRecursive && asserts) -- recursion is verified if the assertions are enabled
            addDefinitions  [defineFunction pName (params' <> params) voidType]

          GracielaFunc{} -> pure () -- graciela native function should be declared here
                                    -- instead of declaring them manually

    else case def' of
      FunctionDef { funcBody, funcRetType, funcParams, funcRecursive, funcDecls } -> do
        doingFunction .= True
        func <- newLabel $ "func" <> unpack defName
        (func #)

        openScope

        params <- mapM makeParam' . toList $ funcParams
        mapM_ arrAux' funcParams

        mapM_ declaration funcDecls

        asserts <- use evalAssertions

        cond <- if asserts 
          then Just <$> precondition pre
          else pure Nothing 

        params' <- recursiveParams (funcRecursive && asserts) -- recursion is verified if the assertions are enabled

        cs <- use currentStruct
        returnType <- toLLVMType funcRetType
        let
          invariant' fn (name, t) | asserts = do
            name' <- getVariableName name
            exit  <- case cond of 
              Just cond' -> callInvariant fn cond' name' t Nothing
              Nothing -> pure Nothing
            when (isJust exit) $ (fromJust exit #)
          invariant' _ _ = pure ()

          dts  = filter (\(_, t) -> isJust (T.hasDT t) ) . toList $ funcParams
          
          body = do
            forM_ dts (invariant' "inv"    )
            forM_ dts (invariant' "coupInv")
            forM_ dts (invariant' "repInv" )
            expression' funcBody

          retVar returnOperand = do
            returnVar     <- insertVar defName

            addInstruction $ returnVar := Alloca
              { allocatedType = returnType
              , numElements   = Nothing
              , alignment     = 4
              , metadata      = [] }

            addInstruction $ Do Store
              { volatile = False
              , address  = LocalReference returnType returnVar
              , value    = returnOperand
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }



        (postFix, returnOperand) <- case cs of
          Nothing  -> do 
            returnOp <- body
            when asserts $ do 
              retVar returnOp
              postcondition (fromJust cond) post
            pure ("", returnOp)
          

          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            let postFix = llvmName ("-" <> structBaseName) t'

            let
              maybeProc = case abstractStruct of
                Just Struct {structProcs} -> defName `Map.lookup` structProcs
                Nothing -> error "Internal error: Missing Abstract Data Type."

            case maybeProc of
              Just Definition{ pre = pre', def' = AbstractFunctionDef {abstFDecl}} -> do
                mapM_ declaration abstFDecl
                when asserts $ preconditionAbstract (fromJust cond) pre' pos
              _ -> pure ()

            returnOp <- body
            when asserts $ retVar returnOp

            case maybeProc of
              Just Definition{post = post'} | asserts -> 
                postconditionAbstract (fromJust cond) post' pos
              _  -> pure ()

            when asserts $ postcondition (fromJust cond) post
            pure (postFix, returnOp)

        terminate Ret
          { returnOperand = Just returnOperand
          , metadata' = [] }

        let name = Name $ unpack defName <> postFix
        blocks' <- use blocks
        blocks .= Seq.empty

        addDefinition $ LLVM.GlobalDefinition functionDefaults
          { name        = name
          , parameters  = (params' <> params, False)
          , returnType
          , basicBlocks = toList blocks'
          }
        closeScope
        doingFunction .= False


      ProcedureDef { procDecl, procParams, procBody, procRecursive } -> do
        proc <- newLabel $ "proc" <> unpack defName
        (proc #)
        asserts <- use evalAssertions
        openScope

        params <- mapM (makeParam False) . toList $ procParams
        mapM_ declarationsOrRead procDecl
        mapM_ arrAux procParams
        
        cond <- if asserts 
          then Just <$> precondition pre
          else pure Nothing 

        params' <- recursiveParams (procRecursive && asserts) -- recursion is verified if the assertions are enabled
        cs <- use currentStruct
        when asserts $ forM_ procParams makeTempVar
        let
          invariant' fn (name, t, _) | asserts = do
            name' <- getVariableName name
            exit  <- case cond of 
              Just cond' -> callInvariant fn cond' name' t Nothing
              Nothing -> pure Nothing
            when (isJust exit) $ (fromJust exit #)
          invariant' _ _ = pure ()

          dts  = filter (\(_, t, _) -> isJust (T.hasDT t) ) . toList $ procParams
          
          body abstractStruct = do
            let
              maybeProc = case abstractStruct of
                Just Struct {structProcs} -> defName `Map.lookup` structProcs
                Nothing                   -> Nothing
            when asserts $ do
              forM_ dts (invariant' "coupInv")
              forM_ dts (invariant' "repInv" )

            let cond' = fromJust cond

            case maybeProc of
              Just Definition{ pre = pre', def' = AbstractProcedureDef{ abstPDecl }} | asserts  -> do
                mapM_ declaration abstPDecl
                preconditionAbstract cond' pre' pos
                                
              _ -> pure ()

            when asserts $ forM_ dts (invariant' "inv")

            instruction procBody

            when asserts $ do
              forM_ dts (invariant' "coupInv")

              forM_ dts (invariant' "inv"    )
              forM_ dts (invariant' "repInv" )
              case maybeProc of
                Just Definition{post = post'} -> 
                  postconditionAbstract cond' post' pos
                _                             -> pure ()

        pName <- case cs of
          Nothing -> do
            body Nothing

            pure $ unpack defName

          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            let postFix = llvmName ("-" <> structBaseName) t'

            body abstractStruct
            pure $ unpack defName <> postFix

        when asserts $ do
          postcondition (fromJust cond) post

        terminate $ Ret Nothing []

        blocks' <- use blocks

        addDefinition $ LLVM.GlobalDefinition functionDefaults
          { name        = Name pName
          , parameters  = (params' <> params,False)
          , returnType  = voidType
          , basicBlocks = toList blocks'
          }

        blocks .= Seq.empty
        closeScope

      GracielaFunc {} -> pure ()

  where

    makeTempVar' (name, t) = makeTempVar (name, t, T.In)
    makeTempVar :: (Text, T.Type, T.ArgMode) -> LLVM ()
    makeTempVar  (name, t, mode) = do
      let tempVarName = name <> "'"
      declaration Declaration
        { declLoc  = gracielaDef
        , declType = t
        , declIds  = Seq.fromList [tempVarName]
        }
      name' <- getVariableName tempVarName
      t'    <- toLLVMType t

      destVar <- objectRef O.Object 
        { O.loc = gracielaDef
        , O.objType = t
        , O.obj' = O.Variable
          { O.name = tempVarName
          , O.mode = Nothing } }

      sourceVar <- objectRef O.Object 
        { O.loc = gracielaDef
        , O.objType = t
        , O.obj' = O.Variable
          { O.name = name
          , O.mode = Just mode } }
      
      case t of 
        T.GArray {} -> copyArray t sourceVar destVar
        T.GDataType {} -> do
          types <- mapM fill (toList . T.typeArgs $ t)

          let
            copyFunc = "copy" <> llvmName (T.typeName t) types
          addInstruction $ Do Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable voidType copyFunc
            , arguments          = (,[]) <$> [ sourceVar
                                             , destVar ]
            , functionAttributes = []
            , metadata           = [] }

        _ -> do
          value <- newLabel "value"
          addInstruction $value := Load
            { volatile  = False
            , address   = sourceVar
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = [] }

          addInstruction $ Do Store
            { volatile = False
            , address  = destVar
            , value    = LocalReference t' value
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = [] }


    makeParam' (name, t) = makeParam True (name, t, T.In)
    makeParam isFunc (name, t, mode)  = do
      name' <- insertVar name
      t'    <- toLLVMType t
      if isFunc && (t =:= T.basic || t == T.I64 || t =:= T.highLevel || t =:= T.GATypeVar)
        then
          pure $ Parameter t' name' []
        else 
          pure $ Parameter (ptr t') name' []

    arrAux' (arr, t) = arrAux (arr, t, T.In) -- ArrAux for Functions
    arrAux  (arr, t@(T.GArray dims inner), mode) = do -- ArrAux for Procedures
      t'      <- toLLVMType t
      arrName <- getVariableName arr
      void $ foldM (dimAux t' arrName) 0 dims
      where
        dimAux t' arrName n dim = do
          paramDim <- expression dim

          dimAddr <- newUnLabel
          addInstruction $ dimAddr := GetElementPtr
            { inBounds = False
            , address  = LocalReference t' arrName
            , indices  =
              [ ConstantOperand (C.Int 32 0)
              , ConstantOperand (C.Int 32 n) ]
            , metadata = [] }

          argDim <- newLabel "arrCheck"
          addInstruction $ argDim := Load
            { volatile       = False
            , address        = LocalReference i32 dimAddr
            , maybeAtomicity = Nothing
            , alignment      = 4
            , metadata       = [] }

          arrCheckCmp <- newLabel "arrCheckCmp"
          addInstruction $ arrCheckCmp := ICmp
            { iPredicate = EQ
            , operand0 = paramDim
            , operand1 = LocalReference i32 argDim
            , metadata = [] }

          arrOk <- newLabel "arrOk"
          arrNotOk <- newLabel "arrNotOk"
          terminate CondBr
            { condition = LocalReference i1 arrCheckCmp
            , trueDest  = arrOk
            , falseDest = arrNotOk
            , metadata' = [] }

          (arrNotOk #)
          addInstruction $ Do Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable voidType writeIString
            , arguments          = [(paramDim,[])]
            , functionAttributes = []
            , metadata           = [] }

          addInstruction $ Do Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable voidType lnString
            , arguments          = []
            , functionAttributes = []
            , metadata           = [] }

          addInstruction $ Do Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable voidType writeIString
            , arguments          = [(LocalReference i32 argDim,[])]
            , functionAttributes = []
            , metadata           = [] }

          abort Abort.BadArrayArg (L.pos . loc $ dim)

          (arrOk #)
          pure $ n + 1

    arrAux _ = pure ()

    loadDtPtr name (T.GPointer t) exit callFunc = do
      yes     <- newLabel "yesNull"
      no      <- newLabel "noNull"
      cast    <- newLabel "cast"
      comp    <- newLabel "comp"
      argLoad <- newLabel "argLoad"
      type'   <- toLLVMType (T.GPointer t)
      exit'   <- case exit of
        Nothing -> newLabel "exit"
        Just e  -> pure e

      addInstruction $ argLoad := Load
            { volatile       = False
            , address        = LocalReference type' name
            , maybeAtomicity = Nothing
            , alignment      = 4
            , metadata       = [] }

      addInstruction $ cast := PtrToInt
            { operand0 = LocalReference type' argLoad
            , type'    = i64
            , metadata = [] }

      addInstruction $ comp := ICmp
              { iPredicate = EQ
              , operand0 = LocalReference intType cast
              , operand1 = ConstantOperand $ C.Int 64 0
              , metadata = [] }

      terminate CondBr
          { condition = LocalReference boolType comp
          , trueDest  = yes
          , falseDest = no
          , metadata' = [] }

      (no #)

      e <- callFunc argLoad t (Just exit')

      terminate Br
          { dest = exit'
          , metadata' = [] }

      (yes #)

      terminate Br
          { dest = exit'
          , metadata' = [] }

      pure e

    callCouple funName name t@(T.GPointer _) exit = do
      loadDtPtr name t exit (callCouple funName)


    callCouple funName name t exit | t =:= T.GADataType = do
      type' <- toLLVMType t
      t' <- mapM fill (toList $ T.typeArgs t)
      let postFix = llvmName ("-" <> T.typeName t) t'

      addInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType (funName <> postFix)
        , arguments          = [(LocalReference type' name,[])]
        , functionAttributes = []
        , metadata           = [] }

      pure exit



    callInvariant funName c name t@(T.GPointer _) exit = do
      loadDtPtr name t exit (callInvariant funName c)


    callInvariant funName cond name t exit = do
      type' <- toLLVMType t
      t' <- mapM fill (toList $ T.typeArgs t)
      let postFix = llvmName ("-" <> T.typeName t) t'


      addInstruction $ Do Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable voidType (funName <> postFix)
        , arguments          = [(LocalReference type' name,[]), (cond,[])]
        , functionAttributes = []
        , metadata           = [] }

      pure exit
    
    recursiveParams False = pure []
    recursiveParams True  = do
      let
        boundExp = fromMaybe
          (internal "boundless recursive function.")
          bound
        hasOldBound = Name $ "." <> unpack defName <> "HasOldBound"
        oldBound = Name $ "." <> unpack defName <> "OldBound"

      funcBodyLabel <- newLabel $ "func" <> unpack defName <> "Body"
      boundOperand <- expression boundExp

      gte0 <- newLabel "funcBoundGte0"
      addInstruction $ gte0 := ICmp
        { iPredicate = SGE
        , operand0   = boundOperand
        , operand1   = ConstantOperand $ C.Int 32 0
        , metadata   = [] }
      yesGte0 <- newLabel "funcGte0Yes"
      noGte0  <- newLabel "funcGte0No"
      terminate CondBr
        { condition = LocalReference boolType gte0
        , trueDest  = yesGte0
        , falseDest = noGte0
        , metadata' = [] }

      (noGte0 #)
      abort Abort.NegativeBound
        (let Location (pos, _) = loc boundExp in pos)

      (yesGte0 #)
      yesOld <- newLabel "funcOldBoundYes"
      noOld  <- newLabel "funcOldBoundNo"
      terminate CondBr
        { condition = LocalReference boolType hasOldBound
        , trueDest  = yesOld
        , falseDest = noOld
        , metadata' = [] }

      (noOld #)
      terminate Br
        { dest = funcBodyLabel
        , metadata' = [] }

      (yesOld #)
      ltOld <- newLabel "funcLtOld"
      addInstruction $ ltOld := ICmp
        { iPredicate = SLT
        , operand0   = boundOperand
        , operand1   = LocalReference intType oldBound
        , metadata   = [] }
      yesLtOld <- newLabel "funcLtOldBoundYes"
      noLtOld  <- newLabel "funcLtOldBoundNo"
      terminate CondBr
        { condition = LocalReference boolType ltOld
        , trueDest  = yesLtOld
        , falseDest = noLtOld
        , metadata' = [] }

      (noLtOld #)
      abort Abort.NonDecreasingBound
        (let Location (pos, _) = loc boundExp in pos)

      (yesLtOld #)
      terminate Br
        { dest = funcBodyLabel
        , metadata' = [] }

      (funcBodyLabel #)

      boundOp .= Just boundOperand
      pure [Parameter i1 hasOldBound [], Parameter i32 oldBound []]

    declarationsOrRead :: Either Declaration G.Instruction -> LLVM ()
    declarationsOrRead (Left decl)   = declaration decl
    declarationsOrRead (Right read') = instruction read'

    precondition :: Expression -> LLVM Operand
    precondition expr@ Expression {loc = Location (pos,_) } = do
        -- Create both labels
        trueLabel  <- newLabel "precondTrue"
        falseLabel <- newLabel "precondFalse"
        -- Evaluate the condition expression
        cond <- wrapBoolean expr
        -- Add the conditional branch
        terminate CondBr
          { condition = cond
          , trueDest  = trueLabel
          , falseDest = falseLabel
          , metadata' = [] }
        -- Set the false label to the warning, then continue normally
        (falseLabel #)
        warn Warning.Pre pos
        terminate Br
          { dest      = trueLabel
          , metadata' = [] }

        -- And the true label to the next instructions
        (trueLabel #)

        pure cond

    preconditionAbstract :: Operand -> Expression -> SourcePos -> LLVM ()
    preconditionAbstract precond expr pos = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "precondAbstTrue"
      falseLabel <- newLabel "precondAbstFalse"
      -- Evaluate the condition expression
      cond <- wrapBoolean expr

      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }

      -- Add the conditional branch
      (evaluate #)
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.BadAbstractCouple pos

      -- And the true label to the next instructions
      (trueLabel #)

    postconditionAbstract :: Operand -> Expression -> SourcePos -> LLVM ()
    postconditionAbstract precond expr pos = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "precondAbstTrue"
      falseLabel <- newLabel "precondAbstFalse"

      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }


      (evaluate #)
      -- Evaluate the condition expression
      cond <- wrapBoolean expr
      -- Add the conditional branch
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.AbstractPost pos

      -- And the true label to the next instructions
      (trueLabel #)

    postcondition :: Operand -> Expression -> LLVM ()
    postcondition precond expr@ Expression {loc = Location(pos,_)} = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "postcondTrue"
      falseLabel <- newLabel "postcondFalse"

      -- Create the conditional branch
      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }

      (evaluate #)
      -- Evaluate the condition expression
      cond <- wrapBoolean expr
      -- Add the conditional branch
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.Post pos
      -- And the true label to the next instructions

      (trueLabel #)


preDefinitions :: [String] -> LLVM ()
preDefinitions files = do
  mapM_ addFile files
  addDefinitions 


    [ defineFunction copyArrayString [ parameter ("size"     , intType)
                                     , parameter ("arrSource", pointerType)
                                     , parameter ("arrDest"  , pointerType)
                                     , parameter ("sizeT"    , intType) ]
                                     voidType
    -- Trace pseudo Functions
    , defineFunction traceIntString         intParam intType
    , defineFunction traceFloatString       floatParam floatType
    , defineFunction traceCharString        charParam charType
    , defineFunction traceBoolString        boolParam boolType
    , defineFunction traceStringIntString   [ parameter ("x", stringType)
                                            , parameter ("y", intType) ]
                                            intType
    , defineFunction traceStringFloatString [ parameter ("x", stringType)
                                            , parameter ("y", floatType) ]
                                            floatType
    , defineFunction traceStringCharString  [ parameter ("x", stringType)
                                            , parameter ("y", charType) ]
                                            charType
    , defineFunction traceStringBoolString  [ parameter ("x", stringType)
                                            , parameter ("y", boolType) ]
                                            boolType

    -- Conversion functions
    , defineFunction float2intString  [ parameter ("x", floatType)
                                      , parameter ("line", intType)
                                      , parameter ("column", intType) ]
                                      intType
    , defineFunction pointer2intString [parameter ("x", floatType)
                                      , parameter ("line", intType)
                                      , parameter ("column", intType) ]
                                      intType
    , defineFunction char2intString   charParam intType
    , defineFunction float2charString [ parameter ("x", floatType)
                                      , parameter ("line", intType)
                                      , parameter ("column", intType) ]
                                      charType
    , defineFunction int2charString   [ parameter ("x", intType)
                                      , parameter ("line", intType)
                                      , parameter ("column", intType) ]
                                      charType
    , defineFunction char2floatString charParam floatType
    , defineFunction int2floatString  intParam  floatType

    -- Polymorphic functions
    , defineFunction sqrtIString [ parameter ("x", intType)
                                 , parameter ("line", intType)
                                 , parameter ("column", intType) ]
                                 intType
    , defineFunction sqrtFString [ parameter ("x", floatType)
                                 , parameter ("line", intType)
                                 , parameter ("column", intType)]
                                 floatType
    , defineFunction absIString  [ parameter ("x", intType)
                                 , parameter ("line", intType)
                                 , parameter ("column", intType)]
                                 intType

    , defineFunction isNanString  [ parameter ("x", floatType) ] boolType
    , defineFunction isInfString  [ parameter ("x", floatType) ] boolType

    , defineFunction absFString               floatParam floatType
    , defineFunction toSetMultiString         ptrParam   pointerType
    , defineFunction toSetSeqString           ptrParam   pointerType
    , defineFunction toSetFuncString          ptrParam   pointerType
    , defineFunction toSetRelString           ptrParam   pointerType
    , defineFunction toMultiSetString         ptrParam   pointerType
    , defineFunction toMultiSeqString         ptrParam   pointerType
    , defineFunction toSeqSetString           ptrParam   pointerType
    , defineFunction toSeqMultiString         ptrParam   pointerType

--------------------------------------------------------------------------------


    , defineFunction firstSetString           ptrParam (ptr iterator)
    , defineFunction nextSetString            [parameter ("x", ptr iterator)]
                                              (ptr iterator)

    , defineFunction firstMultisetString      ptrParam (ptr iterator)
    , defineFunction nextMultisetString       [parameter ("x", ptr iterator)]
                                              (ptr iterator)

    , defineFunction firstSequenceString      ptrParam (ptr iterator)
    , defineFunction nextSequenceString       [parameter ("x", ptr iterator)]
                                              (ptr iterator)

    , defineFunction initTrashCollectorString [] voidType
    , defineFunction freeTrashCollectorString [] voidType
    , defineFunction openScopeString          [] voidType

    -- (Bi)Functors
    , defineFunction newSetString             [] pointerType
    , defineFunction newSeqString             [] pointerType
    , defineFunction newMultisetString        [] pointerType

    , defineFunction newSetPairString         [] pointerType
    , defineFunction newMultisetPairString    [] pointerType
    , defineFunction newSeqPairString         [] pointerType

    , defineFunction newFunction              [] pointerType
    , defineFunction newRelation              [] pointerType

--------------------------------------------------------------------------------

    , defineFunction equalSetString            ptrParam2 boolType
    , defineFunction equalSeqString            ptrParam2 boolType
    , defineFunction equalMultisetString       ptrParam2 boolType

    , defineFunction equalSetPairString        ptrParam2 boolType
    , defineFunction equalSeqPairString        ptrParam2 boolType
    , defineFunction equalMultisetPairString   ptrParam2 boolType

    , defineFunction equalFuncString           ptrParam2 boolType
    , defineFunction equalRelString            ptrParam2 boolType

    , defineFunction equalTupleString            [ parameter ("x", ptr tupleType)
                                               , parameter ("y", ptr tupleType)]
                                               boolType
--------------------------------------------------------------------------------
    , defineFunction sizeSetString             ptrParam intType
    , defineFunction sizeSeqString             ptrParam intType
    , defineFunction sizeMultisetString        ptrParam intType
    , defineFunction sizeRelString             ptrParam intType
    , defineFunction sizeFuncString            ptrParam intType
--------------------------------------------------------------------------------
    , defineFunction supersetSetString         ptrParam2 boolType
    , defineFunction supersetMultisetString    ptrParam2 boolType
    , defineFunction ssupersetSetString        ptrParam2 boolType
    , defineFunction ssupersetMultisetString   ptrParam2 boolType

    , defineFunction supersetSetPairString         ptrParam2 boolType
    , defineFunction supersetMultisetPairString    ptrParam2 boolType
    , defineFunction ssupersetSetPairString        ptrParam2 boolType
    , defineFunction ssupersetMultisetPairString   ptrParam2 boolType
--------------------------------------------------------------------------------
    , defineFunction insertSetString           ptri64Param voidType
    , defineFunction insertSeqString           ptri64Param voidType
    , defineFunction insertMultisetString      ptri64Param voidType

    , defineFunction insertSetPairString       ptrTupleParam voidType
    , defineFunction insertSeqPairString       ptrTupleParam voidType
    , defineFunction insertMultisetPairString  ptrTupleParam voidType
--------------------------------------------------------------------------------

    , defineFunction isElemSetString          ptri64Param boolType
    , defineFunction isElemMultisetString     ptri64Param boolType
    , defineFunction isElemSeqString          ptri64Param boolType

    , defineFunction isElemSetPairString      ptrTupleParam boolType
    , defineFunction isElemMultisetPairString ptrTupleParam boolType
    , defineFunction isElemSeqPairString      ptrTupleParam boolType
--------------------------------------------------------------------------------
    , defineFunction unionSetString           ptrParam2 pointerType
    , defineFunction intersectSetString       ptrParam2 pointerType
    , defineFunction differenceSetString      ptrParam2 pointerType

    , defineFunction unionSetPairString       ptrParam2 pointerType
    , defineFunction intersectSetPairString   ptrParam2 pointerType
    , defineFunction differenceSetPairString  ptrParam2 pointerType

    , defineFunction unionMultisetString      ptrParam2 pointerType
    , defineFunction intersectMultisetString  ptrParam2 pointerType
    , defineFunction differenceMultisetString ptrParam2 pointerType

    , defineFunction unionMultisetPairString      ptrParam2 pointerType
    , defineFunction intersectMultisetPairString  ptrParam2 pointerType
    , defineFunction differenceMultisetPairString ptrParam2 pointerType

    , defineFunction unionFunctionString      [ parameter ("x", pointerType)
                                              , parameter ("y", pointerType)
                                              , parameter ("line", intType)
                                              , parameter ("column", intType)]
                                              pointerType
    , defineFunction intersectFunctionString  ptrParam2 pointerType
    , defineFunction differenceFunctionString ptrParam2 pointerType

--------------------------------------------------------------------------------
    , defineFunction multisetSumString            ptrParam2 pointerType
    , defineFunction concatSequenceString         ptrParam2 pointerType

    , defineFunction multiplicityMultiString      [ parameter ("x", i64)
                                                  , parameter ("y", pointerType)]
                                                  intType
    , defineFunction multiplicitySeqString        [ parameter ("x", i64)
                                                  , parameter ("y", pointerType)]
                                                  intType

    , defineFunction multisetPairSumString        ptrParam2 pointerType
    , defineFunction concatSequencePairString     ptrParam2 pointerType

    , defineFunction multiplicityMultiPairString  [ parameter ("x", ptr tupleType)
                                                  , parameter ("y", pointerType)]
                                                  intType

    , defineFunction multiplicitySeqPairString    [ parameter ("x", ptr tupleType)
                                                  , parameter ("y", pointerType)]
                                                  intType

    , defineFunction atSequenceString             [ parameter ("x", pointerType)
                                                  , parameter ("y", intType)
                                                  , parameter ("line", intType)
                                                  , parameter ("column", intType)]
                                                  i64
    , defineFunction atSequencePairString         [ parameter ("x", pointerType)
                                                  , parameter ("y", intType)
                                                  , parameter ("line", intType)
                                                  , parameter ("column", intType)]
                                                  tupleType

--------------------------------------------------------------------------------
    , defineFunction relString                ptrParam   pointerType
    , defineFunction funcString               [ parameter ("x", pointerType)
                                              , parameter ("line", intType)
                                              , parameter ("column", intType)]
                                              pointerType

    , defineFunction domainFuncString         ptrParam    pointerType
    , defineFunction domainRelString          ptrParam    pointerType

    , defineFunction codomainFuncString       ptrParam    pointerType
    , defineFunction codomainRelString        ptrParam    pointerType

    , defineFunction evalFuncString           [ parameter ("x", pointerType)
                                              , parameter ("y", i64)
                                              , parameter ("line", intType)
                                              , parameter ("column", intType)]  i64
    , defineFunction evalRelString            ptri64Param pointerType

    , defineFunction inverseFuncString        ptrParam    pointerType
    , defineFunction inverseRelString         ptrParam    pointerType


--------------------------------------------------------------------------------
    -- Abort
    , defineFunction abortString [ parameter ("x", intType)
                                 , parameter ("line", intType)
                                 , parameter ("column", intType)]
                                 voidType
    , defineFunction warnString [ parameter ("x", intType)
                                , parameter ("line", intType)
                                , parameter ("column", intType)]
                                voidType
    -- Min and max
    , defineFunction minnumString intParams2 intType
    , defineFunction maxnumString intParams2 intType

    -- Line feed
    , defineFunction lnString [] voidType

    -- Bool Write
    , defineFunction writeBString boolParam voidType

    -- Char Write
    , defineFunction writeCString charParam voidType

    -- Float Write
    , defineFunction writeFString floatParam voidType

    -- Int Write
    , defineFunction writeIString intParam voidType

    -- Pointer Write
    , defineFunction writePString ptrParam voidType

    -- String Write
    , defineFunction writeSString stringParam voidType

    -- Square Root and absolute value
    , defineFunction sqrtString    floatParam floatType
    , defineFunction fabsString    floatParam floatType

    , defineFunction minnumFstring  floatParams2 floatType
    , defineFunction maxnumFstring  floatParams2 floatType
    , defineFunction powIString     [ parameter ("x", intType)
                                    , parameter ("y", intType)
                                    , parameter ("line", intType)
                                    , parameter ("column", intType)]
                                    intType
    , defineFunction powString      floatParams2 floatType

    , defineFunction (safeSub 64) (fmap parameter [("x",i64), ("y",i64)]) (overflow' 64)
    , defineFunction (safeMul 64) (fmap parameter [("x",i64), ("y",i64)]) (overflow' 64)
    , defineFunction (safeAdd 64) (fmap parameter [("x",i64), ("y",i64)]) (overflow' 64)

    , defineFunction (safeSub 32) intParams2 (overflow' 32)
    , defineFunction (safeMul 32) intParams2 (overflow' 32)
    , defineFunction (safeAdd 32) intParams2 (overflow' 32)

    , defineFunction (safeSub  8) charParams2 (overflow' 8)
    , defineFunction (safeMul  8) charParams2 (overflow' 8)
    , defineFunction (safeAdd  8) charParams2 (overflow' 8)

    -- Read
    , defineFunction readIntStd    [] intType
    , defineFunction readBoolStd   [] boolType
    , defineFunction readCharStd   [] charType
    , defineFunction readFloatStd  [] floatType
    , defineFunction readlnString  [parameter ("ptr", ptr $ ptr i32)] pointerType

    -- Rand
    , defineFunction randInt   [] intType
    , defineFunction randBool  [] boolType
    , defineFunction randChar  [] charType
    , defineFunction randFloat [] floatType

    -- , defineFunction randomize  [] voidType
    -- , defineFunction seedRandom [intParam] voidType

    -- Malloc
    , defineFunction mallocString   intParam pointerType
    -- , defineFunction mallocTCString intParam pointerType

    , defineFunction freeString [ parameter ("ptr", pointerType)
                                , parameter ("line", intType)
                                , parameter ("column", intType)]
                                voidType

    , defineFunction addPointerString     [ parameter ("ptr", pointerType)
                                          , parameter ("line", intType)
                                          , parameter ("column", intType)]
                                          voidType
    , defineFunction removePointerString  [ parameter ("ptr", pointerType)
                                          , parameter ("line", intType)
                                          , parameter ("column", intType)]
                                          voidType
    , defineFunction derefPointerString   [ parameter ("ptr", pointerType)
                                          , parameter ("pragma", boolType)
                                          , parameter ("line", intType)
                                          , parameter ("column", intType)]
                                          voidType


    , defineFunction readFileInt   [parameter ("file", pointerType)] intType
    , defineFunction readFileBool  [parameter ("file", pointerType)] boolType
    , defineFunction readFileChar  [parameter ("file", pointerType)] charType
    , defineFunction readFileFloat [parameter ("file", pointerType)] floatType
    , defineFunction closeFileStr  [parameter ("file", pointerType)] voidType
    , defineFunction openFileStr   [parameter ("name", pointerType)] pointerType
    ]

  where
    parameter (name, t) = Parameter t (Name name) []
    intParam      = [parameter ("x",     intType)]
    charParam     = [parameter ("x",    charType)]
    boolParam     = [parameter ("x",    boolType)]
    floatParam    = [parameter ("x",   floatType)]
    ptrParam      = [parameter ("x", pointerType)]
    ptrParam2     = [parameter ("x", pointerType), parameter ("y", pointerType)]
    ptri64Param   = [parameter ("x", pointerType), parameter ("y", i64)]
    ptrTupleParam = [parameter ("x", pointerType), parameter ("y", ptr tupleType)]
    intParams2    = fmap parameter [("x",   intType), ("y",   intType)]
    charParams2   = fmap parameter [("x",  charType), ("y",  charType)]
    floatParams2  = fmap parameter [("x", floatType), ("y", floatType)]
    stringParam   = [Parameter stringType (Name "msg") [NoCapture]]
    overflow' n   = StructureType False [IntegerType n, boolType]
    addFile file  = addDefinition $ LLVM.GlobalDefinition GlobalVariable
        { name            = Name ("__" <> file)
        , linkage         = Private
        , visibility      = Default
        , dllStorageClass = Nothing
        , threadLocalMode = Nothing
        , addrSpace       = AddrSpace 0
        , hasUnnamedAddr  = False
        , isConstant      = False
        , type'           = pointerType
        , initializer     = Just . C.Null $ pointerType
        , section         = Nothing
        , comdat          = Nothing
        , alignment       = 4
      }

defineFunction name params t = LLVM.GlobalDefinition $ functionDefaults
  { name        = Name name
  , parameters  = (params, False)
  , returnType  = t
  , basicBlocks = [] }