{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PostfixOperators         #-}

module LLVM.Definition where
--------------------------------------------------------------------------------
import           AST.Declaration                     (Declaration)
import           AST.Definition
import           AST.Expression                      (Expression (..))
import qualified AST.Instruction                     as G (Instruction)
import           LLVM.Abort                          (abort, abortString, warn,
                                                      warnString)
import qualified LLVM.Abort                          as Abort (Abort (NegativeBound, NondecreasingBound, Post))
import qualified LLVM.Abort                          as Warning (Warning (Pre))
import           LLVM.Declaration                    (declaration)
import           LLVM.Expression
import           LLVM.Instruction
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           Location
import           Treelike
import           Type                                ((=:=))
import qualified Type                                as T
--------------------------------------------------------------------------------
import           Control.Lens                        (use, (%=), (.=))
import           Control.Monad                       (unless)
import           Data.Foldable                       (toList)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Sequence                       as Seq (empty, fromList)
import qualified Data.Sequence                       as Seq (empty)
import           Data.Text                           (Text, unpack)
import           Data.Word
import           LLVM.General.AST                    (BasicBlock (..),
                                                      Named (..),
                                                      Parameter (..),
                                                      Terminator (..),
                                                      functionDefaults)
import qualified LLVM.General.AST                    as LLVM (Definition (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Constant           as C
import           LLVM.General.AST.Global             (Global (..),
                                                      functionDefaults)
import           LLVM.General.AST.Instruction
import           LLVM.General.AST.IntegerPredicate   (IntegerPredicate (SGE, SLT))
import           LLVM.General.AST.Name               (Name (..))
import           LLVM.General.AST.Operand            (MetadataNode (..),
                                                      Operand (..))
import           LLVM.General.AST.ParameterAttribute (ParameterAttribute (..))
import           LLVM.General.AST.Type               (Type (..), double, i1,
                                                      i32, i8, ptr)
import qualified LLVM.General.AST.Type               as LLVM (Type)
--------------------------------------------------------------------------------
import           Debug.Trace

{- Given the instruction blokc of the main program, construct the main LLVM function-}
mainDefinition :: G.Instruction -> LLVM ()
mainDefinition block = do
  main <- newLabel "main"

  (main #)
  instruction block
  terminate' $ Ret (Just . ConstantOperand $ C.Int 32 0) []

  blocks' <- use blocks
  blocks .= Seq.empty
  addDefinition $ LLVM.GlobalDefinition functionDefaults
        { name        = Name "main"
        , parameters  = ([], False)
        , returnType  = i32
        , basicBlocks = toList blocks'
        }

{- Translate a definition from Graciela AST to LLVM AST -}
definition :: Definition -> LLVM ()
definition
  Definition { defName, def', pre, post, bound, defLoc = Location (pos, _to) }
  = case def' of
    FunctionDef { funcBody, funcRetType, funcParams, funcRecursive } -> do
      func <- newLabel $ "func" <> unpack defName
      (func #)

      -- TODO! postcondition
      openScope

      params <- mapM toLLVMParameter . toList $ funcParams

      preOp <- expression pre
      yesPre <- newLabel $ "func" <> unpack defName <> "PreYes"
      noPre  <- newLabel $ "func" <> unpack defName <> "PreNo"
      terminate' CondBr
        { condition = preOp
        , trueDest  = yesPre
        , falseDest = noPre
        , metadata' = [] }

      (noPre #)
      warn Warning.Pre pos
      terminate' Br
        { dest      = yesPre
        , metadata' = [] }

      (yesPre #)

      params' <- if funcRecursive
        then do
          -- Parameter t' (Name name') []
          let
            boundExp = fromMaybe
              (error "internal error: boundless recursive function.")
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
          terminate' CondBr
            { condition = LocalReference boolType gte0
            , trueDest  = yesGte0
            , falseDest = noGte0
            , metadata' = [] }

          (noGte0 #)
          abort Abort.NegativeBound pos

          (yesGte0 #)
          yesOld <- newLabel "funcOldBoundYes"
          noOld  <- newLabel "funcOldBoundNo"
          terminate' CondBr
            { condition = LocalReference boolType hasOldBound
            , trueDest  = yesOld
            , falseDest = noOld
            , metadata' = [] }

          (noOld #)
          terminate' Br
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
          terminate' CondBr
            { condition = LocalReference boolType ltOld
            , trueDest  = yesLtOld
            , falseDest = noLtOld
            , metadata' = [] }

          (noLtOld #)
          abort Abort.NondecreasingBound pos

          (yesLtOld #)
          terminate' Br
            { dest = funcBodyLabel
            , metadata' = [] }

          (funcBodyLabel #)

          boundOp .= Just boundOperand
          pure [Parameter i1 hasOldBound [], Parameter i32 oldBound []]

        else pure []

      returnOperand <- expression funcBody

      returnVar <- Name <$> insertName (unpack defName)

      returnType <- toLLVMType funcRetType
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

      postOperand <- expression post
      yesPost <- newLabel "funcPostYes"
      noPost  <- newLabel "funcPostNo"
      terminate' CondBr
        { condition = postOperand
        , trueDest  = yesPost
        , falseDest = noPost
        , metadata' = [] }

      (noPost #)
      abort Abort.Post pos

      (yesPost #)
      terminate' Ret
        { returnOperand = Just returnOperand
        , metadata' = [] }

      let name = Name $ unpack defName
      blocks' <- use blocks
      blocks .= Seq.empty

      addDefinition $ LLVM.GlobalDefinition functionDefaults
        { name        = name
        , parameters  = (params' <> params, False)
        , returnType
        , basicBlocks = toList blocks'
        }
      closeScope


    ProcedureDef { procDecl, procParams, procBody } -> do
      proc <- newLabel $ "proc" <> unpack defName
      (proc #)

      openScope

      params <- mapM toLLVMParameter' . toList $ procParams

      mapM_ declarationsOrRead procDecl
      precondition pre
      instruction procBody
      postcondition post

      blocks' <- use blocks
      blocks .= Seq.empty

      addDefinition $ LLVM.GlobalDefinition functionDefaults
          { name        = Name (unpack defName)
          , parameters  = (params,False)
          , returnType  = voidType
          , basicBlocks = toList blocks'
          }
      closeScope

  where
    toLLVMParameter (name, t) = do
      name' <- insertName $ unpack name
      t'    <- toLLVMType t
      return $ Parameter t' (Name name') []

    toLLVMParameter' (name, t, mode) | mode == T.In &&
          t =:= T.GOneOf [T.GBool,T.GChar,T.GInt,T.GFloat] = do
      name' <- insertName $ unpack name
      t'    <- toLLVMType t
      return $ Parameter t' (Name name') []

    toLLVMParameter' (name, t, mode) = do
      name' <- insertName $ unpack name
      t'    <- toLLVMType t
      return $ Parameter (ptr t') (Name name') []


declarationsOrRead :: Either Declaration G.Instruction -> LLVM ()
declarationsOrRead (Left decl)   = declaration decl
declarationsOrRead (Right read') = instruction read'

precondition :: Expression -> LLVM ()
precondition expr@ Expression {loc = Location (pos,_) } = do
    -- Evaluate the condition expression
    cond <- expression expr
    -- Create both label
    trueLabel  <- newLabel "precondTrue"
    falseLabel <- newLabel "precondFalse"
    -- Create the conditional branch
    terminate' CondBr
      { condition = cond
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = [] }
    -- Set the false label to the warning, then continue normally
    (falseLabel #)
    warn Warning.Pre pos
    terminate' Br
      { dest      = trueLabel
      , metadata' = [] }

    -- And the true label to the next instructions
    (trueLabel #)

postcondition :: Expression -> LLVM ()
postcondition expr@ Expression {loc = Location(pos,_)} = do
  -- Evaluate the condition expression
  cond <- expression expr
  -- Create both labels
  trueLabel  <- newLabel "postcondTrue"
  falseLabel <- newLabel "postcondFalse"
  -- Create the conditional branch
  terminate' CondBr
    { condition = cond
    , trueDest  = trueLabel
    , falseDest = falseLabel
    , metadata' = [] }
  -- Set the false label to the abort
  (falseLabel #)
  abort Abort.Post pos
  -- And the true label to the next instructions

  (trueLabel #)
  terminate' $ Ret Nothing []

  -- nextLabel <- newLabel
  -- (nextLabel #)


-- createParameters :: [(Name, Type)]
--                  -> [[LLVM.ParameterAttribute]]
--                  -> ([LLVM.Parameter], Bool)
-- createParameters names attrs = (zipWith parameters' names attrs, False)
--   where
--     parameters' (name, t) attr = LLVM.Parameter t name attr

--     parameters' (name, t) = LLVM.Parameter t name []

-- createDef :: Definition -> LLVM ()
-- createDef Definition { name, st, params, def' } = case def' of
--   ProcedureDef { constDec, pre, procbody, post } -> do
--     let name' = TE.unpack name
--     let args  = map (\(id, _) -> (TE.unpack id, fromJust $ checkSymbol id st)) params
--     let args' = ([LLVM.Parameter t (Name id) [] | (id, t) <- convertParams args], False)
--     retTy <- retVoid
--     addArgOperand args
--     mapM_ accToAlloca constDec
--     createState name' pre
--     createInstruction procbody
--     retVarOperand $ reverse args
--     createState name' post
--     addBasicBlock retTy
--     addDefinition name' args' voidType
--     where
--       parameters' params = [LLVM.Parameter t (Name id) [] | (id, t) <- convertParams params]

--   FunctionDef { funcbody, retType } -> do
--     let args' = ([Parameter t (Name id) [] | (id, t) <- convertFuncParams params], False)
--     mapM_ addFuncParam params
--     exp'  <- createExpression funcbody
--     n <- newLabel
--     let retTy = n := Ret (Just exp') []
--     addBasicBlock retTy
--     addDefinition (TE.unpack name) args' (toType retType)


preDefinitions :: [String] -> LLVM ()
preDefinitions files =
  addDefinitions $ fromList [
    -- Random
      defineFunction randomInt [] intType
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

    -- String Write
    , defineFunction writeSString stringParam voidType

    -- Square Root and absolute value
    , defineFunction sqrtString    floatParam floatType
    , defineFunction fabsString    floatParam floatType

    , defineFunction minnumFstring  floatParams2 floatType
    , defineFunction maxnumFstring  floatParams2 floatType
    , defineFunction powString      floatParams2 floatType


    , defineFunction intSub intParams2 overflow'
    , defineFunction intMul intParams2 overflow'
    , defineFunction intAdd intParams2 overflow'

    -- Read
    , defineFunction readIntStd    [] intType
    , defineFunction readCharStd   [] charType
    , defineFunction readFloatStd  [] floatType

    , defineFunction openFileStr [Parameter (ptr pointerType) (Name "nombreArchivo") []] (ptr pointerType)

    -- Malloc
    , defineFunction mallocString intParam (ptr pointerType)
    , defineFunction freeString [parameter ("x", ptr pointerType)] voidType
    -- mapM_ addFile files

    -- addDefinition readFileInt    (createEmptyParameters [(Name "f", ptr pointerType)]) intType
    -- addDefinition readFileChar   (createEmptyParameters [(Name "f", ptr pointerType)]) charType
    -- addDefinition readFileFloat (createEmptyParameters [(Name "f", ptr pointerType)]) floatType
    -- addDefinition closeFileStr   (createEmptyParameters [(Name "f", ptr pointerType)]) voidType
    ]

  where
    defineFunction name params t = LLVM.GlobalDefinition $ functionDefaults
      { name        = Name name
      , parameters  = (params, False)
      , returnType  = t
      , basicBlocks = []
      }
    parameter (name, t) = Parameter t (Name name) []
    intParam      = [parameter ("x",   intType)]
    charParam     = [parameter ("x",  charType)]
    boolParam     = [parameter ("x",  boolType)]
    floatParam    = [parameter ("x", floatType)]
    intParams2    = fmap parameter [("x",   intType), ("y",   intType)]
    floatParams2  = fmap parameter [("x", floatType), ("y", floatType)]
    stringParam   = [Parameter stringType (Name "msg") [NoCapture]]
    overflow'     = StructureType False [intType, boolType]
