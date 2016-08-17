{-# LANGUAGE NamedFieldPuns #-}
module LLVM.Definition

where 

--------------------------------------------------------------------------------
import           Aborts
import qualified AST.Type                                as T
import           AST.Instruction                         (Instruction)
import           AST.Definition                          
import           LLVM.Instruction
import           LLVM.Expression
import           LLVM.State
import           LLVM.Type
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Data.Text                               (Text,unpack)
import           Data.Word
import           Data.Sequence                           as Seq (empty)
import           Data.Foldable                           (toList)
import           Data.Monoid                             ((<>))
import qualified LLVM.General.AST                        as LLVM (Definition(..))
import           LLVM.General.AST                        (Terminator(..),
                                                          Parameter(..),
                                                          BasicBlock(..),
                                                          Named(..))
import           LLVM.General.AST                        (functionDefaults)
import           LLVM.General.AST.Global                 (Global(..), functionDefaults)
import           LLVM.General.AST.Name                   (Name(..))
import qualified LLVM.General.AST.Type                   as LLVM (Type)
import           LLVM.General.AST.Type                   (double, ptr)
import           LLVM.General.AST.ParameterAttribute     (ParameterAttribute(..))
import           LLVM.General.AST.Type                   (Type(StructureType))
--------------------------------------------------------------------------------

{- Given the instruction blokc of the main program, construct the main LLVM function-}
mainDefinition :: Instruction -> LLVM LLVM.Definition
mainDefinition insts = do
  instruction insts
  blocks' <- use blocks
  blocks .= Seq.empty
  currentBlock .= Seq.empty
  label <- newLabel
  let terminator = Do $ Ret Nothing []
  return $ LLVM.GlobalDefinition $ functionDefaults
        { name        = Name "main"
        , parameters  = ([], False)
        , returnType  = voidType
        , basicBlocks = toList blocks'
        }

{- Translate a definition from Graciela AST to LLVM AST -}
definition :: Definition -> LLVM LLVM.Definition
definition Definition {defName, params, st, def'} = case def' of 
  FunctionDef {funcBody, retType} -> do 
    operand <- expression funcBody
    let name = Name $ unpack defName
    blocks' <- use blocks
    blocks .= Seq.empty
    currentBlock .= Seq.empty
    return $ LLVM.GlobalDefinition $ functionDefaults
        { name        = name
        , parameters  = (fmap toLLVMParameter params, False)
        , returnType  = toLLVMType retType
        , basicBlocks = toList blocks'
        }
        
  ProcedureDef {procDecl, pre, procBody, post} -> do
    pre'       <- expression pre
    post       <- expression post
    instruction procBody
    blocks' <- use blocks
    blocks .= Seq.empty
    label <- newLabel
    let terminator = Do $ Ret Nothing []
    return $ LLVM.GlobalDefinition $ functionDefaults
        { name        = Name (unpack defName)
        , parameters  = (fmap toLLVMParameter params, False)
        , returnType  = voidType
        , basicBlocks = toList blocks'
        }
  where 
    toLLVMParameter (name, t) = Parameter (toLLVMType t) (Name (unpack name)) []




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


preDefinitions :: [String] -> LLVM [LLVM.Definition]
preDefinitions files = return [
  -- Random
    declareFunction randomInt [] intType
  -- Abort
  , declareFunction abortString [ parameter ("x", intType)
                                , parameter ("line", intType)
                                , parameter ("column", intType)] 
                                voidType
  -- Min and max
  , declareFunction minnumString intParams2 intType
  , declareFunction maxnumString intParams2 intType
  
  -- Bool Write and Writeln
  , declareFunction writeLnBool boolParam voidType
  , declareFunction writeBool   boolParam voidType
  
  -- Char Write and Writeln
  , declareFunction writeLnChar charParam voidType
  , declareFunction writeChar   charParam voidType
  
  -- Float Write and Writeln
  , declareFunction writeLnFloat floatParam voidType
  , declareFunction writeFloat   floatParam voidType
  
  -- Int Write and Writeln 
  , declareFunction writeLnInt intParam voidType
  , declareFunction writeInt   intParam voidType
  
  -- String Write and Writeln
  , declareFunction writeLnString stringParam intType
  , declareFunction writeString   stringParam intType
  
  -- Square Root and absolute value
  , declareFunction sqrtString    floatParam floatType
  , declareFunction fabsString    floatParam floatType

  , declareFunction minnumFstring  floatParams2 floatType
  , declareFunction maxnumFstring  floatParams2 floatType
  , declareFunction powString      floatParams2 floatType


  , declareFunction intSub intParams2 overflow'
  , declareFunction intMul intParams2 overflow'
  , declareFunction intAdd intParams2 overflow'

  -- Read 
  , declareFunction readIntStd    [] intType
  , declareFunction readCharStd   [] charType
  , declareFunction readFloatStd  [] floatType

  , declareFunction openFileStr [Parameter (ptr pointerType) (Name "nombreArchivo") []] (ptr pointerType)

  -- mapM_ addFile files

  -- addDefinition readFileInt    (createEmptyParameters [(Name "f", ptr pointerType)]) intType
  -- addDefinition readFileChar   (createEmptyParameters [(Name "f", ptr pointerType)]) charType
  -- addDefinition readFileFloat (createEmptyParameters [(Name "f", ptr pointerType)]) floatType
  -- addDefinition closeFileStr   (createEmptyParameters [(Name "f", ptr pointerType)]) voidType
  ]

  where 
      declareFunction name params t = LLVM.GlobalDefinition $ functionDefaults
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
