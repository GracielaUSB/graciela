{-# LANGUAGE NamedFieldPuns #-}
module LLVM.Definition

where 

--------------------------------------------------------------------------------
import           Aborts
import qualified Type                                as T
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
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Control.Monad                           (unless)
import           Data.Sequence                           as Seq (empty,fromList)
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
import           LLVM.General.AST.Type                   (Type(..),i8)
import           LLVM.General.AST.AddrSpace
import           Debug.Trace
--------------------------------------------------------------------------------

{- Given the instruction blokc of the main program, construct the main LLVM function-}
mainDefinition :: Instruction -> LLVM ()
mainDefinition insts = do
  instruction insts
  blocks' <- use blocks
  blocks .= Seq.empty
  currentBlock .= Seq.empty
  label <- newLabel
  let terminator = Do $ Ret Nothing []
  addDefinition $ LLVM.GlobalDefinition functionDefaults
        { name        = Name "main"
        , parameters  = ([], False)
        , returnType  = voidType
        , basicBlocks = toList blocks'
        }

{- Translate a definition from Graciela AST to LLVM AST -}
definition :: Definition -> LLVM ()
definition Definition {defName, params, st, def'} = case def' of 
  FunctionDef {funcBody, retType} -> do 
    operand <- expression funcBody
    let name = Name $ unpack defName
    blocks' <- use blocks
    blocks .= Seq.empty
    currentBlock .= Seq.empty
    addDefinition $ LLVM.GlobalDefinition functionDefaults
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
    addDefinition $ LLVM.GlobalDefinition functionDefaults
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


defineType :: (Text, (T.Type, a)) -> LLVM ()
defineType (name, (t, _)) = do 
  let 
    name' = Name (unpack name)
    t'    = Just $ toLLVMType t
    def   = LLVM.TypeDefinition name' t'
  unless (t `elem` [T.GBool, T.GInt, T.GFloat, T.GChar]) $ do 
    addDefinition def

preDefinitions :: [String] -> LLVM ()
preDefinitions files = do 
  addDefinitions $ fromList [
      -- Random
        defineFunction randomInt [] intType
      -- Abort
      , defineFunction abortString [ parameter ("x", intType)
                                    , parameter ("line", intType)
                                    , parameter ("column", intType)] 
                                    voidType
      -- Min and max
      , defineFunction minnumString intParams2 intType
      , defineFunction maxnumString intParams2 intType
      
      -- Bool Write and Writeln
      , defineFunction writeLnBool boolParam voidType
      , defineFunction writeBool   boolParam voidType
      
      -- Char Write and Writeln
      , defineFunction writeLnChar charParam voidType
      , defineFunction writeChar   charParam voidType
      
      -- Float Write and Writeln
      , defineFunction writeLnFloat floatParam voidType
      , defineFunction writeFloat   floatParam voidType
      
      -- Int Write and Writeln 
      , defineFunction writeLnInt intParam voidType
      , defineFunction writeInt   intParam voidType
      
      -- String Write and Writeln
      , defineFunction writeLnString stringParam voidType
      , defineFunction writeString   stringParam voidType
      
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
      , defineFunction "_malloc" intParam (ptr pointerType)
      , defineFunction "_free" [(parameter ("x", ptr pointerType))] voidType
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
