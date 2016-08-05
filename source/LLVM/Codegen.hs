-- {-# LANGUAGE NamedFieldPuns #-}

module LLVM.Codegen where

-- --------------------------------------------------------------------------------
-- import           Aborts
-- import           AST.Definition
-- import           AST.Instruction                         (Instruction)
-- import           AST.Program

-- import           Limits
-- import           LLVM.CodegenState
-- import           LLVM.Expression
-- import           LLVM.Instruction
-- import           LLVM.Quantification
-- import           SymbolTable
-- import qualified Type                                    as T
-- --------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
-- import qualified Control.Monad.State                     as M (void)
-- import           Data.Foldable                           (toList)
-- import qualified Data.Map                                as DM
-- import           Data.Maybe
-- import           Data.Range.Range                        as RA
-- import qualified Data.Text                               as TE
-- import           Data.Word
-- import           LLVM.General.AST                        (Definition (..),
--                                                           Module (..),
--                                                           Parameter (..),
--                                                           defaultModule)
-- import           LLVM.General.AST.Attribute
-- import qualified LLVM.General.AST.CallingConvention      as CC
-- import qualified LLVM.General.AST.Constant               as C
-- import qualified LLVM.General.AST.FloatingPointPredicate as FL
-- import           LLVM.General.AST.Instruction            (FastMathFlags (..),
--                                                           Instruction (..),
--                                                           Named (..),
--                                                           Terminator (..))
-- import qualified LLVM.General.AST.IntegerPredicate       as IL
-- import           LLVM.General.AST.Name                   (Name (..))
-- import           LLVM.General.AST.Operand                (CallableOperand,
--                                                           Operand (..))
-- import           LLVM.General.AST.Type
-- import           System.Info                             (arch, os)
-- import           System.Process                          (callCommand)
-- --------------------------------------------------------------------------------

-- createParameters :: [(Name, Type)] -> [[ParameterAttribute]] -> ([Parameter], Bool)
-- createParameters names attrs = (map (\((name, t), attr) -> Parameter t name attr) (zip names attrs), False)


-- createEmptyParameters :: [(Name, Type)] -> ([Parameter], Bool)
-- createEmptyParameters names = (map (\(name, t) -> Parameter t name []) names, False)


-- createPreDef :: [String] -> LLVM ()
-- createPreDef files = do

--   addDefinition randomInt (createParameters [] []) intType

--   addDefinition abortString (createEmptyParameters [(Name "x", intType),
--     (Name "line", intType), (Name "column", intType)]) voidType

--   let intParams = createEmptyParameters [(Name "x", intType)]
--   addDefinition writeLnInt intParams voidType
--   addDefinition writeInt   intParams voidType

--   let intParams2 = createEmptyParameters [(Name "x", intType), (Name "y", intType)]
--   addDefinition minnumString intParams2 intType
--   addDefinition maxnumString intParams2 intType

--   let charParams = createEmptyParameters [(Name "x", charType)]
--   addDefinition writeLnChar charParams voidType
--   addDefinition writeChar   charParams voidType

--   let boolParams = createEmptyParameters [(Name "x", boolType)]
--   addDefinition writeLnBool boolParams voidType
--   addDefinition writeBool   boolParams voidType

--   let doubleParams = createEmptyParameters [(Name "x", floatType)]
--   addDefinition writeLnDouble doubleParams voidType
--   addDefinition writeDouble   doubleParams voidType
--   addDefinition sqrtString    doubleParams doubleType
--   addDefinition fabsString    doubleParams doubleType

--   let doubleParams2 = createEmptyParameters [(Name "x", floatType), (Name "y", floatType)]
--   addDefinition minnumFstring doubleParams2 doubleType
--   addDefinition maxnumFtring  doubleParams2 doubleType
--   addDefinition powString     doubleParams2 doubleType

--   let stringParams = createParameters [(Name "msg", stringType)] [[NoCapture]]
--   addDefinition writeLnString stringParams intType
--   addDefinition writeString   stringParams intType

--   let overflow' = StructureType False [intType, boolType]

--   addDefinition intAdd intParams2 overflow'
--   addDefinition intSub intParams2 overflow'
--   addDefinition intMul intParams2 overflow'

--   addDefinition readIntStd    (createEmptyParameters []) intType
--   addDefinition readCharStd   (createEmptyParameters []) charType
--   addDefinition readDoubleStd (createEmptyParameters []) double

--   addDefinition openFileStr (createEmptyParameters [(Name "nombreArchivo", ptr pointerType)]) (ptr pointerType)

--   mapM_ addFile files

--   addDefinition readFileInt    (createEmptyParameters [(Name "f", ptr pointerType)]) intType
--   addDefinition readFileChar   (createEmptyParameters [(Name "f", ptr pointerType)]) charType
--   addDefinition readFileDouble (createEmptyParameters [(Name "f", ptr pointerType)]) doubleType
--   addDefinition closeFileStr   (createEmptyParameters [(Name "f", ptr pointerType)]) voidType

--   return ()


-- addFile :: String -> LLVM ()
-- addFile file = globalVariable (Name (convertFile file)) (ptr pointerType) (C.Null (ptr pointerType))


-- astToLLVM :: [String] -> Program -> String -> Module
-- astToLLVM files Program { name, defs, insts } version =
--   defaultModule
--     { moduleName         = TE.unpack name
--     , moduleDefinitions  = toList . _moduleDefs . execCodegen $ createLLVM files defs insts
--     , moduleTargetTriple = Just whichTarget
--     }
--   where
--     whichTarget = case os of
--       "darwin"  -> arch++"-apple-macosx" ++ crop version -- With Mac, version needs to end with "0",
--       "linux"   -> arch++"-unknown-linux-gnu"            -- example: 11.10.3 -> 11.10.0
--       "windows" -> undefined
--     crop str = if last str == '.'
--         then str++"0"
--         else crop $ init str


-- openFile :: String -> LLVM Operand
-- openFile file = do
--   let file' = convertFile file
--   ops <- addFileNameOpe file
--   op  <- caller (ptr charType) (Right $ definedFunction (ptr charType) (Name openFileStr)) [(ops, [])]
--   store (ptr charType) (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) op


-- closeFile :: String -> LLVM Operand
-- closeFile file = do
--   let file' = convertFile file
--   -- Cargamos la variable gobal perteneciente al archivo.
--   let load' = Load False (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) Nothing 0 []
--   op <- addUnNamedInstruction voidType load'
--   caller voidType (Right $ definedFunction voidType (Name closeFileStr)) [(op, [])]


-- createLLVM :: [String] -> [Definition] -> Instruction -> LLVM ()
-- createLLVM files defs accs = do
--   createPreDef files
--   mapM_ createDef defs
--   m800 <- retVoid
--   mapM_ openFile files
--   createInstruction accs
--   mapM_ closeFile files
--   addBasicBlock m800
--   addDefinition "main" ([],False) voidType
--   return ()


-- addArgOperand :: [(String, Contents SymbolTable)] -> LLVM ()
-- addArgOperand [] = return ()

-- addArgOperand ((id',c):xs) = do
--   let t    = toType $ argType c
--   let tp   = argTypeArg c
--   let id   = convertId id'
--   let e'   = local t (Name id')

--   case tp of
--     T.InOut -> do exp <- addUnNamedInstruction t $ Load False e' Nothing 0 []
--                   op  <- alloca Nothing t id
--                   store t op exp
--                   addVarOperand id' op

--     T.In    -> do op <- alloca Nothing t id
--                   store t op e'
--                   addVarOperand id' op

--     T.Out   -> do op <- alloca Nothing t id
--                   addVarOperand id' op
--                   initialize id $ argType c
--                   return ()

--     T.Ref   -> addVarOperand id' e'

--   addArgOperand xs


-- retVarOperand :: [(String, Contents SymbolTable)] -> LLVM ()
-- retVarOperand [] = pure ()

-- retVarOperand ((id', c):xs) = do

--   let t   = toType $ getContentType c
--   let exp = local t (Name id')
--   let tp  = argTypeArg c

--   case tp of
--     T.InOut -> do add <- load id' t
--                   store t exp add
--                   return ()

--     T.Out   -> do add <- load id' t
--                   store t exp add
--                   return ()

--     T.In     -> return ()
--     T.Ref    -> return ()

--   retVarOperand xs


-- addFuncParam :: (TE.Text, T.Type) -> LLVM ()
-- addFuncParam (id'', t@(T.GArray _ _)) = do
--   let id = TE.unpack id''
--   let e'   = local (toType t) (Name id)
--   addVarOperand id e'
--   return ()
-- addFuncParam _ = return ()


-- createDef :: Definition -> LLVM ()
-- createDef Definition { name, st, params, def' } = case def' of
--   ProcedureDef { constDec, pre, procbody, post } -> do
--     let name' = TE.unpack name
--     let args  = map (\(id, _) -> (TE.unpack id, fromJust $ checkSymbol id st)) params
--     let args' = ([Parameter t (Name id) [] | (id, t) <- convertParams args], False)
--     retTy <- retVoid
--     addArgOperand args
--     mapM_ accToAlloca constDec
--     createState name' pre
--     createInstruction procbody
--     retVarOperand $ reverse args
--     createState name' post
--     addBasicBlock retTy
--     addDefinition name' args' voidType

--   FunctionDef { funcbody, retType } -> do
--     let args' = ([Parameter t (Name id) [] | (id, t) <- convertFuncParams params], False)
--     mapM_ addFuncParam params
--     exp'  <- createExpression funcbody
--     n <- newLabel
--     let retTy = n := Ret (Just exp') []
--     addBasicBlock retTy
--     addDefinition (TE.unpack name) args' (toType retType)

-- -- createBasicBlocks :: [AST] -> Named Terminator -> LLVM ()
-- -- createBasicBlocks accs m800 = genIntructions accs
-- --   where
-- --     genIntructions (acc:xs) = do
-- --       createInstruction acc
-- --       genIntructions xs
-- --     genIntructions [] =
-- --       addBasicBlock m800
