{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified LLVM.General.AST.FloatingPointPredicate as FL 
import qualified LLVM.General.AST.IntegerPredicate       as IL 
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified Data.Sequence                           as DS
import qualified Data.Text                               as TE
import qualified Data.Map                                as DM
import qualified Type                                    as T
import qualified AST                                     as MyAST
import LLVM.General.AST                                  as AST
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.Module
import Data.Foldable (toList)
import SymbolTable
import Data.Either
import Data.Maybe
import Data.Word
import Data.Char
import Contents


data CodegenSt
  = CodeGenSt {
    insCount    :: Word                        -- Cantidad de instrucciones sin nombre
  , blockName   :: Name                        -- Cantidad de bloques básicos en el programa
  , instrs      :: DS.Seq (Named Instruction)  -- Lista de instrucciones en el bloque básico actual
  , bblocs      :: DS.Seq BasicBlock           -- Lista de bloques básicos en la definición actual
  , moduleDefs  :: DS.Seq Definition
  , varsLoc     :: DM.Map String Operand
  , arrsDim     :: DM.Map String [Operand]
  } deriving (Show)


newtype LLVM a = LLVM { unLLVM :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)


emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 1 (UnName 0) DS.empty DS.empty DS.empty DM.empty DM.empty


execCodegen :: LLVM a -> CodegenSt
execCodegen m = execState (unLLVM m) emptyCodegen


astToLLVM :: MyAST.AST T.Type -> AST.Module
astToLLVM (MyAST.Program name _ defs accs _) =
    defaultModule { moduleName        = TE.unpack name
                  , moduleDefinitions = toList $ moduleDefs $ execCodegen $ createLLVM defs accs
    }

addDimToArray :: String -> Operand -> LLVM()
addDimToArray name op = do
    dims <- gets arrsDim
    modify $ \s -> s { arrsDim = DM.insertWith (++) name [op] dims }

createPreDef ::  LLVM () 
createPreDef = do
    let params =  ([Parameter i32 (Name "x") []], False)
    addDefinition "writeLnInt" params VoidType
    let params2 = ([Parameter i1 (Name "x") []], False)
    addDefinition "writeLnBool" params2 VoidType
    let params3 = ([Parameter double (Name "x") []], False)
    addDefinition "writeLnDouble"  params3 VoidType
    addDefinition "llvm.sqrt.f64"   params3 double
    addDefinition "llvm.fabs.f64"   params3 double
    addDefinition "llvm.minnum.f64" params3 double
    addDefinition "llvm.maxnum.f64" params3 double
    let params4 = ([Parameter double (Name "y") [], 
                    Parameter double (Name "x") []], False)
    addDefinition "llvm.pow.f64" params4 double
    return ()






createLLVM :: [MyAST.AST T.Type] -> [MyAST.AST T.Type] -> LLVM ()
createLLVM defs accs = do
    createPreDef
    mapM_ createDef defs
    m800 <- retVoid
    createBasicBlocks accs m800
    addDefinition "main" ([],False) VoidType


createDef :: MyAST.AST T.Type -> LLVM()
createDef (MyAST.DefProc name st accs pre post bound _ _) = do
<<<<<<< HEAD
    -- let procCont  = DM.toList $ getMap $ getActual st
   --- - let args  = map (\(n, t) -> (Name $ TE.unpack n, toType $ symbolType t, procArgType t)) procCont
    -- sTableToAlloca st
 -- --   let sArgs = map (\(id, t) -> let ty = toType $ symbolType t in 
    --  --     (ty, fromJust $ DM.lookup (TE.unpack id) vars, local ty (Name $ TE.unpack id) )) procCont
    -- let args  = map (\(id, t) -> (Name $ TE.unpack id, toType $ symbolType t)) procCont
   --- - storeList $ head sArgs  
    -- let args' = ([Parameter t id [] | (id, t) <- args], False) 
    -- retTy <- retVoid
    -- createBasicBlocks accs retTy
    -- addDefinition (TE.unpack name) args' VoidType

  return () 

=======
    let procCont = DM.toList $ getMap $ getActual st
    let justArgs = filter (\(id, t) -> isArg t) procCont
    let locals   = filter (\(id, t) -> not $ isArg t) procCont 
    localAlloca locals
   -- let args  = map (\(n, t) -> (Name $ TE.unpack n, toType $ symbolType t, procArgType t)) procCont
    let args     = map (\(id, t) -> (Name $ TE.unpack id, toType $ symbolType t)) justArgs
    let args'    = ([Parameter t id [] | (id, t) <- args], False) 
    retTy <- retVoid
    createBasicBlocks accs retTy
    addDefinition (TE.unpack name) args' VoidType

   
>>>>>>> df4e61a91ec3bb084a5bf660064a5aebebf2c401
createDef (MyAST.DefFun fname st _ (MyAST.FunBody _ exp _) reType bound _) = do
    let funcCont  = DM.toList $ getMap $ getActual st
    let args  = map (\(n, t) -> (Name $ TE.unpack n, toType $ symbolType t)) funcCont
    let args' = ([Parameter t n [] | (n, t) <- args], False)
    exp'  <- createExpression exp
    retTy <- retType exp'
    addBasicBlock retTy
    addDefinition (TE.unpack fname) args' (toType reType)
    return ()


addDefinition :: String -> ([Parameter], Bool) -> Type -> LLVM ()
addDefinition name params retTy = do
    bbl  <- gets bblocs
    defs <- gets moduleDefs 
    let def = GlobalDefinition $ functionDefaults {
                name        = Name name
              , parameters  = params
              , returnType  = retTy
              , basicBlocks = (toList bbl)
              }
    modify $ \s -> s { bblocs  = DS.empty }
    modify $ \s -> s { varsLoc = DM.empty }
    modify $ \s -> s { moduleDefs = defs DS.|> def}


newLabel :: LLVM (Name)
newLabel = do
    n <- getCount
    return $ UnName n


setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    modify $ \s -> s { blockName = name }


addBasicBlock :: Named Terminator -> LLVM ()
addBasicBlock t800 = do
    lins  <- gets instrs
    bbl   <- gets bblocs
    name  <- gets blockName
    name' <- newLabel
    modify $ \s -> s { blockName  = name'    }
    modify $ \s -> s { instrs     = DS.empty }
    modify $ \s -> s { bblocs     = bbl DS.|> BasicBlock name (toList lins) t800 }


addNamedInstruction :: Type -> String -> Instruction -> LLVM (Operand)
addNamedInstruction t name ins = do
    lins <- gets instrs
    let r = Name name
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    let op = local t r
    addVarOperand name op
    return op 

addVarOperand :: String -> Operand -> LLVM()
addVarOperand name op = do
    map <- gets varsLoc
    modify $ \s -> s { varsLoc = DM.insert name op map }


addUnNamedInstruction :: Type -> Instruction -> LLVM (Operand)
addUnNamedInstruction t ins = do
    r    <- newLabel
    lins <- gets instrs
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    return $ local t r


getCount :: LLVM Word
getCount = do
    n <- gets insCount
    modify $ \s -> s { insCount = n + 1 }
    return $ n

accToAlloca :: MyAST.AST T.Type -> LLVM()
accToAlloca acc@(MyAST.ID _ id' t) = do
    let id = TE.unpack id'
    dim <- typeToOperand id t 
    alloca dim (toType t) id
    createInstruction acc

<<<<<<< HEAD
accToAlloca acc@(MyAST.LAssign lids _ _ _) = do
    mapM_ idToAlloca lids
    createInstruction acc

idToAlloca :: ((TE.Text, T.Type), [MyAST.AST a]) -> LLVM()
idToAlloca ((id, t),arr) = do
    let id' = TE.unpack id
    dim <- typeToOperand id' t
    alloca dim (toType t) id'
    return ()

typeToOperand :: String -> T.Type -> LLVM (Maybe Operand)
typeToOperand name (T.MyArray dim ty) = do
    r <- typeToOperand name ty
    d <- dimToOperand dim
    addDimToArray name d
    case r of
      Nothing -> return $ return d 
      Just op -> fmap Just $ addUnNamedInstruction (toType T.MyInt) $ irArithmetic MyAST.Mul T.MyInt op d
typeToOperand _  _             = return $ Nothing

dimToOperand :: Either TE.Text Integer -> LLVM Operand
dimToOperand (Right n) = return $ ConstantOperand $ C.Int 32 n
dimToOperand (Left id) = load (TE.unpack id) intType

intType = i32

opsToArrayIndex :: String -> [Operand] -> LLVM (Operand)
opsToArrayIndex name ops = do
    arrD <- gets arrsDim
    let arrDims' = fromJust $ DM.lookup name arrD
    mulDims (tail arrDims') ops

-- Si la primera lista tiene mas elementos que la segunda paso al muy malo en el chequeo de tipos.
-- Significa que estas intentando acceder a una dimension del arreglo que no existe.
mulDims :: [Operand] -> [Operand] -> LLVM Operand
mulDims _ [acc] = return acc

mulDims (arrDim:xs) (acc:ys) = do
    op    <- mulDims xs ys
    opMul <- addUnNamedInstruction intType $ Mul False False arrDim acc []
    addUnNamedInstruction intType $ Add False False op opMul []
=======
localAlloca :: [(TE.Text, Contents a)] -> LLVM ()
localAlloca idList =
    mapM_ (uncurry alloca) $ map (\(id, c) -> ((toType . symbolType) c, TE.unpack id)) idList


sTableToAlloca :: SymbolTable -> LLVM ()
sTableToAlloca st = 
    mapM_ (uncurry alloca) $ map (\(id, c) -> ((toType . symbolType) c, TE.unpack id))
                                                 $ DM.toList $ (getMap . getActual) st
    
>>>>>>> df4e61a91ec3bb084a5bf660064a5aebebf2c401

createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.LAssign (((id, t), []):_) (e:_) _ _) = do
    e' <- createExpression e
    map <- gets varsLoc
    let (t', i) = (toType t, fromJust $ DM.lookup (TE.unpack id) map)
    store t' i e'
    return ()

createInstruction (MyAST.EmptyAST _ ) = return ()
createInstruction (MyAST.ID _ _ _)    = return ()

createInstruction (MyAST.LAssign (((id', t), accs):_) (e:_) _ _) = do
    e'  <- createExpression e
    ac' <- mapM createExpression accs
    map <- gets varsLoc
    let (t', i, id) = (toType t, fromJust $ DM.lookup id map, TE.unpack id')
    ac'' <- opsToArrayIndex id ac'
    opa  <- addUnNamedInstruction (toType T.MyInt) $ GetElementPtr True i [ac''] []
    store t' opa e'
    return ()

createInstruction (MyAST.Write True exp _ t) = do
    let ty = MyAST.tag exp 
    e' <- createExpression exp

    case ty of
    { T.MyInt   -> do addUnNamedInstruction (toType t) $ Call False CC.C [] (Right 
                        (definedFunction i32 (Name "writeLnInt"))) [(e', [])] [] []
    ; T.MyFloat -> do addUnNamedInstruction (toType t) $ Call False CC.C [] (Right 
                        (definedFunction double (Name "writeLnDouble"))) [(e', [])] [] []   
    ; T.MyBool  -> do addUnNamedInstruction (toType t) $ Call False CC.C [] (Right 
                        (definedFunction i1 (Name "writeLnBool"))) [(e', [])] [] []
    }
    return ()


createInstruction (MyAST.Skip _ _) = return ()
 

createInstruction (MyAST.Block _ st decs accs _) = do
    mapM_ accToAlloca decs
    mapM_ createInstruction accs
    return ()


createInstruction (MyAST.Convertion tType _ exp t) = do
    let t' = MyAST.tag exp 
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irConvertion tType t' exp'
    return ()


createInstruction (MyAST.Cond guards _ _) = do
    final <- newLabel
    genGuards guards final final
    return ()


createInstruction (MyAST.Rept guards _ _ _ _) = do
    final   <- newLabel
    initial <- newLabel
    setLabel initial $ branch initial
    genGuards guards final initial


createInstruction (MyAST.ProcCall pname st _ args _) = do
    exp <- mapM createExpression args
    let exp' = map (\i -> (i,[])) exp
    let op   = definedFunction VoidType (Name $ TE.unpack pname)
    addUnNamedInstruction VoidType $ Call False CC.C [] (Right op) exp' [] []
    --setLabel final $ Do $ (Invoke CC.C [] (Right op) exp' [] final final [])
    return ()


branch :: Name -> Named Terminator
branch label = Do $ Br label [] 


condBranch :: Operand -> Name -> Name -> Named Terminator
condBranch op true false = Do $ CondBr op true false []


genGuards :: [MyAST.AST T.Type] -> Name -> Name -> LLVM ()
genGuards (guard:[]) none one = do
    genGuard guard none
    setLabel none $ branch one

genGuards (guard:xs) none one = do
    next <- newLabel
    genGuard guard next
    setLabel next $ branch one
    genGuards xs none one

genGuard (MyAST.Guard guard acc _ _) next = do
    tag  <- createExpression guard
    code <- newLabel
    setLabel code $ condBranch tag code next
    createInstruction acc


definedFunction :: Type -> Name -> Operand
definedFunction ty = ConstantOperand . (C.GlobalReference ty)


alloca :: Maybe Operand -> Type -> String -> LLVM Operand
alloca cant ty r = do
    addNamedInstruction ty r $ Alloca ty cant 0 []


store :: Type -> Operand -> Operand -> LLVM Operand
store t ptr val =
    addUnNamedInstruction t $ Store False ptr val Nothing 0 []


load :: String -> Type -> LLVM (Operand)
load name ty = do 
    addUnNamedInstruction ty $ Load False (local ty (Name name)) Nothing 0 []


createExpression :: MyAST.AST T.Type -> LLVM (Operand)
createExpression (MyAST.ID _ id t) = do
<<<<<<< HEAD
    let (r, ty) = (TE.unpack id, toType t)
    load r ty

createExpression (MyAST.ArrCall _ id' accs t) = do
    accs' <- mapM createExpression accs
    map  <- gets varsLoc
    let (t', i, id) = (toType t, fromJust $ DM.lookup id map, TE.unpack id')
    accs'' <- opsToArrayIndex id accs'
    add <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
    addUnNamedInstruction t' $ Load False add Nothing 0 []
=======
    var <- gets varsLoc
    let (n, ty) = (TE.unpack id, toType t)
    let check   = DM.lookup n var
   
    case check of 
    { Just _  -> do val <- load n ty
                    return val
    ; Nothing -> do return $ local ty (Name n)
    }

>>>>>>> df4e61a91ec3bb084a5bf660064a5aebebf2c401

createExpression (MyAST.Int _ n _) = do
    return $ ConstantOperand $ C.Int 32 n


createExpression (MyAST.Float _ n _) = do
    return $ ConstantOperand $ C.Float $ Double n


createExpression (MyAST.Bool _ True  _) = do
   return $ ConstantOperand $ C.Int 1 1 
 

createExpression (MyAST.Bool _ False _) = do
   return $ ConstantOperand $ C.Int 1 0 
 

createExpression (MyAST.Char _ n _) = do
    return $ ConstantOperand $ C.Int 8 $ toInteger $ digitToInt n
    

createExpression (MyAST.Arithmetic op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
 
 
createExpression (MyAST.Boolean op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction (toType t) $ irBoolean op lexp' rexp'
 
 
createExpression (MyAST.Relational op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    let t' = MyAST.tag lexp 
    addUnNamedInstruction (toType t) $ irRelational op t' lexp' rexp'


createExpression (MyAST.Unary op _ exp t) = do
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irUnary op t exp' 

<<<<<<< HEAD
load :: String -> Type -> LLVM (Operand)
load name ty = do 
    map <- gets varsLoc
    let i = fromJust $ DM.lookup name map
    addUnNamedInstruction ty $ Load False i Nothing 0 []
=======

createExpression (MyAST.FCallExp fname st _ args t) = do
    exp <- mapM createExpression args
    let ty   =  toType t 
    let exp' = map (\i -> (i,[])) exp
    let op   = definedFunction ty (Name $ TE.unpack fname)
    val <- addUnNamedInstruction ty $ Call False CC.C [] (Right op) exp' [] []
    return val
>>>>>>> df4e61a91ec3bb084a5bf660064a5aebebf2c401


createBasicBlocks :: [MyAST.AST T.Type] -> Named Terminator -> LLVM ()
createBasicBlocks accs m800 = do
    genIntructions accs
      where
        genIntructions (acc:xs) = do
            r <- newLabel
            createInstruction acc
            genIntructions xs
        genIntructions [] = do
            r <- newLabel
            addBasicBlock m800


toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i1
toType T.MyChar  = i8
toType (T.MyArray _ t) = toType t 

local :: Type -> Name -> Operand
local = LocalReference


retType :: Operand -> LLVM (Named Terminator)
retType op = do 
    n <- newLabel
    return $ n := Ret (Just op) []


retVoid :: LLVM (Named Terminator)
retVoid = do 
    n <- newLabel
    return $ n := Ret Nothing []


irArithmetic :: MyAST.OpNum -> T.Type -> Operand -> Operand -> Instruction
irArithmetic MyAST.Sum T.MyInt   a b = Add False False a b []
irArithmetic MyAST.Sum T.MyFloat a b = FAdd NoFastMathFlags a b []
irArithmetic MyAST.Sub T.MyInt   a b = Sub False False a b []
irArithmetic MyAST.Sub T.MyFloat a b = FSub NoFastMathFlags a b []
irArithmetic MyAST.Mul T.MyInt   a b = Mul False False a b []
irArithmetic MyAST.Mul T.MyFloat a b = FMul NoFastMathFlags a b []
irArithmetic MyAST.Div T.MyInt   a b = SDiv True a b []
irArithmetic MyAST.Div T.MyFloat a b = FDiv NoFastMathFlags a b []
irArithmetic MyAST.Mod T.MyInt   a b = URem a b []
irArithmetic MyAST.Mod T.MyFloat a b = FRem NoFastMathFlags a b []
--irArithmetic MyAST.Exp T.MyInt   a b = URem a b []
irArithmetic MyAST.Exp T.MyFloat a b = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.pow.f64"))) [(a, []),(b, [])] [] []

--irArithmetic MyAST.Min T.MyInt   a b = URem a b []
irArithmetic MyAST.Min T.MyFloat a b = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.minnum.f64"))) [(a, []),(b, [])] [] []
--irArithmetic MyAST.Max T.MyInt   a b = URem a b []
irArithmetic MyAST.Max T.MyFloat a b = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.maxnum.f64"))) [(a, []),(b, [])] [] []


irBoolean :: MyAST.OpBool -> Operand -> Operand -> Instruction
irBoolean MyAST.Con a b = And a b []
irBoolean MyAST.Dis a b = Or  a b []
--irBoolean MyAST.Implies a b = And a b []
--irBoolean MyAST.Conse a b = Or  a b []


irRelational :: MyAST.OpRel -> T.Type -> Operand -> Operand -> Instruction
irRelational MyAST.Equ     T.MyFloat a b = FCmp FL.OEQ a b []
irRelational MyAST.Less    T.MyFloat a b = FCmp FL.OLT a b []
irRelational MyAST.Greater T.MyFloat a b = FCmp FL.OGT a b []
irRelational MyAST.LEqual  T.MyFloat a b = FCmp FL.OLE a b []
irRelational MyAST.GEqual  T.MyFloat a b = FCmp FL.OGE a b []
irRelational MyAST.Ine     T.MyFloat a b = FCmp FL.OEQ a b [] -- Negacion
irRelational MyAST.Equal   T.MyFloat a b = FCmp FL.ONE a b [] -- Inequiva  REVISARRR

irRelational MyAST.Equ     T.MyInt   a b = ICmp IL.EQ a b []
irRelational MyAST.Less    T.MyInt   a b = ICmp IL.SLT a b []
irRelational MyAST.Greater T.MyInt   a b = ICmp IL.SGT a b []
irRelational MyAST.LEqual  T.MyInt   a b = ICmp IL.SLE a b []
irRelational MyAST.GEqual  T.MyInt   a b = ICmp IL.SGE a b []
irRelational MyAST.Ine     T.MyInt   a b = ICmp IL.EQ a b []
irRelational MyAST.Equal   T.MyInt   a b = ICmp IL.NE a b []


irConvertion :: MyAST.Conv -> T.Type -> Operand -> Instruction
irConvertion MyAST.ToInt    T.MyFloat a = FPToSI a i32    [] 
irConvertion MyAST.ToInt    T.MyChar  a = FPToSI a i32    [] 
irConvertion MyAST.ToDouble T.MyInt   a = SIToFP a double [] 
irConvertion MyAST.ToDouble T.MyChar  a = SIToFP a double [] 
irConvertion MyAST.ToChar   T.MyInt   a = Trunc  a i8     [] 
irConvertion MyAST.ToChar   T.MyFloat a = FPToSI a i8     [] 


irUnary :: MyAST.OpUn -> T.Type -> Operand -> Instruction
irUnary MyAST.Minus T.MyInt   a = Sub False False      (ConstantOperand $ C.Int 32 0) a []
irUnary MyAST.Minus T.MyFloat a = FSub NoFastMathFlags (ConstantOperand $ C.Float $ Double 0) a []
irUnary MyAST.Not   T.MyBool  a = Xor a (ConstantOperand $ C.Int 1 1) [] 
--irUnary MyAST.Abs   T.MyInt a = 
irUnary MyAST.Abs   T.MyFloat a = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.fabs.f64"))) [(a, [])] [] []
irUnary MyAST.Sqrt  T.MyFloat a = Call False CC.C [] (Right ( definedFunction double 
                                    (Name "llvm.sqrt.f64"))) [(a, [])] [] []
