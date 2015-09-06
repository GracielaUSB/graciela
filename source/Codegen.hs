{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified LLVM.General.AST.FloatingPointPredicate as FL 
import qualified LLVM.General.AST.IntegerPredicate       as IL 
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import LLVM.General.AST.AddrSpace
import qualified Data.Sequence                           as DS
import qualified Data.Text                               as TE
import qualified Data.Map                                as DM
import qualified Type                                    as T
import qualified AST                                     as MyAST
import LLVM.General.AST                                  as AST
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Attribute
import LLVM.General.AST.AddrSpace
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
import CodegenState

writeLnInt = "_writeLnInt"
writeLnBool = "_writeLnBool"
writeLnDouble = "_writeLnDouble"
writeLnString = "puts"
writeInt = "_writeInt"
writeBool = "writeBool"
writeDouble = "_writeDouble"
writeString = "puts"

randomInt = "_randomInt"
abortString = "_abort"
sqrtString = "llvm.sqrt.f64"
fabsString = "llvm.fabs.f64"
minnumString = "llvm.minnum.f64"
maxnumString = "llvm.maxnum.f64"
powString = "llvm.pow.f64"

createParameters names attrs = (map (\((name, t), attr) -> Parameter t name attr) (zip names attrs), False)

name     = Name
voidType = VoidType
boolType = i1
doubleType = double

createPreDef ::  LLVM () 
createPreDef = do

    addDefinition randomInt   (createParameters [] []) intType

    let intParams = createParameters [(name "x", intType)] [[]]
    addDefinition writeLnInt  intParams voidType
    addDefinition writeInt    intParams voidType
    addDefinition abortString intParams voidType

    let boolParams = createParameters [(name "x", boolType)] [[]]
    addDefinition writeLnBool boolParams voidType
    addDefinition writeBool   boolParams voidType

    let doubleParams = createParameters [(name "x", doubleType)] [[]]
    addDefinition writeLnDouble doubleParams VoidType
    addDefinition writeDouble   doubleParams VoidType
    addDefinition sqrtString    doubleParams doubleType
    addDefinition fabsString    doubleParams doubleType
    addDefinition minnumString  doubleParams doubleType
    addDefinition maxnumString  doubleParams doubleType

    addDefinition powString (createParameters [(name "x", doubleType), (name "y", doubleType)] [[], []]) doubleType

    return ()

astToLLVM :: MyAST.AST T.Type -> AST.Module
astToLLVM (MyAST.Program name _ defs accs _) =
    defaultModule { moduleName        = TE.unpack name
                  , moduleDefinitions = toList $ moduleDefs $ execCodegen $ createLLVM defs accs
    }

createLLVM :: [MyAST.AST T.Type] -> [MyAST.AST T.Type] -> LLVM ()
createLLVM defs accs = do
    createPreDef
    mapM_ createDef defs
    m800 <- retVoid
    createBasicBlocks accs m800

    -- Tag de error del if
    modify $ \s -> s { blockName = Name "ifAbort" }
    let arg   = [(ConstantOperand $ C.Int 32 1, [])]
    addUnNamedInstruction VoidType $ Call False CC.C [] (Right 
                         (definedFunction i32 (Name abortString))) arg [] []
    addBasicBlock (Do $ Unreachable [])
    --

    addDefinition "main" ([],False) VoidType





createDef :: MyAST.AST T.Type -> LLVM()
createDef (MyAST.DefProc name st accs pre post bound decs params _) = do
    mapM_ accToAlloca decs
    let args     = convertParams (map (\(id, _) -> (id, fromJust $ checkSymbol id st)) params)
    let args'    = ([Parameter t (Name id) [] | (id, t) <- args], False) 
    retTy <- retVoid
    mapM_ (uncurry addVarOperand) $ zip (map fst args) (map (\(id, t) -> local t (Name id)) args)
    createBasicBlocks accs retTy
    addDefinition (TE.unpack name) args' VoidType

   
createDef (MyAST.DefFun fname st _ exp reType bound params _) = do
    let args' = ([Parameter (toType t) (Name (TE.unpack id)) [] | (id, t) <- params], False)
    exp'  <- createExpression exp
    retTy <- retType exp'
    addBasicBlock retTy
    addDefinition (TE.unpack fname) args' (toType reType)

accToAlloca :: MyAST.AST T.Type -> LLVM()
accToAlloca acc@(MyAST.ID _ id' t) = do
    let id = TE.unpack id'
    dim <- typeToOperand id t 
    alloca dim (toType t) id
    createInstruction acc


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

procedureCall t pname es = do
    let es' = map (\e -> (e, [])) es
    let df  = Right $ definedFunction t (name pname)
    addUnNamedInstruction t $ Call False CC.C [] df es' [] []

createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.EmptyAST _ ) = return ()
createInstruction (MyAST.ID _ _ _)    = return ()
createInstruction (MyAST.Skip _ _)    = return ()


createInstruction (MyAST.Abort _ _) = do
    let arg = [(ConstantOperand $ C.Int 32 2, [])]
    addUnNamedInstruction VoidType $ Call False CC.C [] (Right 
                         (definedFunction i32 (Name "abortt"))) arg [] []
    return ()


createInstruction (MyAST.LAssign (((id, t), []):_) (e:_) _ _) = do
    e' <- createExpression e
    map <- gets varsLoc
    let (t', i) = (toType t, fromJust $ DM.lookup (TE.unpack id) map)
    store t' i e'
    return ()


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
    let ty  = MyAST.tag exp 
    let ty' = toType t
    e' <- createExpression exp

    case ty of
    { T.MyInt    -> procedureCall ty' writeLnInt [e']
    ; T.MyFloat  -> procedureCall ty' writeLnDouble [e']
    ; T.MyBool   -> procedureCall ty' writeLnBool [e']
    ; T.MyString -> procedureCall ty' writeLnString [e']
    }
    return ()


createInstruction (MyAST.Write False exp _ t) = do
    let ty = MyAST.tag exp 
    let ty' = toType t
    e' <- createExpression exp
    case ty of
    { T.MyInt    -> procedureCall ty' writeInt [e']
    ; T.MyFloat  -> procedureCall ty' writeDouble [e']
    ; T.MyBool   -> procedureCall ty' writeBool [e']
    ; T.MyString -> procedureCall ty' writeString [e']
    }
    return ()

createInstruction (MyAST.Block _ st decs accs _) = do
    mapM_ accToAlloca decs
    mapM_ createInstruction accs
    return ()


createInstruction (MyAST.Cond guards _ _) = do
    final <- newLabel
    genGuards guards (Name "ifAbort") final
    setLabel final $ branch final
    return ()


createInstruction (MyAST.Rept guards _ _ _ _) = do
    final   <- newLabel
    initial <- newLabel
    setLabel initial $ branch initial
    genGuards guards final initial 
    setLabel final $ branch initial
    return ()


createInstruction (MyAST.ProcCall pname st _ args _) = do
    let c     = fromJust $ checkSymbol pname st
    let dic   = getMap $ getActual $ sTable $ c
    let nargp = map fst $ filter (\(id, t) -> isArg t) (DM.toList dic)
    exp <- createArguments dic nargp args

    procedureCall voidType (TE.unpack pname) exp
    return ()


createInstruction (MyAST.Ran id _ _ t) = do
    vars <- gets varsLoc
    let (ty, i) = (toType t, fromJust $ DM.lookup (TE.unpack id) vars)
    val <- addUnNamedInstruction ty $ Call False CC.C [] (Right ( definedFunction double 
                                                                    (Name "randomInt"))) [] [] []  
    store ty i val
    return ()


createArguments dicnp (nargp:nargps) (arg:args) = do
    lr <- createArguments dicnp nargps args
    let argt = procArgType $ fromJust $ DM.lookup nargp dicnp
    case argt of
      T.In -> 
        do arg' <- createExpression arg
           return $ arg':lr
      otherwise ->
        do dicn <- gets varsLoc
           return $ (fromJust $ DM.lookup (TE.unpack $ fromJust $ MyAST.astToId arg) dicn) : lr


createArguments _ [] [] = return []


branch :: Name -> Named Terminator
branch label = Do $ Br label [] 


condBranch :: Operand -> Name -> Name -> Named Terminator
condBranch op true false = Do $ CondBr op true false []


genGuards :: [MyAST.AST T.Type] -> Name -> Name -> LLVM ()
genGuards (guard:[]) none one  = do
    genGuard guard none


genGuards (guard:xs) none one = do
    next <- newLabel
    genGuard guard next
    setLabel next $ branch one
    genGuards xs none one 


genGuard :: MyAST.AST T.Type -> Name -> LLVM ()
genGuard (MyAST.Guard guard acc _ _) next = do
    tag  <- createExpression guard
    code <- newLabel
    setLabel code $ condBranch tag code next
    createInstruction acc



createExpression :: MyAST.AST T.Type -> LLVM (Operand)
createExpression (MyAST.ID _ id t) = do
    var <- gets varsLoc
    let (n, ty) = (TE.unpack id, toType t)
    let check   = DM.lookup n var
   
    case check of 
    { Just _  -> do val <- load n ty
                    return val
    ; Nothing -> do return $ local ty (Name n)
    }


createExpression (MyAST.ArrCall _ id' accs t) = do
    accs' <- mapM createExpression accs
    map  <- gets varsLoc
    let (t', i, id) = (toType t, fromJust $ DM.lookup id map, TE.unpack id')
    accs'' <- opsToArrayIndex id accs'
    add <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
    addUnNamedInstruction t' $ Load False add Nothing 0 []


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
    

createExpression (MyAST.String _ msg _) = do
    let n  = fromIntegral $ Prelude.length msg + 1
    let ty = ArrayType n i8 
    name <- newLabel 
    addString msg name ty
    return $ ConstantOperand $ C.GetElementPtr True (global i8 name) [C.Int 64 0, C.Int 64 0]


createExpression (MyAST.Convertion tType _ exp t) = do
    let t' = MyAST.tag exp 
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irConvertion tType t' exp'


--Potencia Integer
createExpression (MyAST.Arithmetic MyAST.Exp _ lexp rexp T.MyInt) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    a     <- intToDouble lexp'
    b     <- intToDouble rexp'
    val   <- addUnNamedInstruction double $ Call False CC.C [] (Right ( definedFunction double 
                                              (Name "llvm.pow.f64"))) [(a, []),(b, [])] [] []  
    doubleToInt val


--Minimo Integer
createExpression (MyAST.Arithmetic MyAST.Min _ lexp rexp T.MyInt) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    a     <- intToDouble lexp'
    b     <- intToDouble rexp'
    val   <- addUnNamedInstruction double $ Call False CC.C [] (Right ( definedFunction double 
                                              (Name "llvm.minnum.f64"))) [(a, []),(b, [])] [] []  
    doubleToInt val


--Maximo Integer
createExpression (MyAST.Arithmetic MyAST.Max _ lexp rexp T.MyInt) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    a     <- intToDouble lexp'
    b     <- intToDouble rexp'
    val   <- addUnNamedInstruction double $ Call False CC.C [] (Right ( definedFunction double 
                                              (Name "llvm.maxnum.f64"))) [(a, []),(b, [])] [] []  
    doubleToInt val


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


--ValorAbs Integer
createExpression (MyAST.Unary MyAST.Abs _ exp T.MyInt) = do
    exp' <- createExpression exp
    x     <- intToDouble exp'
    val   <- addUnNamedInstruction double $ Call False CC.C [] (Right ( definedFunction double 
                                              (Name "llvm.fabs.f64"))) [(x, [])] [] []
    doubleToInt val


--Raiz Integer
createExpression (MyAST.Unary MyAST.Sqrt _ exp t) = do
    let ty = MyAST.tag exp
    exp'  <- createExpression exp

    case ty of 
    { T.MyFloat -> addUnNamedInstruction (toType ty) $ irUnary MyAST.Sqrt ty exp' 
    ; T.MyInt   -> do x <- intToDouble exp'
                      addUnNamedInstruction double $ Call False CC.C [] (Right ( definedFunction double 
                                                       (Name "llvm.sqrt.f64"))) [(x, [])] [] []
    }

createExpression (MyAST.Unary op _ exp t) = do
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irUnary op t exp' 


createExpression (MyAST.FCallExp fname st _ args t) = do
    exp <- mapM createExpression args
    let ty   =  toType t 
    let exp' = map (\i -> (i,[])) exp
    let op   = definedFunction ty (Name $ TE.unpack fname)
    val <- addUnNamedInstruction ty $ Call False CC.C [] (Right op) exp' [] []
    return val

createExpression (MyAST.Cond lguards _ rtype) = do
   final  <- newLabel 
   none   <- newLabel
   lnames <- genExpGuards lguards none final
   let rtype' = toType rtype
   setLabel none $ branch final
   setLabel final $ Do $ Unreachable []
   addUnNamedInstruction rtype' $ Phi rtype' lnames []


genExpGuards :: [MyAST.AST T.Type] -> Name -> Name -> LLVM ([(Operand, Name)])
genExpGuards (guard:[]) none one  = do
    r <- genExpGuard guard none
    return [r]

genExpGuards (guard:xs) none one = do
    next <- newLabel
    r <- genExpGuard guard next
    setLabel next $ branch one
    rl <- genExpGuards xs none one 
    return $ r:rl

genExpGuard :: MyAST.AST T.Type -> Name -> LLVM (Operand, Name)
genExpGuard (MyAST.GuardExp guard acc _ _) next = do
    tag  <- createExpression guard
    code <- newLabel
    setLabel code $ condBranch tag code next
    n <- createExpression acc
    return (n, code)

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
irArithmetic MyAST.Exp T.MyFloat a b = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.pow.f64"))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Min T.MyFloat a b = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.minnum.f64"))) [(a, []),(b, [])] [] []
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
irUnary MyAST.Abs   T.MyFloat a = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.fabs.f64"))) [(a, [])] [] []
irUnary MyAST.Sqrt  T.MyFloat a = Call False CC.C [] (Right ( definedFunction double 
                                         (Name "llvm.sqrt.f64"))) [(a, [])] [] []
