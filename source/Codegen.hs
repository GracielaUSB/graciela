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
import Data.Range.Range                                  as RA
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Attribute
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.Module
import Data.Foldable (toList)
import CodegenState
import SymbolTable
import Data.Either
import Data.Maybe
import Data.Word
import Data.Char
import Contents
import Location
import Aborts 
import Limits


writeLnInt    = "_writeLnInt"
writeLnBool   = "_writeLnBool"
writeLnChar   = "_writeLnChar"
writeLnDouble = "_writeLnDouble"
writeLnString = "puts"
writeInt      = "_writeInt"
writeBool     = "_writeBool"
writeChar     = "_writeChar"
writeDouble   = "_writeDouble"
writeString   = "puts"
randomInt     = "_random"
sqrtString    = "llvm.sqrt.f64"
fabsString    = "llvm.fabs.f64"
minnumString  = "_min"
maxnumString  = "_max"
minnumFstring = "_minF"
maxnumFtring  = "_maxF"
powString     = "llvm.pow.f64"


createParameters :: [(Name, Type)] -> [[ParameterAttribute]] -> ([Parameter], Bool)
createParameters names attrs = (map (\((name, t), attr) -> Parameter t name attr) (zip names attrs), False)


createEmptyParameters :: [(Name, Type)] -> ([Parameter], Bool)
createEmptyParameters names = (map (\(name, t) -> Parameter t name []) names, False)


createPreDef ::  LLVM () 
createPreDef = do

    addDefinition randomInt (createParameters [] []) intType

    addDefinition abortString (createEmptyParameters [(Name "x", intType), 
          (Name "line", intType), (Name "column", intType)]) voidType

    let intParams = createEmptyParameters [(Name "x", intType)]
    addDefinition writeLnInt intParams voidType
    addDefinition writeInt   intParams voidType

    let intParams2 = createEmptyParameters [(Name "x", intType), (Name "y", intType)]
    addDefinition minnumString intParams2 intType
    addDefinition maxnumString intParams2 intType

    let charParams = createEmptyParameters [(Name "x", charType)]
    addDefinition writeChar   charParams voidType
    addDefinition writeLnChar charParams voidType

    let boolParams = createEmptyParameters [(Name "x", boolType)]
    addDefinition writeLnBool boolParams voidType
    addDefinition writeBool   boolParams voidType

    let doubleParams = createEmptyParameters [(Name "x", floatType)]
    addDefinition writeLnDouble doubleParams voidType
    addDefinition writeDouble   doubleParams voidType
    addDefinition sqrtString    doubleParams doubleType
    addDefinition fabsString    doubleParams doubleType

    let doubleParams2 = createEmptyParameters [(Name "x", floatType), (Name "y", floatType)]
    addDefinition minnumFstring doubleParams2 doubleType
    addDefinition maxnumFtring  doubleParams2 doubleType
    addDefinition powString     doubleParams2 doubleType

    let stringParams = createParameters [(Name "msg", stringType)] [[NoCapture]]
    addDefinition writeLnString stringParams intType
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
    addDefinition "main" ([],False) voidType


convertID :: String -> String
convertID name = '_':name


addArgOperand :: [(String, Contents SymbolTable)] -> LLVM ()
addArgOperand [] = return ()
addArgOperand ((id',c):xs) = do
    let t  = toType $ symbolType c
    let tp = procArgType c 
    let id = convertID id'
    let exp' = local t (Name id')
    case tp of
      T.InOut -> 
        do exp <- addUnNamedInstruction t $ Load False exp' Nothing 0 []
           op <- alloca Nothing t id
           store t op exp
           addVarOperand id' op
           return ()
      T.In ->
        do op <- alloca Nothing t id
           store t op exp'
           addVarOperand id' op
           return ()
      T.Out -> 
        do op <- alloca Nothing t id
           store t op $ constantInt 0
           addVarOperand id' op
           return ()
      T.Ref -> 
        do addVarOperand id' exp'
           return ()
    addArgOperand xs
    




retVarOperand :: [(String, Contents SymbolTable)] -> LLVM ()
retVarOperand [] = return()
retVarOperand ((id', c):xs) = do
    let t = toType $ symbolType c
    let exp = local t (Name id')
    let tp = procArgType c 
    case tp of
      T.InOut -> 
        do add <- load id' t 
           store t exp add
           return ()
      T.Out -> 
        do add <- load id' t 
           store t exp add
           return ()
      T.In ->
        return ()
      T.Ref ->
        return ()
    retVarOperand xs


createState :: String -> MyAST.AST T.Type -> LLVM ()
createState name (MyAST.States cond loc exp _) = do 

    e' <- createExpression exp
    next     <- newLabel
    warAbort <- newLabel

    case cond of
    { MyAST.Pre        -> do let checkPre = "_resPre" ++ name
                             op <- alloca Nothing boolType checkPre
                             store boolType op e'
                             addVarOperand checkPre op
                             setLabel warAbort $ condBranch e' next warAbort
                             createTagPre next loc 

    ; MyAST.Post       -> do let checkPre = "_resPre" ++ name
                             op <- load checkPre boolType
                             a      <- addUnNamedInstruction boolType $ irUnary   MyAST.Not T.MyBool op
                             check  <- addUnNamedInstruction boolType $ irBoolean MyAST.Dis a e' 
                             setLabel warAbort $ condBranch check next warAbort
                             createTagPost next loc
    
    ; MyAST.Assertion -> do setLabel warAbort $ condBranch e' next warAbort
                            createTagAsert next loc
 
    ; MyAST.Invariant -> do setLabel warAbort $ condBranch e' next warAbort
                            createTagInv next loc
   
    ; MyAST.Bound     -> do checkZero <- newLabel
                            warAbort' <- newLabel

                            op    <- load name intType
                            check <- addUnNamedInstruction intType $ irRelational MyAST.Less T.MyInt e' op
                            setLabel checkZero $ condBranch check checkZero warAbort

                            check' <- addUnNamedInstruction intType $ irRelational MyAST.LEqual T.MyInt e' $ constantInt 0
                            var    <- getVarOperand name
                            store intType var e'
                            setLabel warAbort $ condBranch check' warAbort' next

                            createTagBound warAbort' loc 1
                            createTagBound next      loc 2
    }

    return ()


createDef :: MyAST.AST T.Type -> LLVM()
createDef (MyAST.DefProc name st accs pre post bound decs params _) = do
    
    let name' = (TE.unpack name)
    mapM_ accToAlloca decs
    createState name' pre
    let args     = map (\(id, _) -> (TE.unpack id, fromJust $ checkSymbol id st)) params
    let args'    = ([Parameter t (Name id) [] | (id, t) <- (convertParams args)], False) 
    retTy <- retVoid
    addArgOperand args
    mapM_ createInstruction accs 
    retVarOperand $ reverse args
    createState name' post
    addBasicBlock retTy
    addDefinition name' args' voidType
   
   
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
    let t' = toType t
    alloca dim t' id
    initialize id t
    createInstruction acc


accToAlloca acc@(MyAST.LAssign lids _ _ _) = do
    mapM_ idToAlloca lids
    createInstruction acc


idToAlloca :: MyAST.AST T.Type -> LLVM()
idToAlloca (MyAST.ID _ id t) = do
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
      Just op -> fmap Just $ addUnNamedInstruction intType $ irArithmetic MyAST.Mul T.MyInt op d

typeToOperand _  _             = return $ Nothing


procedureCall :: Type -> [Char] -> [Operand] -> LLVM Operand
procedureCall t pname es = do
    let es' = map (\e -> (e, [])) es
    let df  = Right $ definedFunction t (Name pname)
    caller t df es' 


getStoreDir :: MyAST.AST T.Type -> LLVM Operand
getStoreDir (MyAST.ID _ name _) = getVarOperand (TE.unpack name)
getStoreDir (MyAST.ArrCall _ name exps _) = do
    ac' <- mapM createExpression exps
    map <- gets varsLoc
    let (i, id) = (fromJust $ DM.lookup id map, TE.unpack name)
    ac'' <- opsToArrayIndex id ac'
    addUnNamedInstruction intType $ GetElementPtr True i [ac''] []

 
createAssign :: MyAST.AST T.Type -> MyAST.AST T.Type -> LLVM () 
createAssign id e = do
    e'  <- createExpression e
    id' <- getStoreDir id 
    let t' = toType $ MyAST.tag id
    store t' id' e'
    return ()


createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.EmptyAST _ ) = return ()
createInstruction (MyAST.ID _ _ _)    = return ()
createInstruction (MyAST.Skip _ _)    = return ()


createInstruction (MyAST.Abort loc _) = do
    createTagAbort loc
    return ()


createInstruction (MyAST.GuardAction _ assert action ty) = do
    createState "" assert
    createInstruction action


createInstruction (MyAST.LAssign ids exps _ _) = do
    mapM_ (uncurry createAssign) $ zip ids exps


createInstruction (MyAST.Write True exp _ t) = do
    let ty  = MyAST.tag exp 
    let ty' = toType t
    e' <- createExpression exp

    case ty of
    { T.MyInt    -> procedureCall ty' writeLnInt [e']
    ; T.MyFloat  -> procedureCall ty' writeLnDouble [e']
    ; T.MyBool   -> procedureCall ty' writeLnBool [e']
    ; T.MyChar   -> procedureCall intType writeLnChar [e']   
    ; T.MyString -> do let msj = lines $ MyAST.mstring exp
                       procedureCall ty' writeLnString [e']
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
    ; T.MyChar   -> procedureCall intType writeChar [e']
    ; T.MyString -> procedureCall ty' writeString [e']
    }
    return ()


createInstruction (MyAST.Block _ st decs accs _) = do
    mapM_ accToAlloca decs
    mapM_ createInstruction accs
    return ()


createInstruction (MyAST.Cond guards loc _) = do
    final <- newLabel
    abort <- newLabel
    genGuards guards abort final

    setLabel abort $ branch final
    createTagIf final loc

    return ()


createInstruction (MyAST.Rept guards inv bound _ _) = do
    final   <- newLabel
    initial <- newLabel
    createState "" inv

    --Bound
    name <- getCount
    let boundName = show name
    op <- alloca Nothing intType boundName
    store intType op $ constantInt maxInteger
    addVarOperand (show boundName) op

    setLabel initial $ branch initial
    createState boundName bound
    genGuards guards final initial 
    setLabel final $ branch initial
    return ()


createInstruction (MyAST.ProcCall pname st _ args _) = do
    let c     = fromJust $ checkSymbol pname st
    let dic   = getMap $ getActual $ sTable $ c
    let nargp = nameArgs c
    exp <- createArguments dic nargp args

    procedureCall voidType (TE.unpack pname) exp
    return ()


createInstruction (MyAST.Ran id _ _ t) = do
    vars <- gets varsLoc
    let (ty, i) = (toType t, fromJust $ DM.lookup (TE.unpack id) vars)
    let df      = Right $ definedFunction floatType (Name randomInt)
    val <- caller ty df [] 
    store ty i val
    return ()


createArguments :: DM.Map TE.Text (Contents SymbolTable)
                    -> [TE.Text] -> [MyAST.AST T.Type] -> LLVM [Operand]
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
    return $ constantInt n


createExpression (MyAST.Float _ n _) = do
    return $ constantFloat n


createExpression (MyAST.Bool _ True  _) = do
   return $ constantBool 1 
 

createExpression (MyAST.Bool _ False _) = do
   return $ constantBool 0 
 

createExpression (MyAST.Char _ n _) = do
    return $ constantChar n


createExpression (MyAST.String _ msg _) = do
    let n  = fromIntegral $ Prelude.length msg + 1
    let ty = ArrayType n charType 
    name <- newLabel 

    addString msg name ty
    return $ ConstantOperand $ C.GetElementPtr True (global charType name) [C.Int 64 0, C.Int 64 0]


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
    val   <- addUnNamedInstruction floatType $ irArithmetic MyAST.Exp T.MyFloat a b 
    doubleToInt val


createExpression (MyAST.Arithmetic op loc lexp rexp ty) = do

    lexp' <- createExpression lexp
    rexp' <- createExpression rexp

    case op of
    {
    ; MyAST.Div -> checkDivZero op loc lexp' rexp' ty
    ; MyAST.Mod -> checkDivZero op loc lexp' rexp' ty
    ; otherwise -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'
    }


createExpression (MyAST.Boolean op _ lexp rexp t) = do

    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    
    case op of
    { MyAST.Implies -> do notA <- addUnNamedInstruction boolType $ irUnary MyAST.Not T.MyBool lexp'
                          addUnNamedInstruction boolType $ irBoolean MyAST.Dis notA rexp'
   
    ; MyAST.Conse   -> do notA <- addUnNamedInstruction boolType $ irUnary MyAST.Not T.MyBool rexp'
                          addUnNamedInstruction boolType $ irBoolean MyAST.Dis notA lexp'
    
    ; otherwise     -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'
    }
 

createExpression (MyAST.Relational op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    let t' = MyAST.tag lexp 
    addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


--ValorAbs Integer
createExpression (MyAST.Unary MyAST.Abs _ exp T.MyInt) = do
    exp' <- createExpression exp
    x     <- intToDouble exp'
    val   <- addUnNamedInstruction intType $ irUnary MyAST.Abs T.MyFloat x
    doubleToInt val


--Raiz Integer
createExpression (MyAST.Unary MyAST.Sqrt _ exp t) = do
    let ty = MyAST.tag exp
    let df = Right $ definedFunction floatType (Name sqrtString)
    exp'  <- createExpression exp

    case ty of 
    { T.MyFloat -> addUnNamedInstruction floatType $ irUnary MyAST.Sqrt ty exp' 
    ; T.MyInt   -> do x <- intToDouble exp'
                      addUnNamedInstruction floatType $ irUnary MyAST.Sqrt T.MyFloat x
    }


createExpression (MyAST.Unary op _ exp t) = do
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irUnary op t exp' 


createExpression (MyAST.FCallExp fname st _ args t) = do
    exp <- mapM createExpression args
    let ty   =  toType t 
    let exp' = map (\i -> (i,[])) exp
    let op   = definedFunction ty (Name $ TE.unpack fname)
    caller ty (Right op) exp'


createExpression (MyAST.Cond lguards _ rtype) = do
   final  <- newLabel 
   none   <- newLabel
   lnames <- genExpGuards lguards none final
   let rtype' = toType rtype
   setLabel none $ branch final
   setLabel final $ Do $ Unreachable []
   addUnNamedInstruction rtype' $ Phi rtype' lnames []


createExpression (MyAST.QuantRan opQ varQ loc rangeExp termExp t) = do
   
    let name = TE.unpack varQ

    case opQ of
    { MyAST.ForAll -> do check <- mapM (createQuant True  opQ name loc termExp) rangeExp
                         res   <- joinRange opQ check loc
                         return $ res 

    ; MyAST.Exists -> do check <- mapM (createQuant True  opQ name loc termExp) rangeExp
                         res   <- joinRange opQ check loc
                         return $ res

    ; otherwise    -> do check <- mapM (createQuant False opQ name loc termExp) rangeExp
                         res   <- joinRange opQ check loc
                         return $ res

    }


joinRange :: MyAST.OpQuant -> [Operand] -> Location -> LLVM (Operand)
joinRange MyAST.Summation res _ =
    foldM (\acc i -> do ret <- addUnNamedInstruction intType $ irArithmetic MyAST.Sum T.MyInt acc i
                        return ret) (head res) (tail res)

joinRange MyAST.Product res _ =
    foldM (\acc i -> do ret <- addUnNamedInstruction intType $ irArithmetic MyAST.Mul T.MyInt acc i
                        return ret) (head res) (tail res)

joinRange MyAST.Maximum res _ =
    foldM (\acc i -> do ret <- addUnNamedInstruction intType $ irArithmetic MyAST.Max T.MyInt acc i
                        return ret) (head res) (tail res)

joinRange MyAST.Minimum res _ =
    foldM (\acc i -> do ret <- addUnNamedInstruction intType $ irArithmetic MyAST.Min T.MyInt acc i
                        return ret) (head res) (tail res)

joinRange opQ res loc = do
    warAbort <- newLabel
    next     <- newLabel
    check <- foldM (\acc i -> do ret <- addUnNamedInstruction intType $ irBoolean MyAST.Con acc i
                                 return ret) (head res) (tail res)
    setLabel warAbort $ condBranch check next warAbort

    case opQ of 
    { MyAST.ForAll -> createTagForAll next loc
    ; MyAST.Exists -> createTagExists next loc
    }
    
    return check


createQuant :: Bool -> MyAST.OpQuant -> String -> Location -> 
                   MyAST.AST T.Type -> Range Integer -> LLVM (Operand)
createQuant True opQ var loc exp (SpanRange a b) = do
   
    let ini = constantInt a
    let fin = constantInt b
    op <- alloca Nothing intType var
    store intType op ini
    addVarOperand var op   

    initial <- newLabel
    code    <- newLabel
    final   <- newLabel

    name <- getCount
    let varBool = show name
    op' <- alloca Nothing boolType varBool
    store boolType op' $ constantBool 1
    addVarOperand varBool op'
    setLabel initial $ branch initial

    varQ   <- load var intType
    check' <- addUnNamedInstruction boolType $ irRelational MyAST.LEqual T.MyInt varQ fin 

    checkBool <- load varBool boolType
    tag       <- addUnNamedInstruction boolType $ irBoolean MyAST.Con check' checkBool
    setLabel code $ condBranch tag code final

    e'   <- createExpression exp
    sum' <- addUnNamedInstruction boolType $ irArithmetic MyAST.Sum T.MyInt varQ $ constantInt 1 
    store intType  op  sum' 

    case opQ of 
    { MyAST.ForAll -> store boolType op' e'
    ; MyAST.Exists -> do bool <- addUnNamedInstruction boolType $ irUnary MyAST.Not T.MyBool e'
                         store boolType op' bool 
    }

    setLabel final $ branch initial

    case opQ of 
    { MyAST.ForAll -> do checkBool' <- load varBool boolType
                         return checkBool'
    ; MyAST.Exists -> do checkBool' <- load varBool boolType
                         res <- addUnNamedInstruction boolType $ irUnary MyAST.Not T.MyBool checkBool'
                         return res 
    }


createQuant False opQ var loc exp (SpanRange a b) = do
   
    let ini = constantInt a
    let fin = constantInt b
    op <- alloca Nothing intType var
    store intType op ini
    addVarOperand var op   

    initial <- newLabel
    code    <- newLabel
    final   <- newLabel

    let varInt = "Quant_" ++ var 
    op' <- alloca Nothing intType varInt

    case opQ of
    { MyAST.Summation -> store intType op' $ constantInt 0
    ; MyAST.Product   -> store intType op' $ constantInt 1
    ; MyAST.Maximum   -> store intType op' $ constantInt minInteger
    ; MyAST.Minimum   -> store intType op' $ constantInt maxInteger
    }

    addVarOperand varInt op'

    setLabel initial $ branch initial
    varQ <- load var intType
    tag  <- addUnNamedInstruction boolType $ irRelational MyAST.LEqual T.MyInt varQ fin 
    res  <- load varInt intType
   
    setLabel code $ condBranch tag code final
    e'   <- createExpression exp
    sum  <- addUnNamedInstruction boolType $ irArithmetic MyAST.Sum T.MyInt varQ $ constantInt 1 
    store intType op  sum 

    case opQ of
    { MyAST.Summation -> do check <- addUnNamedInstruction intType $ irArithmetic MyAST.Sum T.MyInt res e'
                            store intType op' check 
    ; MyAST.Product   -> do check <- addUnNamedInstruction intType $ irArithmetic MyAST.Mul T.MyInt res e'
                            store intType op' check 
    ; MyAST.Maximum   -> do check <- addUnNamedInstruction intType $ irArithmetic MyAST.Max T.MyInt res e' 
                            store intType op' check 
    ; MyAST.Minimum   -> do check <- addUnNamedInstruction intType $ irArithmetic MyAST.Min T.MyInt res e' 
                            store intType op' check 
    }


    setLabel final $ branch initial
    return $ res


checkDivZero :: MyAST.OpNum -> Location -> Operand -> Operand -> T.Type -> LLVM Operand
checkDivZero op loc lexp' rexp' ty = do 
    
    next  <- newLabel
    abort <- newLabel
    
    case ty of 
    { T.MyInt   -> do let zero = constantInt 0
                      check <- addUnNamedInstruction intType $ ICmp IL.EQ rexp' zero []
                      setLabel abort $ condBranch check abort next 
                      createTagZero next loc
                      addUnNamedInstruction intType $ irArithmetic op ty lexp' rexp' 
   
    ; T.MyFloat -> do let zero = constantFloat 0.0
                      check <- addUnNamedInstruction floatType $ FCmp FL.OEQ rexp' zero []
                      setLabel abort $ condBranch check abort next 
                      createTagZero next loc
                      addUnNamedInstruction floatType $ irArithmetic op ty lexp' rexp' 
    }


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
irArithmetic MyAST.Exp T.MyFloat a b = Call False CC.C [] (Right ( definedFunction floatType 
                                         (Name powString)))    [(a, []),(b, [])] [] []
irArithmetic MyAST.Min T.MyInt   a b = Call False CC.C [] (Right ( definedFunction intType 
                                         (Name minnumString))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Min T.MyFloat a b = Call False CC.C [] (Right ( definedFunction floatType 
                                         (Name minnumFstring))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Max T.MyInt   a b = Call False CC.C [] (Right ( definedFunction intType 
                                         (Name maxnumString))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Max T.MyFloat a b = Call False CC.C [] (Right ( definedFunction floatType 
                                         (Name maxnumFtring))) [(a, []),(b, [])] [] []


irBoolean :: MyAST.OpBool -> Operand -> Operand -> Instruction
irBoolean MyAST.Con a b = And a b []
irBoolean MyAST.Dis a b = Or  a b []


irRelational :: MyAST.OpRel -> T.Type -> Operand -> Operand -> Instruction
irRelational MyAST.Equ     T.MyFloat a b = FCmp FL.OEQ a b []
irRelational MyAST.Less    T.MyFloat a b = FCmp FL.OLT a b []
irRelational MyAST.Greater T.MyFloat a b = FCmp FL.OGT a b []
irRelational MyAST.LEqual  T.MyFloat a b = FCmp FL.OLE a b []
irRelational MyAST.GEqual  T.MyFloat a b = FCmp FL.OGE a b []
irRelational MyAST.Ine     T.MyFloat a b = FCmp FL.OEQ a b [] 


irRelational MyAST.Equ     T.MyInt   a b = ICmp IL.EQ a b []
irRelational MyAST.Less    T.MyInt   a b = ICmp IL.SLT a b []
irRelational MyAST.Greater T.MyInt   a b = ICmp IL.SGT a b []
irRelational MyAST.LEqual  T.MyInt   a b = ICmp IL.SLE a b []
irRelational MyAST.GEqual  T.MyInt   a b = ICmp IL.SGE a b []
irRelational MyAST.Ine     T.MyInt   a b = ICmp IL.EQ a b []


irConvertion :: MyAST.Conv -> T.Type -> Operand -> Instruction
irConvertion MyAST.ToInt    T.MyFloat a = FPToSI a intType   [] 
irConvertion MyAST.ToInt    T.MyChar  a = FPToSI a intType   [] 
irConvertion MyAST.ToDouble T.MyInt   a = SIToFP a floatType [] 
irConvertion MyAST.ToDouble T.MyChar  a = SIToFP a floatType [] 
irConvertion MyAST.ToChar   T.MyInt   a = Trunc  a charType  [] 
irConvertion MyAST.ToChar   T.MyFloat a = FPToSI a charType  [] 


irUnary :: MyAST.OpUn -> T.Type -> Operand -> Instruction
irUnary MyAST.Minus T.MyInt   a = Sub False False      (constantInt 0) a []
irUnary MyAST.Minus T.MyFloat a = FSub NoFastMathFlags (constantFloat 0) a []
irUnary MyAST.Not   T.MyBool  a = Xor a (constantBool 1) [] 
irUnary MyAST.Abs   T.MyFloat a = Call False CC.C [] (Right ( definedFunction floatType 
                                         (Name fabsString))) [(a, [])] [] []
irUnary MyAST.Sqrt  T.MyFloat a = Call False CC.C [] (Right ( definedFunction floatType 
                                         (Name sqrtString))) [(a, [])] [] []
