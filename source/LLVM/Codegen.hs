module LLVM.Codegen where

--------------------------------------------------------------------------------
import Aborts
import LLVM.CodegenState
import Contents
import Limits
import qualified AST                                     as MyAST
import qualified Type                                    as T
import SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                           (use, (.=), (%=))
import           Control.Monad.State
import           Data.Foldable                          (toList)
import           Data.Maybe
import           Data.Range.Range                        as RA
import           Data.Word
import qualified Data.Map                                as DM
import qualified Data.Text                               as TE
import           LLVM.General.AST                        as AST
import           LLVM.General.AST.Attribute
import           LLVM.General.AST.Type
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           System.Info                            (os,arch)
import           System.Process                         (callCommand)
import           Text.Megaparsec.Pos                    (SourcePos)
--------------------------------------------------------------------------------

writeLnInt    = "_writeLnInt"
writeLnBool   = "_writeLnBool"
writeLnChar   = "_writeLnChar"
writeLnDouble = "_writeLnDouble"
writeLnString = "_writeLnString"
writeInt      = "_writeInt"
writeBool     = "_writeBool"
writeChar     = "_writeChar"
writeDouble   = "_writeDouble"
writeString   = "_writeString"
randomInt     = "_random"
sqrtString    = "llvm.sqrt.f64"
fabsString    = "llvm.fabs.f64"
powString     = "llvm.pow.f64"
minnumString  = "_min"
maxnumString  = "_max"
minnumFstring = "_minF"
maxnumFtring  = "_maxF"
readIntStd    = "_readIntStd"
readCharStd   = "_readCharStd"
readDoubleStd = "_readDoubleStd"
openFileStr   = "_openFile"
readFileInt   = "_readFileInt"
closeFileStr  = "_closeFile"
readFileChar  = "_readFileChar"
readFileDouble= "_readFileDouble"
intAdd        = "llvm.sadd.with.overflow.i32"
intSub        = "llvm.ssub.with.overflow.i32"
intMul        = "llvm.smul.with.overflow.i32"


createParameters :: [(Name, Type)] -> [[ParameterAttribute]] -> ([Parameter], Bool)
createParameters names attrs = (map (\((name, t), attr) -> Parameter t name attr) (zip names attrs), False)


createEmptyParameters :: [(Name, Type)] -> ([Parameter], Bool)
createEmptyParameters names = (map (\(name, t) -> Parameter t name []) names, False)


createPreDef :: [String] -> LLVM ()
createPreDef files = do

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
  addDefinition writeLnChar charParams voidType
  addDefinition writeChar   charParams voidType

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
  addDefinition writeString   stringParams intType

  let overflow' = StructureType False [intType, boolType]

  addDefinition intAdd intParams2 overflow'
  addDefinition intSub intParams2 overflow'
  addDefinition intMul intParams2 overflow'

  addDefinition readIntStd    (createEmptyParameters []) intType
  addDefinition readCharStd   (createEmptyParameters []) charType
  addDefinition readDoubleStd (createEmptyParameters []) double

  addDefinition openFileStr (createEmptyParameters [(Name "nombreArchivo", ptr pointerType)]) (ptr pointerType)

  mapM_ addFile files

  addDefinition readFileInt    (createEmptyParameters [(Name "f", ptr pointerType)]) intType
  addDefinition readFileChar   (createEmptyParameters [(Name "f", ptr pointerType)]) charType
  addDefinition readFileDouble (createEmptyParameters [(Name "f", ptr pointerType)]) doubleType
  addDefinition closeFileStr   (createEmptyParameters [(Name "f", ptr pointerType)]) voidType

  return ()


convertFile :: String -> String
convertFile file = '_': ('_':file)


addFile :: String -> LLVM ()
addFile file = globalVariable (Name (convertFile file)) (ptr pointerType) (C.Null (ptr pointerType))


astToLLVM :: [String] -> MyAST.AST T.Type -> String -> AST.Module
astToLLVM files (MyAST.Program name _ defs accs _) version =
  defaultModule
    { moduleName         = TE.unpack name
    , moduleDefinitions  = toList . _moduleDefs . execCodegen $ createLLVM files defs accs
    , moduleTargetTriple = Just whichTarget
    }
  where
    whichTarget = case os of
      "darwin"  -> arch++"-apple-macosx" ++ crop version -- With Mac, version needs to end with "0",
      "linux"   -> arch++"-unknown-linux-gnu"            -- example: 11.10.3 -> 11.10.0
      "windows" -> undefined
    crop str = if last str == '.'
        then str++"0"
        else crop $ init str


openFile :: String -> LLVM Operand
openFile file = do
  let file' = convertFile file
  ops <- addFileNameOpe file
  op  <- caller (ptr charType) (Right $ definedFunction (ptr charType) (Name openFileStr)) [(ops, [])]
  store (ptr charType) (AST.ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) op


closeFile :: String -> LLVM Operand
closeFile file = do
  let file' = convertFile file
  -- Cargamos la variable gobal perteneciente al archivo.
  let load' = Load False (AST.ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) Nothing 0 []
  op <- addUnNamedInstruction voidType load'
  caller voidType (Right $ definedFunction voidType (Name closeFileStr)) [(op, [])]


createLLVM :: [String] -> [MyAST.AST T.Type] -> MyAST.AST T.Type -> LLVM ()
createLLVM files defs accs = do
  createPreDef files
  mapM_ createDef defs
  m800 <- retVoid
  mapM_ openFile files
  createInstruction accs
  mapM_ closeFile files
  addBasicBposk m800
  addDefinition "main" ([],False) voidType
  return ()


convertId :: String -> String
convertId name = '_':name

convertId' :: MyAST.AST T.Type -> String
convertId' (MyAST.ArrCall _ id _ _) = "__" ++ TE.unpack id
convertId' (MyAST.Id        _ id _) = "__" ++ TE.unpack id

convertId'' :: MyAST.AST T.Type -> String
convertId'' (MyAST.ArrCall _ id _ _) = "___" ++ TE.unpack id
convertId'' (MyAST.Id        _ id _) = "___" ++ TE.unpack id


addArgOperand :: [(String, Contents SymbolTable)] -> LLVM ()
addArgOperand [] = return ()

addArgOperand ((id',c):xs) = do
  let t    = toType $ argType c
  let tp   = argTypeArg c
  let id   = convertId id'
  let e'   = posal t (Name id')

  void $ case tp of
    T.InOut -> do exp <- addUnNamedInstruction t $ Load False e' Nothing 0 []
                  op  <- alposa Nothing t id
                  store t op exp
                  addVarOperand id' op

    T.In    -> do op <- alposa Nothing t id
                  store t op e'
                  addVarOperand id' op

    T.Out   -> do op <- alposa Nothing t id
                  addVarOperand id' op
                  initialize id $ argType c

    T.Ref   -> addVarOperand id' e'

  addArgOperand xs


retVarOperand :: [(String, Contents SymbolTable)] -> LLVM ()
retVarOperand [] = pure ()

retVarOperand ((id', c):xs) = do

  let t   = toType $ getContentType c
  let exp = posal t (Name id')
  let tp  = argTypeArg c

  case tp of
    T.InOut -> do add <- load id' t
                  store t exp add
                  return ()

    T.Out   -> do add <- load id' t
                  store t exp add
                  return ()

    T.In     -> return ()
    T.Ref    -> return ()

  retVarOperand xs

createState :: String -> MyAST.AST T.Type -> LLVM ()
createState name (MyAST.States cond pos exp _) = do
  e' <- createExpression exp
  next     <- newLabel
  warAbort <- newLabel

  case cond of
    MyAST.Pre        -> do let checkPre = "_resPre" ++ name
                           op <- alposa Nothing boolType checkPre
                           store boolType op e'
                           addVarOperand checkPre op
                           setLabel warAbort $ condBranch e' next warAbort
                           createTagPre next pos

    MyAST.Post       -> do let checkPre = "_resPre" ++ name
                           op     <- load checkPre boolType
                           a      <- addUnNamedInstruction boolType $ _not op
                           check  <- addUnNamedInstruction boolType $ _or a e'
                           setLabel warAbort $ condBranch check next warAbort
                           createTagPost next pos

    MyAST.Assertion -> do setLabel warAbort $ condBranch e' next warAbort
                          createTagAsert next pos

    MyAST.Invariant -> do setLabel warAbort $ condBranch e' next warAbort
                          createTagInv next pos

    MyAST.Bound     -> do checkZero <- newLabel
                          warAbort' <- newLabel

                          op    <- load name intType
                          check <- addUnNamedInstruction intType $ _less e' op
                          setLabel checkZero $ condBranch check checkZero warAbort

                          check' <- addUnNamedInstruction intType $ _less e' $ constantInt 0
                          var    <- getVarOperand name
                          store intType var e'
                          setLabel warAbort $ condBranch check' warAbort' next

                          createTagBound warAbort' pos 1
                          createTagBound next      pos 2
  return ()


addFuncParam :: (TE.Text, T.Type) -> LLVM ()
addFuncParam (id'', t@(T.GArray _ _)) = do
  let id = TE.unpack id''
  let e'   = posal (toType t) (Name id)
  addVarOperand id e'
  return ()
addFuncParam _ = return ()


createDef :: MyAST.AST T.Type -> LLVM ()
createDef (MyAST.DefProc name st accs pre post bound decs params _) = do
  let name' = TE.unpack name
  let args  = map (\(id, _) -> (TE.unpack id, fromJust $ checkSymbol id st)) params
  let args' = ([Parameter t (Name id) [] | (id, t) <- convertParams args], False)
  retTy <- retVoid
  addArgOperand args
  mapM_ accToAlposa decs
  createState name' pre
  createInstruction accs
  retVarOperand $ reverse args
  createState name' post
  addBasicBposk retTy
  addDefinition name' args' voidType


createDef (MyAST.DefFun fname st _ exp reType bound params _) = do
  let args' = ([Parameter t (Name id) [] | (id, t) <- convertFuncParams params], False)
  mapM_ addFuncParam params
  exp'  <- createExpression exp
  retTy <- retType exp'
  addBasicBposk retTy
  addDefinition (TE.unpack fname) args' (toType reType)


accToAlposa :: MyAST.AST T.Type -> LLVM ()
accToAlposa acc@(MyAST.Id _ id' t) = do
  let id = TE.unpack id'
  dim <- typeToOperand id t
  let t' = toType t
  alposa dim t' id

  case t of
    T.GArray d ty -> createInstruction acc
    _             -> do
      initialize id t
      createInstruction acc

accToAlposa acc@(MyAST.LAssign lids _ _ _) = do
  mapM_ idToAlposa lids
  createInstruction acc

accToAlposa (MyAST.Read _ Nothing types vars _) = do
  res <- mapM callRead types
  ads <- mapM (getVarOperand . TE.unpack . fst) vars
  mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
  return ()

accToAlposa (MyAST.Read _ (Just arch) types vars _) = do
  res <- mapM (callReadFile arch) types
  ads <- mapM (getVarOperand . TE.unpack . fst) vars
  mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
  return ()


callReadFile :: String -> T.Type -> LLVM Operand
callReadFile arch T.GInt = do
  let i = AST.ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller intType (Right $ definedFunction intType (Name readFileInt)) [(op, [])]

callReadFile arch T.GFloat = do
  let i = AST.ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller doubleType (Right $ definedFunction doubleType (Name readFileDouble)) [(op, [])]

callReadFile arch T.GChar = do
  let i = AST.ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller charType (Right $ definedFunction charType (Name readFileChar)) [(op, [])]


callRead :: T.Type -> LLVM Operand
callRead T.GInt   =
  caller intType   (Right $ definedFunction intType   (Name readIntStd))    []

callRead T.GChar  =
  caller charType  (Right $ definedFunction charType  (Name readCharStd))   []

callRead T.GFloat =
  caller floatType (Right $ definedFunction floatType (Name readDoubleStd)) []


idToAlposa :: MyAST.AST T.Type -> LLVM ()
idToAlposa (MyAST.Id _ id t) = do
  let id' = TE.unpack id
  dim <- typeToOperand id' t
  alposa dim (toType t) id'
  return ()


typeToOperand :: String -> T.Type -> LLVM (Maybe Operand)
typeToOperand name (T.GArray dim ty) = do
  r <- typeToOperand name ty
  d <- dimToOperand dim
  addDimToArray name d

  case r of
    Nothing -> return $ return d
    Just op -> fmap Just $ addUnNamedInstruction intType $ _mul op d

typeToOperand _ _ = return Nothing


procedureCall :: Type -> String -> [Operand] -> LLVM Operand
procedureCall t pname es = do
  let es' = map (\e -> (e, [])) es
  let df  = Right $ definedFunction t (Name pname)
  caller t df es'


getStoreDir :: MyAST.AST T.Type -> LLVM Operand
getStoreDir (MyAST.Id _ name _) = getVarOperand (TE.unpack name)
getStoreDir (MyAST.ArrCall _ name exps _) = do
  ac' <- mapM createExpression exps
  map <- use varsLoc
  let (i, id) = (fromJust $ DM.lookup id map, TE.unpack name)
  ac'' <- opsToArrayIndex id ac'
  addUnNamedInstruction intType $ GetElementPtr True i [ac''] []


createAssign :: MyAST.AST T.Type -> MyAST.AST T.Type -> LLVM String
createAssign id' exp = do
  let id = convertId' id'
  let ty = toType $ MyAST.tag exp
  e'  <- createExpression exp
  op  <- checkVar id ty
  res <- store ty op e'
  return id


createMultyAssign :: MyAST.AST T.Type -> String -> LLVM ()
createMultyAssign id aux = do
  let ty = toType $ MyAST.tag id
  id'   <- getStoreDir id
  e'    <- load aux ty
  store ty id' e'
  return ()


createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction MyAST.EmptyAST {} = return ()
createInstruction MyAST.Id       {} = return ()
createInstruction MyAST.Skip     {} = return ()


createInstruction (MyAST.Abort pos _) = do
  createTagAbort pos
  return ()


createInstruction (MyAST.GuardAction _ assert action ty) = do
  createState "" assert
  createInstruction action


createInstruction (MyAST.LAssign [id] [exp] _ _) = do
  let ty = toType $ MyAST.tag id
  e'  <- createExpression exp
  id' <- getStoreDir id
  store ty id' e'
  return ()


createInstruction (MyAST.LAssign ids exps _ _) = do
  list <- zipWithM createAssign ids exps
  zipWithM_ createMultyAssign ids list


createInstruction (MyAST.Write True exp _ t) = do
  let ty  = MyAST.tag exp
  let ty' = voidType
  e'     <- createExpression exp

  case ty of
    T.GInt      -> procedureCall ty' writeLnInt    [e']
    T.GFloat    -> procedureCall ty' writeLnDouble [e']
    T.GBoolean  -> procedureCall ty' writeLnBool   [e']
    T.GChar     -> procedureCall ty' writeLnChar   [e']
    T.GEmpty    -> procedureCall ty' writeLnString [e']
  return ()


createInstruction (MyAST.Write False exp _ t) = do
  let ty  = MyAST.tag exp
  let ty' = voidType
  e'     <- createExpression exp

  case ty of
    T.GInt      -> procedureCall ty' writeInt    [e']
    T.GFloat    -> procedureCall ty' writeDouble [e']
    T.GBoolean  -> procedureCall ty' writeBool   [e']
    T.GChar     -> procedureCall ty' writeChar   [e']
    T.GEmpty    -> procedureCall ty' writeString [e']
  return ()


createInstruction (MyAST.Bposk _ st decs accs _) = do
  mapM_ accToAlposa decs
  mapM_ createInstruction accs
  return ()


createInstruction (MyAST.Cond guards pos _) = do
  final <- newLabel
  abort <- newLabel
  genGuards guards abort final

  setLabel abort $ branch final
  createTagIf final pos
  return ()


createInstruction (MyAST.Rept guards inv bound _ _) = do
  final   <- newLabel
  initial <- newLabel

  name <- getCount
  let boundName = show name
  op' <- alposa Nothing intType boundName
  store intType op' $ constantInt maxInteger
  addVarOperand (show boundName) op'

  setLabel initial $ branch initial
  createState "" inv
  createState boundName bound
  genGuards guards final initial
  setLabel final $ branch initial
  return ()


createInstruction (MyAST.ProcCallCont pname st _ args c _) = do
  let dic   = getMap . getCurrent . procTable $ c
  let nargp = procArgs c
  exp <- createArguments dic nargp args
  procedureCall voidType (TE.unpack pname) exp
  return ()


createInstruction (MyAST.Ran id _ _ t) = do
  vars <- use varsLoc
  let (ty, i) = (toType t, fromJust $ DM.lookup (TE.unpack id) vars)
  let df      = Right $ definedFunction floatType (Name randomInt)
  val <- caller ty df []
  store ty i val
  return ()


createArguments :: DM.Map TE.Text (Contents SymbolTable)
                -> [TE.Text] -> [MyAST.AST T.Type] -> LLVM [Operand]
createArguments _ [] [] = return []
createArguments dicnp (nargp:nargps) (arg:args) = do
  lr <- createArguments dicnp nargps args
  let argt = argTypeArg $ fromJust $ DM.lookup nargp dicnp

  case argt of
    T.In -> do
      arg' <- createExpression arg
      return $ arg':lr
    _    -> do
      dicn <- use varsLoc
      return $ fromJust (DM.lookup (TE.unpack $
        fromJust $ MyAST.astToId arg) dicn) : lr


genGuards :: [MyAST.AST T.Type] -> Name -> Name -> LLVM ()
genGuards [guard] none one = genGuard guard none

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

myFromJust (Just x) = x


createExpression :: MyAST.AST T.Type -> LLVM Operand
createExpression (MyAST.Id _ id t) = do
  var <- use varsLoc
  let (n, ty) = (TE.unpack id, toType t)
  let check   = DM.lookup n var

  case check of
    Just add -> case t of
      T.GArray _ _ -> return add
      _            -> load n ty
    Nothing -> return $ posal ty (Name n)


createExpression (MyAST.Cond lguards pos rtype) = do
  final  <- newLabel
  abort  <- newLabel
  lnames <- genExpGuards lguards abort final
  let rtype' = toType rtype
  setLabel abort $ branch final
  createTagIf final pos
  condName .= final
  addUnNamedInstruction rtype' $ Phi rtype' lnames []


createExpression (MyAST.ArrCall _ id' accs t) = do
  accs' <- mapM createExpression accs
  map   <- use varsLoc
  let (t', i, id) = (toType t, myFromJust $ DM.lookup id map, TE.unpack id')
  accs'' <- opsToArrayIndex id accs'
  add    <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
  addUnNamedInstruction t' $ Load False add Nothing 0 []


createExpression (MyAST.Int _ n _) =
  return $ constantInt n


createExpression (MyAST.Float _ n _) =
  return $ constantFloat n


createExpression (MyAST.Bool _ True  _) =
  return $ constantBool 1


createExpression (MyAST.Bool _ False _) =
  return $ constantBool 0


createExpression (MyAST.Char _ n _) =
  return $ constantChar n


createExpression (MyAST.String _ msg _) =
  addStringOpe msg


createExpression (MyAST.Conversion tType _ exp t) = do
  let t' = MyAST.tag exp
  exp' <- createExpression exp

  if t' == T.GChar && tType == MyAST.ToInt then do
    op <- intToDouble exp'
    doubleToInt op
  else
    addUnNamedInstruction (toType t) $ irConversion tType t' exp'


createExpression (MyAST.Arithmetic op pos lexp rexp ty) = do
  lexp' <- createExpression lexp
  rexp' <- createExpression rexp

  case op of
    MyAST.Exp -> do a   <- intToDouble lexp'
                    b   <- intToDouble rexp'
                    val <- addUnNamedInstruction floatType $ irArithmetic MyAST.Exp T.GFloat a b
                    doubleToInt val
    MyAST.Max -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'
    MyAST.Min -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'
    MyAST.Div -> checkDivZero  op pos lexp' rexp' ty
    MyAST.Mod -> checkDivZero  op pos lexp' rexp' ty
    _         -> case ty of
      T.GInt   -> checkOverflow op pos lexp' rexp' ty
      T.GFloat -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'


createExpression (MyAST.Boolean op _ lexp rexp t) = do

  lexp' <- createExpression lexp
  rexp' <- createExpression rexp

  case op of
    MyAST.Implies -> do notA <- addUnNamedInstruction boolType $ _not lexp'
                        addUnNamedInstruction boolType $ _or notA rexp'

    MyAST.Conse   -> do notA <- addUnNamedInstruction boolType $ _not rexp'
                        addUnNamedInstruction boolType $ _or notA lexp'

    _             -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'


createExpression (MyAST.Relational op _ lexp rexp t) = do
  lexp' <- createExpression lexp
  rexp' <- createExpression rexp
  let t' = MyAST.tag lexp
  addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


createExpression (MyAST.Unary MyAST.Abs _ exp T.GInt) = do
  exp'  <- createExpression exp
  x     <- intToDouble exp'
  val   <- addUnNamedInstruction intType $ irUnary MyAST.Abs T.GFloat x
  doubleToInt val


createExpression (MyAST.Unary MyAST.Sqrt _ exp t) = do
  let ty = MyAST.tag exp
  let df = Right $ definedFunction floatType (Name sqrtString)
  exp'  <- createExpression exp

  case ty of
    T.GFloat ->
      addUnNamedInstruction floatType $ irUnary MyAST.Sqrt ty exp'
    T.GInt   -> do
      x <- intToDouble exp'
      addUnNamedInstruction floatType $ irUnary MyAST.Sqrt T.GFloat x


createExpression (MyAST.Unary op _ exp t) = do
  exp' <- createExpression exp
  addUnNamedInstruction (toType t) $ irUnary op t exp'


createExpression (MyAST.FCallExp fname st _ args t) = do
  exp     <- mapM createExpression args
  let ty   =  toType t
  let exp' = map (\i -> (i,[])) exp
  let op   = definedFunction ty (Name $ TE.unpack fname)
  caller ty (Right op) exp'


createExpression (MyAST.QuantRan opQ varQ pos rangeExp termExp t) = do
  let name  = TE.unpack varQ
  let tyExp = MyAST.tag termExp

  case opQ of
    MyAST.ForAll -> do check <- mapM (createQuant True  opQ name pos termExp) rangeExp
                       joinRange opQ check pos tyExp

    MyAST.Exists -> do check <- mapM (createQuant True  opQ name pos termExp) rangeExp
                       joinRange opQ check pos tyExp

    _            -> do check <- mapM (createQuant False opQ name pos termExp) rangeExp
                       joinRange opQ check pos tyExp


createExpression (MyAST.QuantRanUn opQ varQ pos rangeExp termExp t) = do
  let name  = TE.unpack varQ
  let tyExp = MyAST.tag termExp

  ranges  <- doRange opQ rangeExp pos
  rangesF <- makeRanges ranges

  case opQ of
    MyAST.ForAll -> do check <- mapM (createQuant' True  opQ name pos termExp) rangesF
                       joinRange opQ check pos tyExp

    MyAST.Exists -> do check <- mapM (createQuant' True  opQ name pos termExp) rangesF
                       joinRange opQ check pos tyExp

    _            -> do check <- mapM (createQuant' False opQ name pos termExp) rangesF
                       joinRange opQ check pos tyExp


joinRange :: MyAST.OpQuant -> [Operand] -> SourcePos -> T.Type -> LLVM Operand
joinRange MyAST.Summation res pos T.GInt   =
  foldM (\acc i -> checkOverflow MyAST.Sum pos acc i T.GInt) (head res) (tail res)

joinRange MyAST.Summation res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _addF acc i) (head res) (tail res)

joinRange MyAST.Product   res pos T.GInt   =
  foldM (\acc i -> checkOverflow MyAST.Mul pos acc i T.GInt) (head res) (tail res)

joinRange MyAST.Product   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _mulF acc i) (head res) (tail res)

joinRange MyAST.Maximum   res pos T.GInt   =
  foldM (\acc i -> addUnNamedInstruction intType $ _max acc i) (head res) (tail res)

joinRange MyAST.Maximum   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _maxF acc i) (head res) (tail res)

joinRange MyAST.Minimum   res pos T.GInt   =
  foldM (\acc i -> addUnNamedInstruction intType $ _min acc i) (tail res)

joinRange MyAST.Minimum   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _minF acc i) (head res) (tail res)

joinRange opQ res pos _ = do
  warAbort <- newLabel
  next     <- newLabel
  check    <- foldM (\acc i -> addUnNamedInstruction intType $ _and acc i) (head res) (tail res)
  setLabel warAbort $ condBranch check next warAbort

  case opQ of
    MyAST.ForAll -> createTagForAll next pos
    MyAST.Exists -> createTagExists next pos

  return check


createQuant :: Bool -> MyAST.OpQuant -> String -> SourcePos ->
                   MyAST.AST T.Type -> Range Integer -> LLVM Operand
createQuant True opQ var pos exp (SpanRange a b) = do
  let ini = constantInt a
  let fin = constantInt b
  op     <- alposa Nothing intType var
  store intType op ini
  addVarOperand var op

  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  name <- getCount
  let varBool = show name
  op' <- alposa Nothing boolType varBool
  store boolType op' $ constantBool 1
  addVarOperand varBool op'
  setLabel initial $ branch initial

  varQ   <- load var intType
  check' <- addUnNamedInstruction boolType $ _lequal varQ fin

  checkBool <- load varBool boolType
  tag       <- addUnNamedInstruction boolType $ _and check' checkBool
  setLabel code $ condBranch tag code final

  e'   <- createExpression exp
  sum' <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
  store intType  op  sum'

  case opQ of
    MyAST.ForAll -> store boolType op' e'
    MyAST.Exists -> do bool <- addUnNamedInstruction boolType $ _not e'
                       store boolType op' bool

  setLabel final $ branch initial

  case opQ of
    MyAST.ForAll -> load varBool boolType
    MyAST.Exists -> do checkBool' <- load varBool boolType
                       addUnNamedInstruction boolType $ _not checkBool'


createQuant False opQ var pos exp (SpanRange a b) = do
  let ini = constantInt a
  let fin = constantInt b
  op <- alposa Nothing intType var
  store intType op ini
  addVarOperand var op

  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  let varQuant = "Quant_" ++ var
  let tyExp = MyAST.tag exp

  case tyExp of
    T.GInt -> do
      op' <- alposa Nothing intType varQuant
      case opQ of
        MyAST.Summation -> store intType op' $ constantInt 0
        MyAST.Product   -> store intType op' $ constantInt 1
        MyAST.Maximum   -> store intType op' $ constantInt minInteger
        MyAST.Minimum   -> store intType op' $ constantInt maxInteger

      addVarOperand varQuant op'

      setLabel initial $ branch initial
      varQ <- load var intType
      tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
      res  <- load varQuant intType

      setLabel code $ condBranch tag code final

      e'   <- createExpression exp

      sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
      store intType op  sum

      case opQ of
        MyAST.Summation -> do
          check <- checkOverflow MyAST.Sum pos res e' T.GInt
          store intType op' check
        MyAST.Product   -> do
          check <- checkOverflow MyAST.Mul pos res e' T.GInt
          store intType op' check
        MyAST.Maximum   -> do
          check <- addUnNamedInstruction intType $ _max res e'
          store intType op' check
        MyAST.Minimum   -> do
          check <- addUnNamedInstruction intType $ _min res e'
          store intType op' check

      setLabel final $ branch initial
      return res

    T.GFloat -> do
      op' <- alposa Nothing floatType varQuant

      case opQ of
        MyAST.Summation -> store floatType op' $ constantFloat 0.0
        MyAST.Product   -> store floatType op' $ constantFloat 1.0
        MyAST.Maximum   -> store floatType op' $ constantFloat minDouble
        MyAST.Minimum   -> store floatType op' $ constantFloat maxDouble

      addVarOperand varQuant op'

      setLabel initial $ branch initial
      varQ <- load var floatType
      tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
      res  <- load varQuant floatType

      setLabel code $ condBranch tag code final

      e'   <- createExpression exp

      sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
      store floatType op  sum

      case opQ of
        MyAST.Summation -> do check <- addUnNamedInstruction floatType $ _addF res e'
                              store floatType op' check
        MyAST.Product   -> do check <- addUnNamedInstruction floatType $ _mulF res e'
                              store floatType op' check
        MyAST.Maximum   -> do check <- addUnNamedInstruction floatType $ _maxF res e'
                              store floatType op' check
        MyAST.Minimum   -> do check <- addUnNamedInstruction floatType $ _minF res e'
                              store floatType op' check

      setLabel final $ branch initial
      return res


data RangeCodegen
  = SetOp
    { getOp   :: MyAST.OpSet
    , getLexp :: RangeCodegen
    , getRexp :: RangeCodegen
    }
  | RangeOp
    { getLeft  :: Operand
    , getRight :: Operand
    }
  deriving (Eq)


doRange opQ (MyAST.SetRange op lexp rexp) pos = do
  l <- doRange opQ lexp pos
  r <- doRange opQ rexp pos

  case op of
    MyAST.Intersec -> intersecRange opQ l r pos
    MyAST.Union    -> return $ SetOp MyAST.Union l r


doRange _ (MyAST.TupleRange lexp rexp) _ = do
  l <- createExpression lexp
  r <- createExpression rexp
  return $ RangeOp l r


intersecRange opQ (RangeOp l1 r1) (RangeOp l2 r2) pos = do
  l <- addUnNamedInstruction intType $ _max l1 l2
  r <- addUnNamedInstruction intType $ _min r1 r2

  check <- addUnNamedInstruction boolType $ _less r l

  error <- newLabel
  final <- newLabel

  setLabel error $ condBranch check error final

  case opQ of
    MyAST.Maximum -> createTagRangeAbort final pos
    MyAST.Minimum -> createTagRangeAbort final pos
    _             -> createTagRange      final pos

  return $ RangeOp l r


makeRanges (SetOp _ lexp rexp) = do
  l <- makeRanges lexp
  r <- makeRanges rexp

  return $ l ++ r

makeRanges res@(RangeOp _ _) = return [res]


createQuant' :: Bool -> MyAST.OpQuant -> String -> SourcePos
             -> MyAST.AST T.Type -> RangeCodegen -> LLVM Operand
createQuant' True opQ var pos exp (RangeOp a b) = do
  let ini = a
  let fin = b

  op <- alposa Nothing intType var
  store intType op ini
  addVarOperand var op

  empty   <- newLabel
  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  name <- getCount
  let varBool = show name
  op' <- alposa Nothing boolType varBool
  store boolType op' $ constantBool 1
  addVarOperand varBool op'

  -- Check Empty Range
  checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
  setLabel empty $ condBranch checkRange initial empty


  setLabel initial $ branch final
  varQ   <- load var intType
  check' <- addUnNamedInstruction boolType $ _lequal varQ fin

  checkBool <- load varBool boolType
  tag       <- addUnNamedInstruction boolType $ _and check' checkBool
  setLabel code $ condBranch tag code final

  e'   <- createExpression exp
  sum' <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
  store intType  op  sum'

  case opQ of
    MyAST.ForAll -> store boolType op' e'
    MyAST.Exists -> do bool <- addUnNamedInstruction boolType $ _not e'
                       store boolType op' bool


  setLabel final $ branch initial

  case opQ of
    MyAST.ForAll -> load varBool boolType
    MyAST.Exists -> do
      checkBool' <- load varBool boolType
      addUnNamedInstruction boolType $ _not checkBool'


createQuant' False opQ var pos exp (RangeOp a b) = do
  let ini = a
  let fin = b
  op <- alposa Nothing intType var
  store intType op ini
  addVarOperand var op

  empty   <- newLabel
  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  let varQuant = "Quant_" ++ var
  let tyExp = MyAST.tag exp

  case tyExp of
    T.GInt   -> do
      op' <- alposa Nothing intType varQuant

      case opQ of
        MyAST.Summation -> store intType op' $ constantInt 0
        MyAST.Product   -> store intType op' $ constantInt 1
        MyAST.Maximum   -> store intType op' $ constantInt minInteger
        MyAST.Minimum   -> store intType op' $ constantInt maxInteger

      addVarOperand varQuant op'

      -- Check Empty Range
      checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
      setLabel empty $ condBranch checkRange initial empty


      setLabel initial $ branch final
      varQ <- load var intType
      tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
      res  <- load varQuant intType

      setLabel code $ condBranch tag code final

      e'   <- createExpression exp

      sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
      store intType op sum

      case opQ of
        MyAST.Summation -> do
          check <- checkOverflow MyAST.Sum pos res e' T.GInt
          store intType op' check
        MyAST.Product   -> do
          check <- checkOverflow MyAST.Mul pos res e' T.GInt
          store intType op' check
        MyAST.Maximum   -> do
          check <- addUnNamedInstruction intType $ _max res e'
          store intType op' check
        MyAST.Minimum   -> do
          check <- addUnNamedInstruction intType $ _min res e'
          store intType op' check

      setLabel final $ branch initial

      load varQuant intType

    T.GFloat -> do
      op' <- alposa Nothing floatType varQuant

      case opQ of
        MyAST.Summation -> store floatType op' $ constantFloat 0.0
        MyAST.Product   -> store floatType op' $ constantFloat 1.0
        MyAST.Maximum   -> store floatType op' $ constantFloat minDouble
        MyAST.Minimum   -> store floatType op' $ constantFloat maxDouble


      addVarOperand varQuant op'

      -- Revisar Rango Vacio
      checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
      setLabel empty $ condBranch checkRange initial empty


      setLabel initial $ branch final
      varQ <- load var floatType
      tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
      res  <- load varQuant floatType

      setLabel code $ condBranch tag code final

      e'   <- createExpression exp

      sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
      store floatType op  sum

      case opQ of
        MyAST.Summation -> do
          check <- addUnNamedInstruction floatType $ _addF res e'
          store floatType op' check
        MyAST.Product   -> do
          check <- addUnNamedInstruction floatType $ _mulF res e'
          store floatType op' check
        MyAST.Maximum   -> do
          check <- addUnNamedInstruction floatType $ _maxF res e'
          store floatType op' check
        MyAST.Minimum   -> do
          check <- addUnNamedInstruction floatType $ _minF res e'
          store floatType op' check

      setLabel final $ branch initial

      load varQuant floatType


checkDivZero :: MyAST.OpNum -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
checkDivZero op pos lexp' rexp' ty = do
  next  <- newLabel
  abort <- newLabel
  condName .= next

  case ty of
    T.GInt   -> do
      let zero = constantInt 0
      check <- addUnNamedInstruction intType $ ICmp IL.EQ rexp' zero []
      setLabel abort $ condBranch check abort next
      createTagZero next pos
      addUnNamedInstruction intType $ irArithmetic op ty lexp' rexp'

    T.GFloat -> do
      let zero = constantFloat 0.0
      check <- addUnNamedInstruction floatType $ FCmp FL.OEQ rexp' zero []
      setLabel abort $ condBranch check abort next
      createTagZero next pos
      addUnNamedInstruction floatType $ irArithmetic op ty lexp' rexp'


checkOverflow :: MyAST.OpNum -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
checkOverflow op pos lexp rexp ty = do
  overAbort <- newLabel
  next      <- newLabel
  res       <- addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp rexp
  check     <- extracValue res 1

  setLabel overAbort $ condBranch check overAbort next
  createTagOverflow next pos

  condName .= next
  extracValue res 0


genExpGuards :: [MyAST.AST T.Type] -> Name -> Name -> LLVM [(Operand, Name)]
genExpGuards [guard] none one  = do
  r <- genExpGuard guard none
  return [r]

genExpGuards (guard:xs) none one = do
  next <- newLabel
  r    <- genExpGuard guard next
  setLabel next $ branch one
  rl   <- genExpGuards xs none one
  return $ r:rl


createGuardExp :: MyAST.AST T.Type -> Name -> LLVM (Operand, Name)
--createGuardExp (MyAST.Cond lguards _ rtype) _ = do
--    final  <- newLabel
--    none   <- newLabel
--    lnames <- genExpGuards lguards none final
--    let rtype' = toType rtype
--    setLabel none $ branch final
--    setLabel final $ Do $ Unreachable []
--    n <- addUnNamedInstruction rtype' $ Phi rtype' lnames []
--    return (n, final)

createGuardExp acc code = do
  exp   <- createExpression acc
  label <- use condName
  return (exp, label)


genExpGuard :: MyAST.AST T.Type -> Name -> LLVM (Operand, Name)
genExpGuard (MyAST.GuardExp guard acc _ _) next = do
  tag  <- createExpression guard
  code <- newLabel
  condName .= code
  setLabel code $ condBranch tag code next
  createGuardExp acc code


createBasicBposks :: [MyAST.AST T.Type] -> Named Terminator -> LLVM ()
createBasicBposks accs m800 = genIntructions accs
  where
    genIntructions (acc:xs) = do
      createInstruction acc
      genIntructions xs
    genIntructions [] =
      addBasicBposk m800


intToDouble :: Operand -> LLVM Operand
intToDouble x = addUnNamedInstruction floatType $ _toFloat x


doubleToInt :: Operand -> LLVM Operand
doubleToInt x = addUnNamedInstruction intType $ _toInt x


irArithmetic :: MyAST.OpNum -> T.Type -> Operand -> Operand -> Instruction
--irArithmetic MyAST.Sum T.GInt   a b = _add  a b
--irArithmetic MyAST.Sub T.GInt   a b = Sub False False a b []
--irArithmetic MyAST.Mul T.GInt   a b = _mul a b
irArithmetic MyAST.Sum T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intAdd))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Sub T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intSub))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Mul T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intMul))) [(a, []),(b, [])] [] []
irArithmetic MyAST.Sum T.GFloat a b = _addF   a b
irArithmetic MyAST.Mul T.GFloat a b = _mulF   a b
irArithmetic MyAST.Sub T.GFloat a b = FSub NoFastMathFlags a b []
irArithmetic MyAST.Div T.GInt   a b = SDiv True a b []
irArithmetic MyAST.Div T.GFloat a b = FDiv NoFastMathFlags a b []
irArithmetic MyAST.Mod T.GInt   a b = URem a b []
irArithmetic MyAST.Mod T.GFloat a b = FRem NoFastMathFlags a b []
irArithmetic MyAST.Exp T.GFloat a b = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name powString)))    [(a, []),(b, [])] [] []
irArithmetic MyAST.Min T.GFloat a b = _minF a b
irArithmetic MyAST.Max T.GFloat a b = _maxF a b
irArithmetic MyAST.Max T.GInt   a b = _max  a b
irArithmetic MyAST.Min T.GInt   a b = _min  a b


irBoolean :: MyAST.OpBool -> Operand -> Operand -> Instruction
irBoolean MyAST.Con a b = _and a b
irBoolean MyAST.Dis a b = _or  a b


irRelational :: MyAST.OpRel -> T.Type -> Operand -> Operand -> Instruction
irRelational MyAST.Equ     T.GFloat a b = FCmp FL.OEQ a b []
irRelational MyAST.Less    T.GFloat a b = FCmp FL.OLT a b []
irRelational MyAST.Greater T.GFloat a b = FCmp FL.OGT a b []
irRelational MyAST.LEqual  T.GFloat a b = FCmp FL.OLE a b []
irRelational MyAST.GEqual  T.GFloat a b = FCmp FL.OGE a b []
irRelational MyAST.Ine     T.GFloat a b = FCmp FL.ONE a b []


irRelational MyAST.Equ     T.GInt   a b = ICmp IL.EQ  a b []
irRelational MyAST.Less    T.GInt   a b = _less   a b
irRelational MyAST.Greater T.GInt   a b = ICmp IL.SGT a b []
irRelational MyAST.LEqual  T.GInt   a b = _lequal a b
irRelational MyAST.GEqual  T.GInt   a b = ICmp IL.SGE a b []
irRelational MyAST.Ine     T.GInt   a b = ICmp IL.NE  a b []


irConversion :: MyAST.Conv -> T.Type -> Operand -> Instruction
irConversion MyAST.ToInt    T.GFloat a = _toInt   a
irConversion MyAST.ToInt    T.GBoolean
          (ConstantOperand (C.Int 1 0)) = _toInt $ constantInt 0
irConversion MyAST.ToInt    T.GBoolean
          (ConstantOperand (C.Int 1 1)) = _toInt $ constantInt 1
irConversion MyAST.ToDouble T.GInt   a = _toFloat a
irConversion MyAST.ToDouble T.GChar  a = _toFloat a
irConversion MyAST.ToChar   T.GInt   a = Trunc  a charType  []
irConversion MyAST.ToChar   T.GFloat a = FPToSI a charType  []


irUnary :: MyAST.OpUn -> T.Type -> Operand -> Instruction
irUnary MyAST.Minus T.GInt   a = Sub False False      (constantInt 0) a []
irUnary MyAST.Minus T.GFloat a = FSub NoFastMathFlags (constantFloat 0) a []
irUnary MyAST.Abs   T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name fabsString))) [(a, [])] [] []
irUnary MyAST.Sqrt  T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name sqrtString))) [(a, [])] [] []
irUnary MyAST.Not   T.GBoolean  a = _not a


_and    a b = And a b []
_not    a   = Xor a (constantBool 1) []
_or     a b = Or  a b []
_less   a b = ICmp IL.SLT a b []
_lequal a b = ICmp IL.SLE a b []
_add    a b = Add False False a b []
_addF   a b = FAdd NoFastMathFlags a b []
_mul    a b = Mul False False a b []
_mulF   a b = FMul NoFastMathFlags a b []
_min    a b = Call Nothing CC.C [] (Right ( definedFunction intType
                         (Name minnumString)))  [(a, []),(b, [])] [] []
_minF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
                         (Name minnumFstring))) [(a, []),(b, [])] [] []
_max    a b = Call Nothing CC.C [] (Right ( definedFunction intType
                         (Name maxnumString)))  [(a, []),(b, [])] [] []
_maxF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
                         (Name maxnumFtring)))  [(a, []),(b, [])] [] []
_toFloat a = SIToFP a floatType []
_toInt   a = FPToSI a intType   []
