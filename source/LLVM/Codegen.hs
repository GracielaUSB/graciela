module LLVM.Codegen where

--------------------------------------------------------------------------------
import           Aborts
import           AST                                     (AST(..))
import qualified AST                                     as AST
import           Contents
import           Limits
import           LLVM.CodegenState
import           LLVM.Expression
import           LLVM.Instruction
import           SymbolTable
import qualified Type                                    as T
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.State
import           Data.Foldable                           (toList)
import qualified Data.Map                                as DM
import           Data.Maybe
import           Data.Range.Range                        as RA
import qualified Data.Text                               as TE
import           Data.Word
import           LLVM.General.AST.Name                  (Name(..))
import           LLVM.General.AST.Instruction           (Instruction(..), Named(..),
                                                         Terminator(..), FastMathFlags(..))
import           LLVM.General.AST.Operand               (Operand(..), CallableOperand)
import           LLVM.General.AST                       (Definition(..), Module(..))
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Type                   
import           System.Info                             (arch, os)
import           System.Process                          (callCommand)
import           Text.Megaparsec.Pos                     (SourcePos)
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


astToLLVM :: [String] -> AST T.Type -> String -> Module
astToLLVM files (AST.Program name _ defs accs _) version =
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
  store (ptr charType) (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) op


closeFile :: String -> LLVM Operand
closeFile file = do
  let file' = convertFile file
  -- Cargamos la variable gobal perteneciente al archivo.
  let load' = Load False (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) Nothing 0 []
  op <- addUnNamedInstruction voidType load'
  caller voidType (Right $ definedFunction voidType (Name closeFileStr)) [(op, [])]


createLLVM :: [String] -> [AST T.Type] -> AST T.Type -> LLVM ()
createLLVM files defs accs = do
  createPreDef files
  mapM_ createDef defs
  m800 <- retVoid
  mapM_ openFile files
  createInstruction accs
  mapM_ closeFile files
  addBasicBlock m800
  addDefinition "main" ([],False) voidType
  return ()


convertId :: String -> String
convertId name = '_':name

convertId' :: AST T.Type -> String
convertId' (AST.ArrCall _ id _ _) = "__" ++ TE.unpack id
convertId' (AST.Id        _ id _) = "__" ++ TE.unpack id

convertId'' :: AST T.Type -> String
convertId'' (AST.ArrCall _ id _ _) = "___" ++ TE.unpack id
convertId'' (AST.Id        _ id _) = "___" ++ TE.unpack id


addArgOperand :: [(String, Contents SymbolTable)] -> LLVM ()
addArgOperand [] = return ()

addArgOperand ((id',c):xs) = do
  let t    = toType $ argType c
  let tp   = argTypeArg c
  let id   = convertId id'
  let e'   = local t (Name id')

  void $ case tp of
    T.InOut -> do exp <- addUnNamedInstruction t $ Load False e' Nothing 0 []
                  op  <- alloca Nothing t id
                  store t op exp
                  addVarOperand id' op

    T.In    -> do op <- alloca Nothing t id
                  store t op e'
                  addVarOperand id' op

    T.Out   -> do op <- alloca Nothing t id
                  addVarOperand id' op
                  initialize id $ argType c

    T.Ref   -> addVarOperand id' e'

  addArgOperand xs


retVarOperand :: [(String, Contents SymbolTable)] -> LLVM ()
retVarOperand [] = pure ()

retVarOperand ((id', c):xs) = do

  let t   = toType $ getContentType c
  let exp = local t (Name id')
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

createState :: String -> AST T.Type -> LLVM ()
createState name (AST.States cond pos exp _) = do
  e' <- createExpression exp
  next     <- newLabel
  warAbort <- newLabel

  case cond of
    AST.Pre        -> do let checkPre = "_resPre" ++ name
                         op <- alloca Nothing boolType checkPre
                         store boolType op e'
                         addVarOperand checkPre op
                         setLabel warAbort $ condBranch e' next warAbort
                         createTagPre next pos

    AST.Post       -> do let checkPre = "_resPre" ++ name
                         op     <- load checkPre boolType
                         a      <- addUnNamedInstruction boolType $ _not op
                         check  <- addUnNamedInstruction boolType $ _or a e'
                         setLabel warAbort $ condBranch check next warAbort
                         createTagPost next pos

    AST.Assertion -> do setLabel warAbort $ condBranch e' next warAbort
                        createTagAsert next pos

    AST.Invariant -> do setLabel warAbort $ condBranch e' next warAbort
                        createTagInv next pos

    AST.Bound     -> do checkZero <- newLabel
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
  let e'   = local (toType t) (Name id)
  addVarOperand id e'
  return ()
addFuncParam _ = return ()


createDef :: AST T.Type -> LLVM ()
createDef (AST.DefProc name st accs pre post bound decs params _) = do
  let name' = TE.unpack name
  let args  = map (\(id, _) -> (TE.unpack id, fromJust $ checkSymbol id st)) params
  let args' = ([Parameter t (Name id) [] | (id, t) <- convertParams args], False)
  retTy <- retVoid
  addArgOperand args
  mapM_ accToAlloca decs
  createState name' pre
  createInstruction accs
  retVarOperand $ reverse args
  createState name' post
  addBasicBlock retTy
  addDefinition name' args' voidType


createDef (AST.DefFun fname st _ exp reType bound params _) = do
  let args' = ([Parameter t (Name id) [] | (id, t) <- convertFuncParams params], False)
  mapM_ addFuncParam params
  exp'  <- createExpression exp
  retTy <- retType exp'
  addBasicBlock retTy
  addDefinition (TE.unpack fname) args' (toType reType)


accToAlloca :: AST T.Type -> LLVM ()
accToAlloca acc@(AST.Id _ id' t) = do
  let id = TE.unpack id'
  dim <- typeToOperand id t
  let t' = toType t
  alloca dim t' id

  case t of
    T.GArray d ty -> createInstruction acc
    _             -> do
      initialize id t
      createInstruction acc

accToAlloca acc@(AST.LAssign lids _ _ _) = do
  mapM_ idToAlloca lids
  createInstruction acc

accToAlloca (AST.Read _ Nothing types vars _) = do
  res <- mapM callRead types
  ads <- mapM (getVarOperand . TE.unpack . fst) vars
  mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
  return ()

accToAlloca (AST.Read _ (Just arch) types vars _) = do
  res <- mapM (callReadFile arch) types
  ads <- mapM (getVarOperand . TE.unpack . fst) vars
  mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
  return ()


callReadFile :: String -> T.Type -> LLVM Operand
callReadFile arch T.GInt = do
  let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller intType (Right $ definedFunction intType (Name readFileInt)) [(op, [])]

callReadFile arch T.GFloat = do
  let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller doubleType (Right $ definedFunction doubleType (Name readFileDouble)) [(op, [])]

callReadFile arch T.GChar = do
  let i = ConstantOperand $ global (ptr pointerType) (Name (convertFile arch))
  op <- addUnNamedInstruction (ptr pointerType) $ Load False i Nothing 0 []
  caller charType (Right $ definedFunction charType (Name readFileChar)) [(op, [])]


callRead :: T.Type -> LLVM Operand
callRead T.GInt   =
  caller intType   (Right $ definedFunction intType   (Name readIntStd))    []

callRead T.GChar  =
  caller charType  (Right $ definedFunction charType  (Name readCharStd))   []

callRead T.GFloat =
  caller floatType (Right $ definedFunction floatType (Name readDoubleStd)) []


idToAlloca :: AST T.Type -> LLVM ()
idToAlloca (AST.Id _ id t) = do
  let id' = TE.unpack id
  dim <- typeToOperand id' t
  alloca dim (toType t) id'
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


getStoreDir :: AST T.Type -> LLVM Operand
getStoreDir (AST.Id _ name _) = getVarOperand (TE.unpack name)
getStoreDir (AST.ArrCall _ name exps _) = do
  ac' <- mapM createExpression exps
  map <- use varsLoc
  let (i, id) = (fromJust $ DM.lookup id map, TE.unpack name)
  ac'' <- opsToArrayIndex id ac'
  addUnNamedInstruction intType $ GetElementPtr True i [ac''] []


createAssign :: AST T.Type -> AST T.Type -> LLVM String
createAssign id' exp = do
  let id = convertId' id'
  let ty = toType $ AST.tag exp
  e'  <- createExpression exp
  op  <- checkVar id ty
  res <- store ty op e'
  return id


createMultyAssign :: AST T.Type -> String -> LLVM ()
createMultyAssign id aux = do
  let ty = toType $ AST.tag id
  id'   <- getStoreDir id
  e'    <- load aux ty
  store ty id' e'
  return ()


createArguments :: DM.Map TE.Text (Contents SymbolTable)
                -> [TE.Text] -> [AST T.Type] -> LLVM [Operand]
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
        fromJust $ astToId arg) dicn) : lr


genGuards :: [AST T.Type] -> Name -> Name -> LLVM ()
genGuards [guard] none one = genGuard guard none

genGuards (guard:xs) none one = do
  next <- newLabel
  genGuard guard next
  setLabel next $ branch one
  genGuards xs none one


genGuard :: AST T.Type -> Name -> LLVM ()
genGuard (AST.Guard guard acc _ _) next = do
  tag  <- createExpression guard
  code <- newLabel
  setLabel code $ condBranch tag code next
  createInstruction acc

myFromJust (Just x) = x





joinRange :: AST.QuantOp -> [Operand] -> SourcePos -> T.Type -> LLVM Operand
joinRange AST.Summation res pos T.GInt   =
  foldM (\acc i -> checkOverflow AST.Sum pos acc i T.GInt) (head res) (tail res)

joinRange AST.Summation res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _addF acc i) (head res) (tail res)

joinRange AST.Product   res pos T.GInt   =
  foldM (\acc i -> checkOverflow AST.Mul pos acc i T.GInt) (head res) (tail res)

joinRange AST.Product   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _mulF acc i) (head res) (tail res)

joinRange AST.Maximum   res pos T.GInt   =
  foldM (\acc i -> addUnNamedInstruction intType $ _max acc i) (head res) (tail res)

joinRange AST.Maximum   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _maxF acc i) (head res) (tail res)

joinRange AST.Minimum   res pos T.GInt   =
  foldM (\acc i -> addUnNamedInstruction intType $ _min acc i) (tail res)

joinRange AST.Minimum   res pos T.GFloat =
  foldM (\acc i -> addUnNamedInstruction intType $ _minF acc i) (head res) (tail res)

joinRange opQ res pos _ = do
  warAbort <- newLabel
  next     <- newLabel
  check    <- foldM (\acc i -> addUnNamedInstruction intType $ _and acc i) (head res) (tail res)
  setLabel warAbort $ condBranch check next warAbort

  case opQ of
    AST.ForAll -> createTagForAll next pos
    AST.Exists -> createTagExists next pos

  return check


createQuant :: Bool -> AST.QuantOp -> String -> SourcePos ->
                   AST T.Type -> Range Integer -> LLVM Operand
createQuant True opQ var pos exp (SpanRange a b) = do
  let ini = constantInt a
  let fin = constantInt b
  op     <- alloca Nothing intType var
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
  check' <- addUnNamedInstruction boolType $ _lequal varQ fin

  checkBool <- load varBool boolType
  tag       <- addUnNamedInstruction boolType $ _and check' checkBool
  setLabel code $ condBranch tag code final

  e'   <- createExpression exp
  sum' <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
  store intType  op  sum'

  case opQ of
    AST.ForAll -> store boolType op' e'
    AST.Exists -> do  bool <- addUnNamedInstruction boolType $ _not e'
                      store boolType op' bool

  setLabel final $ branch initial

  case opQ of
    AST.ForAll -> load varBool boolType
    AST.Exists -> do  checkBool' <- load varBool boolType
                      addUnNamedInstruction boolType $ _not checkBool'


createQuant False opQ var pos exp (SpanRange a b) = do
  let ini = constantInt a
  let fin = constantInt b
  op <- alloca Nothing intType var
  store intType op ini
  addVarOperand var op

  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  let varQuant = "Quant_" ++ var
  let tyExp = AST.qVarType exp

  case tyExp of
    T.GInt -> do
      op' <- alloca Nothing intType varQuant
      case opQ of
        AST.Summation -> store intType op' $ constantInt 0
        AST.Product   -> store intType op' $ constantInt 1
        AST.Maximum   -> store intType op' $ constantInt minInteger
        AST.Minimum   -> store intType op' $ constantInt maxInteger

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
        AST.Summation -> do
          check <- checkOverflow AST.Sum pos res e' T.GInt
          store intType op' check
        AST.Product   -> do
          check <- checkOverflow AST.Mul pos res e' T.GInt
          store intType op' check
        AST.Maximum   -> do
          check <- addUnNamedInstruction intType $ _max res e'
          store intType op' check
        AST.Minimum   -> do
          check <- addUnNamedInstruction intType $ _min res e'
          store intType op' check

      setLabel final $ branch initial
      return res

    T.GFloat -> do
      op' <- alloca Nothing floatType varQuant

      case opQ of
        AST.Summation -> store floatType op' $ constantFloat 0.0
        AST.Product   -> store floatType op' $ constantFloat 1.0
        AST.Maximum   -> store floatType op' $ constantFloat minDouble
        AST.Minimum   -> store floatType op' $ constantFloat maxDouble

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
        AST.Summation -> do check <- addUnNamedInstruction floatType $ _addF res e'
                            store floatType op' check
        AST.Product   -> do check <- addUnNamedInstruction floatType $ _mulF res e'
                            store floatType op' check
        AST.Maximum   -> do check <- addUnNamedInstruction floatType $ _maxF res e'
                            store floatType op' check
        AST.Minimum   -> do check <- addUnNamedInstruction floatType $ _minF res e'
                            store floatType op' check

      setLabel final $ branch initial
      return res


data RangeCodegen
  = SetOp
    { getOp   :: AST.OpSet
    , getLexp :: RangeCodegen
    , getRexp :: RangeCodegen
    }
  | RangeOp
    { getLeft  :: Operand
    , getRight :: Operand
    }
  deriving (Eq)


doRange opQ (AST.QRange op lexp rexp) pos = undefined--do
  -- l <- doRange opQ lexp pos
  -- r <- doRange opQ rexp pos

  -- case op of
  --   AST.Intersec -> intersecRange opQ l r pos
  --   AST.Union    -> return $ SetOp AST.Union l r


doRange _ (AST.QRange lexp rexp) _ = undefined--do
  -- l <- createExpression lexp
  -- r <- createExpression rexp
  -- return $ RangeOp  l r


intersecRange opQ (RangeOp l1 r1) (RangeOp l2 r2) pos = do
  l <- addUnNamedInstruction intType $ _max l1 l2
  r <- addUnNamedInstruction intType $ _min r1 r2

  check <- addUnNamedInstruction boolType $ _less r l

  error <- newLabel
  final <- newLabel

  setLabel error $ condBranch check error final

  case opQ of
    AST.Maximum -> createTagRangeAbort final pos
    AST.Minimum -> createTagRangeAbort final pos
    _             -> createTagRange      final pos

  return $ RangeOp l r


makeRanges (SetOp _ lexp rexp) = do
  l <- makeRanges lexp
  r <- makeRanges rexp

  return $ l ++ r

makeRanges res@(RangeOp _ _) = return [res]


createQuant' :: Bool -> AST.QuantOp -> String -> SourcePos
             -> AST T.Type -> RangeCodegen -> LLVM Operand
createQuant' True opQ var pos exp (RangeOp a b) = do
  let ini = a
  let fin = b

  op <- alloca Nothing intType var
  store intType op ini
  addVarOperand var op

  empty   <- newLabel
  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  name <- getCount
  let varBool = show name
  op' <- alloca Nothing boolType varBool
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
    AST.ForAll -> store boolType op' e'
    AST.Exists -> do  bool <- addUnNamedInstruction boolType $ _not e'
                      store boolType op' bool


  setLabel final $ branch initial

  case opQ of
    AST.ForAll -> load varBool boolType
    AST.Exists -> do
      checkBool' <- load varBool boolType
      addUnNamedInstruction boolType $ _not checkBool'


createQuant' False opQ var pos exp (RangeOp a b) = do
  let ini = a
  let fin = b
  op <- alloca Nothing intType var
  store intType op ini
  addVarOperand var op

  empty   <- newLabel
  initial <- newLabel
  code    <- newLabel
  final   <- newLabel

  let varQuant = "Quant_" ++ var
  let tyExp = AST.qVarType exp

  case tyExp of
    T.GInt   -> do
      op' <- alloca Nothing intType varQuant

      case opQ of
        AST.Summation -> store intType op' $ constantInt 0
        AST.Product   -> store intType op' $ constantInt 1
        AST.Maximum   -> store intType op' $ constantInt minInteger
        AST.Minimum   -> store intType op' $ constantInt maxInteger

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
        AST.Summation -> do
          check <- checkOverflow AST.Sum pos res e' T.GInt
          store intType op' check
        AST.Product   -> do
          check <- checkOverflow AST.Mul pos res e' T.GInt
          store intType op' check
        AST.Maximum   -> do
          check <- addUnNamedInstruction intType $ _max res e'
          store intType op' check
        AST.Minimum   -> do
          check <- addUnNamedInstruction intType $ _min res e'
          store intType op' check

      setLabel final $ branch initial

      load varQuant intType

    T.GFloat -> do
      op' <- alloca Nothing floatType varQuant

      case opQ of
        AST.Summation -> store floatType op' $ constantFloat 0.0
        AST.Product   -> store floatType op' $ constantFloat 1.0
        AST.Maximum   -> store floatType op' $ constantFloat minDouble
        AST.Minimum   -> store floatType op' $ constantFloat maxDouble


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
        AST.Summation -> do
          check <- addUnNamedInstruction floatType $ _addF res e'
          store floatType op' check
        AST.Product   -> do
          check <- addUnNamedInstruction floatType $ _mulF res e'
          store floatType op' check
        AST.Maximum   -> do
          check <- addUnNamedInstruction floatType $ _maxF res e'
          store floatType op' check
        AST.Minimum   -> do
          check <- addUnNamedInstruction floatType $ _minF res e'
          store floatType op' check

      setLabel final $ branch initial

      load varQuant floatType


checkDivZero :: AST.OpNum -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
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


checkOverflow :: AST.OpNum -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
checkOverflow op pos lexp rexp ty = do
  overAbort <- newLabel
  next      <- newLabel
  res       <- addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp rexp
  check     <- extracValue res 1

  setLabel overAbort $ condBranch check overAbort next
  createTagOverflow next pos

  condName .= next
  extracValue res 0


genExpGuards :: [AST T.Type] -> Name -> Name -> LLVM [(Operand, Name)]
genExpGuards [guard] none one  = do
  r <- genExpGuard guard none
  return [r]

genExpGuards (guard:xs) none one = do
  next <- newLabel
  r    <- genExpGuard guard next
  setLabel next $ branch one
  rl   <- genExpGuards xs none one
  return $ r:rl


createGuardExp :: AST T.Type -> Name -> LLVM (Operand, Name)
--createGuardExp (AST.Cond lguards _ rtype) _ = do
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


genExpGuard :: AST T.Type -> Name -> LLVM (Operand, Name)
genExpGuard (AST.Guard guard acc _ _) next = do
  tag  <- createExpression guard
  code <- newLabel
  condName .= code
  setLabel code $ condBranch tag code next
  createGuardExp acc code


createBasicBlocks :: [AST T.Type] -> Named Terminator -> LLVM ()
createBasicBlocks accs m800 = genIntructions accs
  where
    genIntructions (acc:xs) = do
      createInstruction acc
      genIntructions xs
    genIntructions [] =
      addBasicBlock m800


intToDouble :: Operand -> LLVM Operand
intToDouble x = addUnNamedInstruction floatType $ _toFloat x


doubleToInt :: Operand -> LLVM Operand
doubleToInt x = addUnNamedInstruction intType $ _toInt x


irArithmetic :: AST.OpNum -> T.Type -> Operand -> Operand -> Instruction
--irArithmetic AST.Sum T.GInt   a b = _add  a b
--irArithmetic AST.Sub T.GInt   a b = Sub False False a b []
--irArithmetic AST.Mul T.GInt   a b = _mul a b
irArithmetic AST.Sum T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intAdd))) [(a, []),(b, [])] [] []
irArithmetic AST.Sub T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intSub))) [(a, []),(b, [])] [] []
irArithmetic AST.Mul T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
                                         (Name intMul))) [(a, []),(b, [])] [] []
irArithmetic AST.Sum T.GFloat a b = _addF   a b
irArithmetic AST.Mul T.GFloat a b = _mulF   a b
irArithmetic AST.Sub T.GFloat a b = FSub NoFastMathFlags a b []
irArithmetic AST.Div T.GInt   a b = SDiv True a b []
irArithmetic AST.Div T.GFloat a b = FDiv NoFastMathFlags a b []
irArithmetic AST.Mod T.GInt   a b = URem a b []
irArithmetic AST.Mod T.GFloat a b = FRem NoFastMathFlags a b []
irArithmetic AST.Exp T.GFloat a b = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name powString)))    [(a, []),(b, [])] [] []
irArithmetic AST.Min T.GFloat a b = _minF a b
irArithmetic AST.Max T.GFloat a b = _maxF a b
irArithmetic AST.Max T.GInt   a b = _max  a b
irArithmetic AST.Min T.GInt   a b = _min  a b


irBoolean :: AST.OpBool -> Operand -> Operand -> Instruction
irBoolean AST.Con a b = _and a b
irBoolean AST.Dis a b = _or  a b


irRelational :: AST.OpRel -> T.Type -> Operand -> Operand -> Instruction
irRelational AST.Equ     T.GFloat a b = FCmp FL.OEQ a b []
irRelational AST.Less    T.GFloat a b = FCmp FL.OLT a b []
irRelational AST.Greater T.GFloat a b = FCmp FL.OGT a b []
irRelational AST.LEqual  T.GFloat a b = FCmp FL.OLE a b []
irRelational AST.GEqual  T.GFloat a b = FCmp FL.OGE a b []
irRelational AST.Ine     T.GFloat a b = FCmp FL.ONE a b []


irRelational AST.Equ     T.GInt   a b = ICmp IL.EQ  a b []
irRelational AST.Less    T.GInt   a b = _less   a b
irRelational AST.Greater T.GInt   a b = ICmp IL.SGT a b []
irRelational AST.LEqual  T.GInt   a b = _lequal a b
irRelational AST.GEqual  T.GInt   a b = ICmp IL.SGE a b []
irRelational AST.Ine     T.GInt   a b = ICmp IL.NE  a b []


irConversion :: AST.Conv -> T.Type -> Operand -> Instruction
irConversion AST.ToInt    T.GFloat a = _toInt   a
irConversion AST.ToInt    T.GBoolean
          (ConstantOperand (C.Int 1 0)) = _toInt $ constantInt 0
irConversion AST.ToInt    T.GBoolean
          (ConstantOperand (C.Int 1 1)) = _toInt $ constantInt 1
irConversion AST.ToDouble T.GInt   a = _toFloat a
irConversion AST.ToDouble T.GChar  a = _toFloat a
irConversion AST.ToChar   T.GInt   a = Trunc  a charType  []
irConversion AST.ToChar   T.GFloat a = FPToSI a charType  []


irUnary :: AST.OpUn -> T.Type -> Operand -> Instruction
irUnary AST.Minus T.GInt   a = Sub False False      (constantInt 0) a []
irUnary AST.Minus T.GFloat a = FSub NoFastMathFlags (constantFloat 0) a []
irUnary AST.Abs   T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name fabsString))) [(a, [])] [] []
irUnary AST.Sqrt  T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
                                         (Name sqrtString))) [(a, [])] [] []
irUnary AST.Not   T.GBoolean  a = _not a


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
