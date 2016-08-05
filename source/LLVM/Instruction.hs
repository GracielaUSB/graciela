module LLVM.Instruction

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Instruction
-- import qualified AST.Instruction                         as AST

import           Limits
import           LLVM.CodegenState
import           LLVM.Expression
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


createInstruction :: Instruction -> LLVM ()
createInstruction (Instruction loc t inst') = case ast' of

  EmptyAST {} -> return ()
  Id       {} -> return ()
  Skip        -> return ()


  Abort -> do
    createTagAbort pos
    return ()

  -- GuardAction assert action -> do
  --   createState "" assert
  --   createInstruction action

  -- LAssign [id] [exp] -> do
  --   let ty = toType $ expType id
  --   e'  <- createExpression exp
  --   id' <- getStoreDir id
  --   store ty id' e'
  --   return ()

  -- LAssign ids exps -> do
  --   list <- zipWithM createAssign ids exps
  --   zipWithM_ createMultyAssign ids list

  Write True exp -> do
    let ty  = expType exp
    let ty' = voidType
    e'     <- createExpression exp
    case ty of
      T.GInt      -> procedureCall ty' writeLnInt    [e']
      T.GFloat    -> procedureCall ty' writeLnDouble [e']
      T.GBoolean  -> procedureCall ty' writeLnBool   [e']
      T.GChar     -> procedureCall ty' writeLnChar   [e']
      T.GEmpty    -> procedureCall ty' writeLnString [e']
    return ()

  Write False exp -> do
    let ty  = expType exp
    let ty' = voidType
    e'     <- createExpression exp

    case ty of
      T.GInt      -> procedureCall ty' writeInt    [e']
      T.GFloat    -> procedureCall ty' writeDouble [e']
      T.GBoolean  -> procedureCall ty' writeBool   [e']
      T.GChar     -> procedureCall ty' writeChar   [e']
      T.GEmpty    -> procedureCall ty' writeString [e']
    return ()

  Block st decs accs -> do
    mapM_ accToAlloca decs
    mapM_ createInstruction accs
    return ()

  Conditional guards -> do
    final <- newLabel
    abort <- newLabel
    genGuards guards abort final

    setLabel abort $ branch final
    createTagIf final pos
    return ()

  Repeat guards inv bound -> do
    final   <- newLabel
    initial <- newLabel

    name <- getCount
    let boundName = show name
    op' <- alloca Nothing intType boundName
    store intType op' $ constantInt maxInteger
    addVarOperand (show boundName) op'

    setLabel initial $ branch initial
    createState "" inv
    createState boundName bound
    genGuards guards final initial
    setLabel final $ branch initial
    return ()

  ProcCallCont pname st args c -> do
    let dic   = getMap . getCurrent . procTable $ c
    let nargp = procArgs c
    exp <- createArguments dic nargp args
    procedureCall voidType (TE.unpack pname) exp
    return ()

  Ran id t -> do
    vars <- use varsLoc
    let (ty, i) = (toType t, fromJust $ DM.lookup (TE.unpack id) vars)
    let df      = Right $ definedFunction floatType (Name randomInt)
    val <- caller ty df []
    store ty i val
    return ()



accToAlloca :: AST -> LLVM ()
accToAlloca acc@(AST pos _ t ast') = case ast' of

  Id name -> do
    let name' = TE.unpack name
    dim <- typeToOperand name' t
    let t' = toType t
    alloca dim t' name'

    case t of
      T.GArray d ty -> createInstruction acc
      _             -> do
        initialize name' t
        createInstruction acc

  LAssign lids _  -> do
    mapM_ idToAlloca lids
    createInstruction acc

  Read Nothing types vars -> do
    res <- mapM callRead types
    ads <- mapM (getVarOperand . TE.unpack . fst) vars
    mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
    return ()

  Read (Just arch) types vars -> do
    res <- mapM (callReadFile $ TE.unpack arch) types
    ads <- mapM (getVarOperand . TE.unpack . fst) vars
    mapM_ (\(ty, r, a) -> store (toType ty) a r) $ zip3 types res ads
    return ()


convertFile :: String -> String
convertFile file = "__"++file

convertId :: String -> String
convertId name = '_':name

convertId' :: AST -> String
convertId' (AST _ _ _ ast') = case ast' of
  ArrCall id _ -> "__" ++ TE.unpack id
  Id      id   -> "__" ++ TE.unpack id
  _            -> undefined

convertId'' :: AST -> String
convertId'' (AST _ _ _ ast') = case ast' of
  ArrCall id _ -> "___" ++ TE.unpack id
  Id      id   -> "___" ++ TE.unpack id
  _            -> undefined

procedureCall :: Type -> String -> [Operand] -> LLVM Operand
procedureCall t pname es = do
  let es' = map (\e -> (e, [])) es
  let df  = Right $ definedFunction t (Name pname)
  caller t df es'


getStoreDir :: AST -> LLVM Operand
getStoreDir (AST pos _ t ast') = case ast' of

  Id name ->  getVarOperand (TE.unpack name)

  ArrCall name exps -> do
    ac' <- mapM createExpression exps
    map <- use varsLoc
    let (i, id) = (fromJust $ DM.lookup id map, TE.unpack name)
    ac'' <- opsToArrayIndex id ac'
    addUnNamedInstruction intType $ GetElementPtr True i [ac''] []

  _ -> undefined


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


createAssign :: AST -> AST -> LLVM String
createAssign id' exp = do
  let id = convertId' id'
  let ty = toType $ expType exp
  e'  <- createExpression exp
  op  <- checkVar id ty
  res <- store ty op e'
  return id


createMultyAssign :: AST -> String -> LLVM ()
createMultyAssign id aux = do
  let ty = toType $ expType id
  id'   <- getStoreDir id
  e'    <- load aux ty
  store ty id' e'
  return ()

createArguments :: DM.Map TE.Text (Contents SymbolTable)
                -> [TE.Text] -> [AST] -> LLVM [Operand]
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
        fromJust $ AST.astToId arg) dicn) : lr


genGuards :: [AST] -> Name -> Name -> LLVM ()
genGuards [guard] none one = genGuard guard none

genGuards (guard:xs) none one = do
  next <- newLabel
  genGuard guard next
  setLabel next $ branch one
  genGuards xs none one


genGuard :: AST -> Name -> LLVM ()
genGuard (AST _ _ _ (Guard guard acc)) next = do
  tag  <- createExpression guard
  code <- newLabel
  setLabel code $ condBranch tag code next
  createInstruction acc

createState :: String -> AST -> LLVM ()
createState name (AST pos _ _ (States cond exp)) = do
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

idToAlloca :: AST -> LLVM ()
idToAlloca (AST _ _ t (Id id)) = do
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
