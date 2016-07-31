module LLVM.Expression

where 
--------------------------------------------------------------------------------
import           Aborts
import           AST                                     (AST(..), AST'(..))
import qualified AST                                     as AST
import           Contents
import           Limits
import           LLVM.CodegenState
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
import           Text.Megaparsec                         (SourcePos)
import           LLVM.General.AST.Name                   (Name(..))
import           LLVM.General.AST.Instruction            (Instruction(..), Named(..),
                                                          Terminator(..), FastMathFlags(..))
import           LLVM.General.AST.Operand                (Operand(..), CallableOperand)
import           LLVM.General.AST                        (Definition(..), Module(..))
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Type                   
  


createExpression :: AST -> LLVM Operand
createExpression (AST posFrom posTo t ast') = case ast' of 

  {- Basic Types -}
  Int n      -> return $ constantInt n

  Float n    -> return $ constantFloat n

  Bool True  -> return $ constantBool 1

  Bool False -> return $ constantBool 0

  Char n     -> return $ constantChar n

  String msg -> addStringOpe msg 

  Id name -> do
    var <- use varsLoc
    let (n, ty) = (TE.unpack name, toType t)
    let check   = DM.lookup n var

    case check of
      Just add -> case t of
        T.GArray _ _ -> return add
        _            -> load n ty
      Nothing -> return $ local ty (Name n)


  Cond lguards -> do
    final  <- newLabel
    abort  <- newLabel
    lnames <- genExpGuards lguards abort final
    let t' = toType t
    setLabel abort $ branch final
    createTagIf final posFrom
    condName .= final
    addUnNamedInstruction t' $ Phi t' lnames []


  ArrCall name' accs -> do
    accs' <- mapM createExpression accs
    map   <- use varsLoc
    let (t', i, name) = (toType t, fromJust $ DM.lookup name map, TE.unpack name')
    accs'' <- opsToArrayIndex name accs'
    add    <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
    addUnNamedInstruction t' $ Load False add Nothing 0 []
  
  Conversion tType expr -> do
    let t' = astType expr
    expr'  <- createExpression expr

    if t' == T.GChar && tType == AST.ToInt
      then do
        op <- intToDouble expr'
        doubleToInt op
      else
        addUnNamedInstruction (toType t) $ irConversion tType t' expr'


  Arithmetic op lexp rexp -> do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    case op of
      AST.Exp -> do a   <- intToDouble lexp'
                    b   <- intToDouble rexp'
                    val <- addUnNamedInstruction floatType $ 
                           irArithmetic AST.Exp T.GFloat a b
                    doubleToInt val
      AST.Max -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
      AST.Min -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
      AST.Div -> checkDivZero  op posFrom lexp' rexp' t
      AST.Mod -> checkDivZero  op posFrom lexp' rexp' t
      _         -> case t of
        T.GInt   -> checkOverflow op posFrom lexp' rexp' t
        T.GFloat -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'


  Boolean op lexp rexp -> do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    case op of
      AST.Implies -> do notA <- addUnNamedInstruction boolType $ _not lexp'
                        addUnNamedInstruction boolType $ _or notA rexp'

      AST.Conse   -> do notA <- addUnNamedInstruction boolType $ _not rexp'
                        addUnNamedInstruction boolType $ _or notA lexp'

      _             -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'


  Relational op lexp rexp -> do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    let t' = astType lexp
    addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


  Unary AST.Abs exp | t == T.GInt -> do
    exp'  <- createExpression exp
    x     <- intToDouble exp'
    val   <- addUnNamedInstruction intType $ irUnary AST.Abs T.GFloat x
    doubleToInt val


  Unary AST.Sqrt exp -> do
    let ty = astType exp
    let df = Right $ definedFunction floatType (Name sqrtString)
    exp'  <- createExpression exp
    case ty of
      T.GFloat ->
        addUnNamedInstruction floatType $ irUnary AST.Sqrt ty exp'
      T.GInt   -> do
        x <- intToDouble exp'
        addUnNamedInstruction floatType $ irUnary AST.Sqrt T.GFloat x


  Unary op exp -> do
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irUnary op t exp'


  FCallExp fname st args -> do
    exp     <- mapM createExpression args
    let ty   =  toType t
    let exp' = map (\i -> (i,[])) exp
    let op   = definedFunction ty (Name $ TE.unpack fname)
    caller ty (Right op) exp'


{- Comentado porque no estoy seguro de esto -}

-- createExpression (AST.QuantRan opQ varQ pos rangeExp termExp t) = do
--   let name  = TE.unpack varQ
--   let tyExp = AST.qVarType termExp

--   case opQ of
--     AST.ForAll -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
--                       joinRange opQ check pos tyExp

--     AST.Exists -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
--                       joinRange opQ check pos tyExp

--     _          -> do  check <- mapM (createQuant False opQ name pos termExp) rangeExp
--                       joinRange opQ check pos tyExp


-- createExpression (AST.QuantRanUn opQ varQ pos rangeExp termExp t) = do
--   let name  = TE.unpack varQ
--   let tyExp = AST.qVarType termExp

--   ranges  <- doRange opQ rangeExp pos
--   rangesF <- makeRanges ranges

--   case opQ of
--     AST.ForAll -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
--                       joinRange opQ check pos tyExp

--     AST.Exists -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
--                       joinRange opQ check pos tyExp

--     _          -> do  check <- mapM (createQuant' False opQ name pos termExp) rangesF
--                       joinRange opQ check pos tyExp


genExpGuards :: [AST] -> Name -> Name -> LLVM [(Operand, Name)]
genExpGuards [guard] none one  = do
  r <- genExpGuard guard none
  return [r]

genExpGuards (guard:xs) none one = do
  next <- newLabel
  r    <- genExpGuard guard next
  setLabel next $ branch one
  rl   <- genExpGuards xs none one
  return $ r:rl


createGuardExp :: AST -> Name -> LLVM (Operand, Name)
{- Ya estaba comentado (?)-}
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


genExpGuard :: AST -> Name -> LLVM (Operand, Name)
genExpGuard (AST _ _ _ (Guard guard acc)) next = do
  tag  <- createExpression guard
  code <- newLabel
  condName .= code
  setLabel code $ condBranch tag code next
  createGuardExp acc code

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
