module LLVM.Expression

where 
--------------------------------------------------------------------------------
import           Aborts
import           AST                                     (AST(..))
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
  


createExpression :: AST -> LLVM Operand
createExpression (AST.Id _ id t) = do
  var <- use varsLoc
  let (n, ty) = (TE.unpack id, toType t)
  let check   = DM.lookup n var

  case check of
    Just add -> case t of
      T.GArray _ _ -> return add
      _            -> load n ty
    Nothing -> return $ local ty (Name n)


createExpression (AST.Cond lguards pos rtype) = do
  final  <- newLabel
  abort  <- newLabel
  lnames <- genExpGuards lguards abort final
  let rtype' = toType rtype
  setLabel abort $ branch final
  createTagIf final pos
  condName .= final
  addUnNamedInstruction rtype' $ Phi rtype' lnames []


createExpression (AST.ArrCall _ id' accs t) = do
  accs' <- mapM createExpression accs
  map   <- use varsLoc
  let (t', i, id) = (toType t, myFromJust $ DM.lookup id map, TE.unpack id')
  accs'' <- opsToArrayIndex id accs'
  add    <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
  addUnNamedInstruction t' $ Load False add Nothing 0 []


createExpression (AST.Int _ n _) =
  return $ constantInt n


createExpression (AST.Float _ n _) =
  return $ constantFloat n


createExpression (AST.Bool _ True  _) =
  return $ constantBool 1


createExpression (AST.Bool _ False _) =
  return $ constantBool 0


createExpression (AST.Char _ n _) =
  return $ constantChar n


createExpression (AST.String _ msg _) =
  addStringOpe msg


createExpression (AST.Conversion tType _ exp t) = do
  let t' = AST.tag exp
  exp' <- createExpression exp

  if t' == T.GChar && tType == AST.ToInt then do
    op <- intToDouble exp'
    doubleToInt op
  else
    addUnNamedInstruction (toType t) $ irConversion tType t' exp'


createExpression (AST.Arithmetic op pos lexp rexp ty) = do
  lexp' <- createExpression lexp
  rexp' <- createExpression rexp

  case op of
    AST.Exp -> do a   <- intToDouble lexp'
                  b   <- intToDouble rexp'
                  val <- addUnNamedInstruction floatType $ irArithmetic AST.Exp T.GFloat a b
                  doubleToInt val
    AST.Max -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'
    AST.Min -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'
    AST.Div -> checkDivZero  op pos lexp' rexp' ty
    AST.Mod -> checkDivZero  op pos lexp' rexp' ty
    _         -> case ty of
      T.GInt   -> checkOverflow op pos lexp' rexp' ty
      T.GFloat -> addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp' rexp'


createExpression (AST.Boolean op _ lexp rexp t) = do

  lexp' <- createExpression lexp
  rexp' <- createExpression rexp

  case op of
    AST.Implies -> do notA <- addUnNamedInstruction boolType $ _not lexp'
                      addUnNamedInstruction boolType $ _or notA rexp'

    AST.Conse   -> do notA <- addUnNamedInstruction boolType $ _not rexp'
                      addUnNamedInstruction boolType $ _or notA lexp'

    _             -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'


createExpression (AST.Relational op _ lexp rexp t) = do
  lexp' <- createExpression lexp
  rexp' <- createExpression rexp
  let t' = AST.tag lexp
  addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


createExpression (AST.Unary AST.Abs _ exp T.GInt) = do
  exp'  <- createExpression exp
  x     <- intToDouble exp'
  val   <- addUnNamedInstruction intType $ irUnary AST.Abs T.GFloat x
  doubleToInt val


createExpression (AST.Unary AST.Sqrt _ exp t) = do
  let ty = AST.tag exp
  let df = Right $ definedFunction floatType (Name sqrtString)
  exp'  <- createExpression exp

  case ty of
    T.GFloat ->
      addUnNamedInstruction floatType $ irUnary AST.Sqrt ty exp'
    T.GInt   -> do
      x <- intToDouble exp'
      addUnNamedInstruction floatType $ irUnary AST.Sqrt T.GFloat x


createExpression (AST.Unary op _ exp t) = do
  exp' <- createExpression exp
  addUnNamedInstruction (toType t) $ irUnary op t exp'


createExpression (AST.FCallExp fname st _ args t) = do
  exp     <- mapM createExpression args
  let ty   =  toType t
  let exp' = map (\i -> (i,[])) exp
  let op   = definedFunction ty (Name $ TE.unpack fname)
  caller ty (Right op) exp'


{- No estoy seguro de esto -}

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