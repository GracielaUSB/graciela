module LLVM.Instruction

where 
--------------------------------------------------------------------------------
import           Aborts
import           AST                                     (AST(..))
import qualified AST                                     as AST
import           Contents
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



createInstruction :: AST T.Type -> LLVM ()
createInstruction AST.EmptyAST {} = return ()
createInstruction AST.Id       {} = return ()
createInstruction AST.Skip     {} = return ()


createInstruction (AST.Abort pos _) = do
  createTagAbort pos
  return ()


createInstruction (AST.GuardAction _ assert action ty) = do
  createState "" assert
  createInstruction action


createInstruction (AST.LAssign [id] [exp] _ _) = do
  let ty = toType $ AST.tag id
  e'  <- createExpression exp
  id' <- getStoreDir id
  store ty id' e'
  return ()


createInstruction (AST.LAssign ids exps _ _) = do
  list <- zipWithM createAssign ids exps
  zipWithM_ createMultyAssign ids list


createInstruction (AST.Write True exp _ t) = do
  let ty  = AST.tag exp
  let ty' = voidType
  e'     <- createExpression exp

  case ty of
    T.GInt      -> procedureCall ty' writeLnInt    [e']
    T.GFloat    -> procedureCall ty' writeLnDouble [e']
    T.GBool  -> procedureCall ty' writeLnBool   [e']
    T.GChar     -> procedureCall ty' writeLnChar   [e']
    T.GEmpty    -> procedureCall ty' writeLnString [e']
  return ()


createInstruction (AST.Write False exp _ t) = do
  let ty  = AST.tag exp
  let ty' = voidType
  e'     <- createExpression exp

  case ty of
    T.GInt      -> procedureCall ty' writeInt    [e']
    T.GFloat    -> procedureCall ty' writeDouble [e']
    T.GBool  -> procedureCall ty' writeBool   [e']
    T.GChar     -> procedureCall ty' writeChar   [e']
    T.GEmpty    -> procedureCall ty' writeString [e']
  return ()


createInstruction (AST.Block _ st decs accs _) = do
  mapM_ accToAlloca decs
  mapM_ createInstruction accs
  return ()


createInstruction (AST.Cond guards pos _) = do
  final <- newLabel
  abort <- newLabel
  genGuards guards abort final

  setLabel abort $ branch final
  createTagIf final pos
  return ()


createInstruction (AST.Rept guards inv bound _ _) = do
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


createInstruction (AST.ProcCallCont pname st _ args c _) = do
  let dic   = getMap . getCurrent . procTable $ c
  let nargp = procArgs c
  exp <- createArguments dic nargp args
  procedureCall voidType (TE.unpack pname) exp
  return ()


createInstruction (AST.Ran id _ _ t) = do
  vars <- use varsLoc
  let (ty, i) = (toType t, fromJust $ DM.lookup (TE.unpack id) vars)
  let df      = Right $ definedFunction floatType (Name randomInt)
  val <- caller ty df []
  store ty i val
  return ()
