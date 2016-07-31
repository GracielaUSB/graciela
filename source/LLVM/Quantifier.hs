module LLVM.Quantifier where

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
import           Text.Megaparsec.Pos                     (SourcePos)



createQuant :: Bool -> AST.QuantOp -> String -> SourcePos ->
                   AST -> Range Integer -> LLVM Operand
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
  let tyExp = astType exp

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
  foldM (\acc i -> addUnNamedInstruction intType $ _min acc i) (head res) (tail res)

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


{- No estoy muy seguro de esto, solo se usa en un lugar que
   del que tampoco estoy seguro.
-}

-- doRange opQ (AST pos _ _ ast' = case ast' of

--  QRange op lexp rexp = undefined--do
--   -- l <- doRange opQ lexp pos
--   -- r <- doRange opQ rexp pos

--   -- case op of
--   --   AST.Intersec -> intersecRange opQ l r pos
--   --   AST.Union    -> return $ SetOp AST.Union l r


--   QRange lexp rexp = undefined--do
--   -- l <- createExpression lexp
--   -- r <- createExpression rexp
--   -- return $ RangeOp  l r


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
             -> AST -> RangeCodegen -> LLVM Operand
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
  let tyExp = astType exp

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
