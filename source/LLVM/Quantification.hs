module LLVM.Quantification where

--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression                          (BinaryOperator (Plus, Times),
                                                          Expression,
                                                          QuantOperator (..))

import           Limits
import           LLVM.Expression
import           LLVM.Instruction
import           LLVM.State
import           SymbolTable
import qualified Type                                    (Type (..))
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.Trans.State
import           Data.Foldable                           (toList)
import           Data.Maybe
import           Data.Range.Range                        as RA
import qualified Data.Text                               as TE
import           Data.Word
import           LLVM.General.AST                        (Definition (..),
                                                          Module (..))
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Name                   (Name (..))
import           LLVM.General.AST.Operand                (CallableOperand,
                                                          Operand (..))
import           LLVM.General.AST.Type
import           Text.Megaparsec.Pos                     (SourcePos)
--------------------------------------------------------------------------------


-- createQuant :: Bool -> QuantOperator -> String -> SourcePos ->
--                    Expression -> Range Integer -> LLVM Operand
-- createQuant True opQ var pos exp (SpanRange a b) = do
--   let ini = constantInt a
--   let fin = constantInt b
--   op     <- alloca Nothing intType var
--   store intType op ini
--   addVarOperand var op

--   initial <- newLabel
--   code    <- newLabel
--   final   <- newLabel

--   name <- getCount
--   let varBool = show name
--   op' <- alloca Nothing boolType varBool
--   store boolType op' $ constantBool 1
--   addVarOperand varBool op'
--   setLabel initial $ branch initial

--   varQ   <- load var intType
--   check' <- addUnNamedInstruction boolType $ _lequal varQ fin

--   checkBool <- load varBool boolType
--   tag       <- addUnNamedInstruction boolType $ _and check' checkBool
--   setLabel code $ condBranch tag code final

--   e'   <- createExpression exp
--   sum' <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--   store intType  op  sum'

--   case opQ of
--     ForAll -> store boolType op' e'
--     Exists -> do
--       bool <- addUnNamedInstruction boolType $ _not e'
--       store boolType op' bool

--   setLabel final $ branch initial

--   case opQ of
--     ForAll -> load varBool boolType
--     Exists -> do
--       checkBool' <- load varBool boolType
--       addUnNamedInstruction boolType $ _not checkBool'


-- createQuant False opQ var pos exp (SpanRange a b) = do
--   let ini = constantInt a
--   let fin = constantInt b
--   op <- alloca Nothing intType var
--   store intType op ini
--   addVarOperand var op

--   initial <- newLabel
--   code    <- newLabel
--   final   <- newLabel

--   let varQuant = "Quant_" ++ var
--   let tyExp = astType exp

--   case tyExp of
--     GInt -> do
--       op' <- alloca Nothing intType varQuant
--       case opQ of
--         Summation -> store intType op' $ constantInt 0
--         Product   -> store intType op' $ constantInt 1
--         Maximum   -> store intType op' $ constantInt minInteger
--         Minimum   -> store intType op' $ constantInt maxInteger

--       addVarOperand varQuant op'

--       setLabel initial $ branch initial
--       varQ <- load var intType
--       tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
--       res  <- load varQuant intType

--       setLabel code $ condBranch tag code final

--       e'   <- createExpression exp

--       sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--       store intType op  sum

--       case opQ of
--         Summation -> do
--           check <- checkOverflow Plus pos res e' GInt
--           store intType op' check
--         Product   -> do
--           check <- checkOverflow Times pos res e' GInt
--           store intType op' check
--         Maximum   -> do
--           check <- addUnNamedInstruction intType $ _max res e'
--           store intType op' check
--         Minimum   -> do
--           check <- addUnNamedInstruction intType $ _min res e'
--           store intType op' check

--       setLabel final $ branch initial
--       return res

--     GFloat -> do
--       op' <- alloca Nothing floatType varQuant

--       case opQ of
--         Summation -> store floatType op' $ constantFloat 0.0
--         Product   -> store floatType op' $ constantFloat 1.0
--         Maximum   -> store floatType op' $ constantFloat minDouble
--         Minimum   -> store floatType op' $ constantFloat maxDouble

--       addVarOperand varQuant op'

--       setLabel initial $ branch initial
--       varQ <- load var floatType
--       tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
--       res  <- load varQuant floatType

--       setLabel code $ condBranch tag code final

--       e'   <- createExpression exp

--       sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--       store floatType op  sum

--       case opQ of
--         Summation -> do check <- addUnNamedInstruction floatType $ _addF res e'
--                         store floatType op' check
--         Product   -> do check <- addUnNamedInstruction floatType $ _mulF res e'
--                         store floatType op' check
--         Maximum   -> do check <- addUnNamedInstruction floatType $ _maxF res e'
--                         store floatType op' check
--         Minimum   -> do check <- addUnNamedInstruction floatType $ _minF res e'
--                         store floatType op' check

--       setLabel final $ branch initial
--       return res


-- joinRange :: QuantOperator -> [Operand] -> SourcePos -> T.Type -> LLVM Operand
-- joinRange Summation res pos GInt   =
--   foldM (\acc i -> checkOverflow Plus pos acc i GInt) (head res) (tail res)

-- joinRange Summation res pos GFloat =
--   foldM (\acc i -> addUnNamedInstruction intType $ _addF acc i) (head res) (tail res)

-- joinRange Product   res pos GInt   =
--   foldM (\acc i -> checkOverflow Times pos acc i GInt) (head res) (tail res)

-- joinRange Product   res pos GFloat =
--   foldM (\acc i -> addUnNamedInstruction intType $ _mulF acc i) (head res) (tail res)

-- joinRange Maximum   res pos GInt   =
--   foldM (\acc i -> addUnNamedInstruction intType $ _max acc i) (head res) (tail res)

-- joinRange Maximum   res pos GFloat =
--   foldM (\acc i -> addUnNamedInstruction intType $ _maxF acc i) (head res) (tail res)

-- joinRange Minimum   res pos GInt   =
--   foldM (\acc i -> addUnNamedInstruction intType $ _min acc i) (head res) (tail res)

-- joinRange Minimum   res pos GFloat =
--   foldM (\acc i -> addUnNamedInstruction intType $ _minF acc i) (head res) (tail res)

-- joinRange opQ res pos _ = do
--   warAbort <- newLabel
--   next     <- newLabel
--   check    <- foldM (\acc i -> addUnNamedInstruction intType $ _and acc i) (head res) (tail res)
--   setLabel warAbort $ condBranch check next warAbort

--   case opQ of
--     ForAll -> createTagForAll next pos
--     Exists -> createTagExists next pos

--   return check


-- {- No estoy muy seguro de esto, solo se usa en un lugar que
--    del que tampoco estoy seguro.
-- -}

-- -- data RangeCodegen
-- --   = SetOp
-- --     { getOp   :: AST.OpSet
-- --     , getLexp :: RangeCodegen
-- --     , getRexp :: RangeCodegen
-- --     }
-- --   | RangeOp
-- --     { getLeft  :: Operand
-- --     , getRight :: Operand
-- --     }
-- --   deriving (Eq)

-- -- doRange opQ (AST pos _ _ ast' = case ast' of

-- --  QRange op lexp rexp = undefined--do
-- --   -- l <- doRange opQ lexp pos
-- --   -- r <- doRange opQ rexp pos

-- --   -- case op of
-- --   --   AST.Intersec -> intersecRange opQ l r pos
-- --   --   AST.Union    -> return $ SetOp AST.Union l r


-- --   QRange lexp rexp = undefined--do
-- --   -- l <- createExpression lexp
-- --   -- r <- createExpression rexp
-- --   -- return $ RangeOp  l r


-- intersecRange opQ (RangeOp l1 r1) (RangeOp l2 r2) pos = do
--   l <- addUnNamedInstruction intType $ _max l1 l2
--   r <- addUnNamedInstruction intType $ _min r1 r2

--   check <- addUnNamedInstruction boolType $ _less r l

--   error <- newLabel
--   final <- newLabel

--   setLabel error $ condBranch check error final

--   case opQ of
--     Maximum -> createTagRangeAbort final pos
--     Minimum -> createTagRangeAbort final pos
--     _       -> createTagRange      final pos

--   return $ RangeOp l r

-- makeRanges (SetOp _ lexp rexp) = do
--   l <- makeRanges lexp
--   r <- makeRanges rexp

--   return $ l ++ r

-- makeRanges res@(RangeOp _ _) = return [res]


-- createQuant' :: Bool -> QuantOperator -> String -> SourcePos
--              -> AST -> RangeCodegen -> LLVM Operand
-- createQuant' True opQ var pos exp (RangeOp a b) = do
--   let ini = a
--   let fin = b

--   op <- alloca Nothing intType var
--   store intType op ini
--   addVarOperand var op

--   empty   <- newLabel
--   initial <- newLabel
--   code    <- newLabel
--   final   <- newLabel

--   name <- getCount
--   let varBool = show name
--   op' <- alloca Nothing boolType varBool
--   store boolType op' $ constantBool 1
--   addVarOperand varBool op'

--   -- Check Empty Range
--   checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
--   setLabel empty $ condBranch checkRange initial empty


--   setLabel initial $ branch final
--   varQ   <- load var intType
--   check' <- addUnNamedInstruction boolType $ _lequal varQ fin

--   checkBool <- load varBool boolType
--   tag       <- addUnNamedInstruction boolType $ _and check' checkBool
--   setLabel code $ condBranch tag code final

--   e'   <- createExpression exp
--   sum' <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--   store intType  op  sum'

--   case opQ of
--     ForAll -> store boolType op' e'
--     Exists -> do
--       bool <- addUnNamedInstruction boolType $ _not e'
--       store boolType op' bool


--   setLabel final $ branch initial

--   case opQ of
--     ForAll -> load varBool boolType
--     Exists -> do
--       checkBool' <- load varBool boolType
--       addUnNamedInstruction boolType $ _not checkBool'


-- createQuant' False opQ var pos exp (RangeOp a b) = do
--   let ini = a
--   let fin = b
--   op <- alloca Nothing intType var
--   store intType op ini
--   addVarOperand var op

--   empty   <- newLabel
--   initial <- newLabel
--   code    <- newLabel
--   final   <- newLabel

--   let varQuant = "Quant_" ++ var
--   let tyExp = astType exp

--   case tyExp of
--     GInt -> do
--       op' <- alloca Nothing intType varQuant

--       case opQ of
--         Summation -> store intType op' $ constantInt 0
--         Product   -> store intType op' $ constantInt 1
--         Maximum   -> store intType op' $ constantInt minInteger
--         Minimum   -> store intType op' $ constantInt maxInteger

--       addVarOperand varQuant op'

--       -- Check Empty Range
--       checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
--       setLabel empty $ condBranch checkRange initial empty


--       setLabel initial $ branch final
--       varQ <- load var intType
--       tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
--       res  <- load varQuant intType

--       setLabel code $ condBranch tag code final

--       e'   <- createExpression exp

--       sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--       store intType op sum

--       case opQ of
--         Summation -> do
--           check <- checkOverflow Plus pos res e' GInt
--           store intType op' check
--         Product   -> do
--           check <- checkOverflow Times pos res e' GInt
--           store intType op' check
--         Maximum   -> do
--           check <- addUnNamedInstruction intType $ _max res e'
--           store intType op' check
--         Minimum   -> do
--           check <- addUnNamedInstruction intType $ _min res e'
--           store intType op' check

--       setLabel final $ branch initial

--       load varQuant intType

--     GFloat -> do
--       op' <- alloca Nothing floatType varQuant

--       case opQ of
--         Summation -> store floatType op' $ constantFloat 0.0
--         Product   -> store floatType op' $ constantFloat 1.0
--         Maximum   -> store floatType op' $ constantFloat minDouble
--         Minimum   -> store floatType op' $ constantFloat maxDouble


--       addVarOperand varQuant op'

--       -- Revisar Rango Vacio
--       checkRange <- addUnNamedInstruction boolType $ _lequal ini fin
--       setLabel empty $ condBranch checkRange initial empty


--       setLabel initial $ branch final
--       varQ <- load var floatType
--       tag  <- addUnNamedInstruction boolType $ _lequal varQ fin
--       res  <- load varQuant floatType

--       setLabel code $ condBranch tag code final

--       e'   <- createExpression exp

--       sum  <- addUnNamedInstruction boolType $ _add varQ $ constantInt 1
--       store floatType op  sum

--       case opQ of
--         Summation -> do
--           check <- addUnNamedInstruction floatType $ _addF res e'
--           store floatType op' check
--         Product   -> do
--           check <- addUnNamedInstruction floatType $ _mulF res e'
--           store floatType op' check
--         Maximum   -> do
--           check <- addUnNamedInstruction floatType $ _maxF res e'
--           store floatType op' check
--         Minimum   -> do
--           check <- addUnNamedInstruction floatType $ _minF res e'
--           store floatType op' check

--       setLabel final $ branch initial

--       load varQuant floatType
