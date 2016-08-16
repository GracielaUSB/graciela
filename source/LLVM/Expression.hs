{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Expression

where
--------------------------------------------------------------------------------
import           Aborts
import           AST.Expression                          hiding (BinaryOperator(..))
import qualified AST.Expression                          as Op (BinaryOperator(..))
import           AST.Expression                          (Expression(..), Object)
import           AST.Object                              (Object'(..), Object''(..))
import           AST.Type                                as T
import           Limits
import           LLVM.State
import           LLVM.Type                               (toLLVMType)
import           SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Data.Foldable                           (toList)
import           Data.Text                               (unpack)
import           Data.Word
import           Data.Monoid                             ((<>))
import           Data.Char                               (ord)
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import qualified LLVM.General.AST.Float                  as LLVM (SomeFloat(Double))
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction (..),
                                                          Named (..),
                                                          Terminator (..))
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Name                   (Name (..))
import           LLVM.General.AST.Operand                (CallableOperand,
                                                          Operand (..))
import           LLVM.General.AST.Type
import           Prelude                                 hiding (Ordering (..))
import           Debug.Trace                            
--------------------------------------------------------------------------------


expression :: Expression -> LLVM (Operand, [Named Instruction])
expression Expression { expType, constant, exp'} = case exp' of

  BoolLit   theBool   -> 
    return (ConstantOperand $ C.Int 1 (if theBool then 1 else 0), [])
  CharLit   theChar   -> 
    return (ConstantOperand $ C.Int 8 $ (fromIntegral . ord) theChar, [])
  FloatLit  theFloat  -> 
    return (ConstantOperand $ C.Float $ LLVM.Double theFloat, [])
  IntLit    theInt    -> 
    return (ConstantOperand $ C.Int 32 theInt, [])

  -- StringLit theString -> return $ ConstantOperand $ C.Int 32 10

  Binary { binOp, lexpr, rexpr } -> do
    (lOperand, lInsts) <- expression lexpr
    (rOperand, rInsts) <- expression rexpr

    label <- nextLabel
    let inst = case expType of
          T.GInt   -> opInt  binOp lOperand rOperand
          T.GBool  -> opBool binOp lOperand rOperand
          T.GFloat -> opFloat binOp lOperand rOperand

    let operand = LocalReference (toLLVMType expType) label

    return (operand, lInsts <> rInsts <> [label := inst])

    where 
      opInt :: Op.BinaryOperator -> Operand -> Operand -> Instruction
      opInt op lOperand rOperand = case op of
        -- Plus   -> secureIntFunc intAdd lOperand rOperand
        -- BMinus -> secureIntFunc intSub lOperand rOperand
        -- Times  -> secureIntFunc intMul lOperand rOperand
        Op.Plus   -> Add  { nsw      = False
                          , nuw      = False
                          , operand0 = lOperand
                          , operand1 = rOperand
                          , metadata = []
                          }
        Op.BMinus -> Sub  { nsw      = False
                          , nuw      = False
                          , operand0 = lOperand
                          , operand1 = rOperand
                          , metadata = []
                          }
        Op.Times  -> Mul  { nsw      = False
                          , nuw      = False
                          , operand0 = lOperand
                          , operand1 = rOperand
                          , metadata = []
                          }
        Op.Div    -> SDiv { exact = True
                          , operand0 = lOperand
                          , operand1 = rOperand
                          , metadata = [] 
                          }
        Op.Mod    -> SRem { operand0 = lOperand
                          , operand1 = rOperand 
                          , metadata = [] 
                          }
        -- Op.Power  -> undefined
        -- Op.Max    -> undefined
        -- Op.Min    -> undefined
        _         -> error "opFloat"

      opFloat :: Op.BinaryOperator -> Operand -> Operand -> Instruction
      opFloat op lOperand rOperand = case op of
        -- Plus   -> secureIntFunc intAdd lOperand rOperand
        -- BMinus -> secureIntFunc intSub lOperand rOperand
        -- Times  -> secureIntFunc intMul lOperand rOperand
        Op.Plus   -> FAdd { fastMathFlags = NoFastMathFlags 
                          , operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        Op.BMinus -> FSub { fastMathFlags = NoFastMathFlags
                          , operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        Op.Times  -> FMul { fastMathFlags = NoFastMathFlags
                          , operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        Op.Div   -> FDiv  { fastMathFlags = NoFastMathFlags
                          , operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        -- Op.Power  -> undefined
        -- Op.Max    -> undefined
        -- Op.Min    -> undefined
        _         -> error "opFloat"

      opBool :: Op.BinaryOperator -> Operand -> Operand -> Instruction
      opBool op lOperand rOperand = case op of
        Op.And     -> And { operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        Op.Or      -> Or  { operand0      = lOperand
                          , operand1      = rOperand
                          , metadata      = []
                          }
        _ -> error "opBool"
        -- Op.Implies    ->undefined
        -- Op.Consequent ->undefined
        -- Op.BEQ        ->undefined
        -- Op.BNE        ->undefined

      -- LLVM offers a set of secure operations that know when an int operation reach an overflow
      -- llvm.sadd.with.overflow.i32 (intAdd)
      -- llvm.ssub.with.overflow.i32 (intSub)
      -- llvm.smul.with.overflow.i32 (intMul)
      secureIntFunc :: String -> Operand -> Operand -> Instruction
      secureIntFunc fun lOperand rOperand = 
        let
          type' = StructureType False [i32, i1]
          funRef = Right . ConstantOperand $ C.GlobalReference i32 $ Name fun
        in Call { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = funRef
                , arguments          = [(lOperand,[]), (rOperand,[])]
                , functionAttributes = []
                , metadata           = []
                }

  Obj obj -> object obj


  -- Dummy operand
  _ -> return (ConstantOperand $ C.Int 32 10,[])


object :: Object -> LLVM (Operand, [Named Instruction])
object Object { objType, obj' } = case obj' of
    Variable { name } -> do
      label <- nextLabel
      let 
        -- Load the value in the variable address (e.g. %12 = load i32* %a, align 4)
        addrToLoad = LocalReference (toLLVMType objType) $ Name (unpack name) 
        load = Load { volatile  = False
                    , address   = addrToLoad 
                    , maybeAtomicity = Nothing
                    , alignment = 4
                    , metadata  = []
                    }
        -- Ref is the label where the variable value was loaded (e.g. %12)
        ref = LocalReference (toLLVMType objType) label
      return (ref, [label := load])

    _ -> error "Obj"


-- createExpression :: Expression -> LLVM Operand
-- createExpression (Expression loc expType constant exp') = case exp' of

--   {- Basic Types -}
--   BoolLit True  -> return $ constantBool  1
--   BoolLit False -> return $ constantBool  0
--   CharLit   c   -> return $ constantChar  c
--   IntLit    i   -> return $ constantInt   i
--   FloatLit  f   -> return $ constantFloat f
--   String   str  -> addStringOpe str

--   Id name -> do
--     var <- use varsLoc
--     let (n, ty) = (TE.unpack name, toType t)
--     let check   = DM.lookup n var

--     case check of
--       Just add -> case t of
--         T.GArray _ _ -> return add
--         _            -> load n ty
--       Nothing -> return $ local ty (Name n)


--   Cond lguards -> do
--     final  <- newLabel
--     abort  <- newLabel
--     lnames <- genExpGuards lguards abort final
--     let t' = toType t
--     setLabel abort $ branch final
--     createTagIf final posFrom
--     condName .= final
--     addUnNamedInstruction t' $ Phi t' lnames []


--   ArrCall name' accs -> do
--     accs' <- mapM createExpression accs
--     map   <- use varsLoc
--     let (t', i, name) = (toType t, fromJust $ DM.lookup name map, TE.unpack name')
--     accs'' <- opsToArrayIndex name accs'
--     add    <- addUnNamedInstruction t' $ GetElementPtr True i [accs''] []
--     addUnNamedInstruction t' $ Load False add Nothing 0 []

--   Conversion tType expr -> do
--     let t' = astType expr
--     expr'  <- createExpression expr

--     if t' == T.GChar && tType == ToInt
--       then do
--         op <- intToDouble expr'
--         doubleToInt op
--       else
--         addUnNamedInstruction (toType t) $ irConversion tType t' expr'


--   Arithmetic op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     case op of
--       Exp -> do a   <- intToDouble lexp'
--                 b   <- intToDouble rexp'
--                 val <- addUnNamedInstruction floatType $
--                        irArithmetic Exp T.GFloat a b
--                 doubleToInt val
--       Max -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
--       Min -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
--       Div -> checkDivZero  op posFrom lexp' rexp' t
--       Mod -> checkDivZero  op posFrom lexp' rexp' t
--       _   -> case t of
--         T.GInt   -> checkOverflow op posFrom lexp' rexp' t
--         T.GFloat -> addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'


--   Boolean op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     case op of
--       Implies    -> do
--         notA <- addUnNamedInstruction boolType $ _not lexp'
--         addUnNamedInstruction boolType $ _or notA rexp'

--       Consequent -> do
--         notA <- addUnNamedInstruction boolType $ _not rexp'
--         addUnNamedInstruction boolType $ _or notA lexp'

--       _ -> addUnNamedInstruction boolType $ irBoolean op lexp' rexp'


--   Relational op lexp rexp -> do
--     lexp' <- createExpression lexp
--     rexp' <- createExpression rexp
--     let t' = astType lexp
--     addUnNamedInstruction boolType $ irRelational op t' lexp' rexp'


--   Unary Abs exp | t == T.GInt -> do
--     exp'  <- createExpression exp
--     x     <- intToDouble exp'
--     val   <- addUnNamedInstruction intType $ irUnary Abs T.GFloat x
--     doubleToInt val


--   Unary Sqrt exp -> do
--     let ty = astType exp
--     let df = Right $ definedFunction floatType (Name sqrtString)
--     exp'  <- createExpression exp
--     case ty of
--       T.GFloat ->
--         addUnNamedInstruction floatType $ irUnary Sqrt ty exp'
--       T.GInt   -> do
--         x <- intToDouble exp'
--         addUnNamedInstruction floatType $ irUnary Sqrt T.GFloat x


--   Unary op exp -> do
--     exp' <- createExpression exp
--     addUnNamedInstruction (toType t) $ irUnary op t exp'


--   FCallExp fname st args -> do
--     exp     <- mapM createExpression args
--     let ty   =  toType t
--     let exp' = map (\i -> (i,[])) exp
--     let op   = definedFunction ty (Name $ TE.unpack fname)
--     caller ty (Right op) exp'


-- {- Comentado porque no estoy seguro de esto -}

-- -- createExpression (AST.QuantRan opQ varQ pos rangeExp termExp t) = do
-- --   let name  = TE.unpack varQ
-- --   let tyExp = AST.qVarType termExp

-- --   case opQ of
-- --     AST.ForAll -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp

-- --     AST.Exists -> do  check <- mapM (createQuant True  opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp

-- --     _          -> do  check <- mapM (createQuant False opQ name pos termExp) rangeExp
-- --                       joinRange opQ check pos tyExp


-- -- createExpression (AST.QuantRanUn opQ varQ pos rangeExp termExp t) = do
-- --   let name  = TE.unpack varQ
-- --   let tyExp = AST.qVarType termExp

-- --   ranges  <- doRange opQ rangeExp pos
-- --   rangesF <- makeRanges ranges

-- --   case opQ of
-- --     AST.ForAll -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp

-- --     AST.Exists -> do  check <- mapM (createQuant' True  opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp

-- --     _          -> do  check <- mapM (createQuant' False opQ name pos termExp) rangesF
-- --                       joinRange opQ check pos tyExp


-- genExpGuards :: [AST] -> Name -> Name -> LLVM [(Operand, Name)]
-- genExpGuards [guard] none one  = do
--   r <- genExpGuard guard none
--   return [r]

-- genExpGuards (guard:xs) none one = do
--   next <- newLabel
--   r    <- genExpGuard guard next
--   setLabel next $ branch one
--   rl   <- genExpGuards xs none one
--   return $ r:rl


-- createGuardExp :: AST -> Name -> LLVM (Operand, Name)
-- {- Ya estaba comentado (?)-}
-- --createGuardExp (AST.Cond lguards _ rtype) _ = do
-- --    final  <- newLabel
-- --    none   <- newLabel
-- --    lnames <- genExpGuards lguards none final
-- --    let rtype' = toType rtype
-- --    setLabel none $ branch final
-- --    setLabel final $ Do $ Unreachable []
-- --    n <- addUnNamedInstruction rtype' $ Phi rtype' lnames []
-- --    return (n, final)

-- createGuardExp acc code = do
--   exp   <- createExpression acc
--   label <- use condName
--   return (exp, label)


-- genExpGuard :: AST -> Name -> LLVM (Operand, Name)
-- genExpGuard (AST _ _ _ (Guard guard acc)) next = do
--   tag  <- createExpression guard
--   code <- newLabel
--   condName .= code
--   setLabel code $ condBranch tag code next
--   createGuardExp acc code

-- checkDivZero :: BinaryOperator -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
-- checkDivZero op pos lexp' rexp' ty = do
--   next  <- newLabel
--   abort <- newLabel
--   condName .= next

--   case ty of
--     T.GInt   -> do
--       let zero = constantInt 0
--       check <- addUnNamedInstruction intType $ ICmp IL.EQ rexp' zero []
--       setLabel abort $ condBranch check abort next
--       createTagZero next pos
--       addUnNamedInstruction intType $ irArithmetic op ty lexp' rexp'

--     T.GFloat -> do
--       let zero = constantFloat 0.0
--       check <- addUnNamedInstruction floatType $ FCmp FL.OEQ rexp' zero []
--       setLabel abort $ condBranch check abort next
--       createTagZero next pos
--       addUnNamedInstruction floatType $ irArithmetic op ty lexp' rexp'


-- checkOverflow :: BinaryOperator -> SourcePos -> Operand -> Operand -> T.Type -> LLVM Operand
-- checkOverflow op pos lexp rexp ty = do
--   overAbort <- newLabel
--   next      <- newLabel
--   res       <- addUnNamedInstruction (toType ty) $ irArithmetic op ty lexp rexp
--   check     <- extracValue res 1

--   setLabel overAbort $ condBranch check overAbort next
--   createTagOverflow next pos

--   condName .= next
--   extracValue res 0

-- intToDouble :: Operand -> LLVM Operand
-- intToDouble x = addUnNamedInstruction floatType $ _toFloat x


-- doubleToInt :: Operand -> LLVM Operand
-- doubleToInt x = addUnNamedInstruction intType $ _toInt x


-- irArithmetic :: BinaryOperator -> T.Type -> Operand -> Operand -> Instruction
-- --irArithmetic AST.Sum T.GInt   a b = _add  a b
-- --irArithmetic AST.Sub T.GInt   a b = Sub False False a b []
-- --irArithmetic AST.Mul T.GInt   a b = _mul a b
-- irArithmetic Sum T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intAdd))) [(a, []),(b, [])] [] []
-- irArithmetic Sub T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intSub))) [(a, []),(b, [])] [] []
-- irArithmetic Mul T.GInt   a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                                     (Name intMul))) [(a, []),(b, [])] [] []
-- irArithmetic Sum T.GFloat a b = _addF   a b
-- irArithmetic Mul T.GFloat a b = _mulF   a b
-- irArithmetic Sub T.GFloat a b = FSub NoFastMathFlags a b []
-- irArithmetic Div T.GInt   a b = SDiv True a b []
-- irArithmetic Div T.GFloat a b = FDiv NoFastMathFlags a b []
-- irArithmetic Mod T.GInt   a b = URem a b []
-- irArithmetic Mod T.GFloat a b = FRem NoFastMathFlags a b []
-- irArithmetic Exp T.GFloat a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                     (Name powString)))    [(a, []),(b, [])] [] []
-- irArithmetic Min T.GFloat a b = _minF a b
-- irArithmetic Max T.GFloat a b = _maxF a b
-- irArithmetic Max T.GInt   a b = _max  a b
-- irArithmetic Min T.GInt   a b = _min  a b


-- irBoolean :: BinaryOperator -> Operand -> Operand -> Instruction
-- irBoolean And a b = _and a b
-- irBoolean Or a b = _or  a b


-- irRelational :: BinaryOperator -> T.Type -> Operand -> Operand -> Instruction
-- irRelational AEQ  T.GFloat a b = FCmp FL.OEQ a b []
-- irRelational LT   T.GFloat a b = FCmp FL.OLT a b []
-- irRelational GT   T.GFloat a b = FCmp FL.OGT a b []
-- irRelational LE   T.GFloat a b = FCmp FL.OLE a b []
-- irRelational GE   T.GFloat a b = FCmp FL.OGE a b []
-- irRelational ANE  T.GFloat a b = FCmp FL.ONE a b []


-- irRelational AEQ  T.GInt   a b = ICmp IL.EQ  a b []
-- irRelational LT   T.GInt   a b = _less   a b
-- irRelational GT   T.GInt   a b = ICmp IL.SGT a b []
-- irRelational LE   T.GInt   a b = _lequal a b
-- irRelational GE   T.GInt   a b = ICmp IL.SGE a b []
-- irRelational ANE  T.GInt   a b = ICmp IL.NE  a b []


-- irConversion :: Conversion -> T.Type -> Operand -> Instruction
-- irConversion ToInt    T.GFloat a = _toInt   a
-- irConversion ToInt    T.GBoolean
--           (ConstantOperand (C.Int 1 0)) = _toInt $ constantInt 0
-- irConversion ToInt    T.GBoolean
--           (ConstantOperand (C.Int 1 1)) = _toInt $ constantInt 1
-- irConversion ToDouble T.GInt   a = _toFloat a
-- irConversion ToDouble T.GChar  a = _toFloat a
-- irConversion ToChar   T.GInt   a = Trunc  a charType  []
-- irConversion ToChar   T.GFloat a = FPToSI a charType  []


-- irUnary :: UnaryOperator -> T.Type -> Operand -> Instruction
-- irUnary Minus T.GInt   a = Sub False False      (constantInt 0) a []
-- irUnary Minus T.GFloat a = FSub NoFastMathFlags (constantFloat 0) a []
-- irUnary Abs   T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                      (Name fabsString))) [(a, [])] [] []
-- irUnary Sqrt  T.GFloat a = Call Nothing CC.C [] (Right ( definedFunction floatType
--                                      (Name sqrtString))) [(a, [])] [] []
-- irUnary Not   T.GBoolean  a = _not a


-- _and    a b = And a b []
-- _not    a   = Xor a (constantBool 1) []
-- _or     a b = Or  a b []
-- _less   a b = ICmp IL.SLT a b []
-- _lequal a b = ICmp IL.SLE a b []
-- _add    a b = Add False False a b []
-- _addF   a b = FAdd NoFastMathFlags a b []
-- _mul    a b = Mul False False a b []
-- _mulF   a b = FMul NoFastMathFlags a b []
-- _min    a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                          (Name minnumString)))  [(a, []),(b, [])] [] []
-- _minF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                          (Name minnumFstring))) [(a, []),(b, [])] [] []
-- _max    a b = Call Nothing CC.C [] (Right ( definedFunction intType
--                          (Name maxnumString)))  [(a, []),(b, [])] [] []
-- _maxF   a b = Call Nothing CC.C [] (Right ( definedFunction floatType
--                          (Name maxnumFtring)))  [(a, []),(b, [])] [] []
-- _toFloat a = SIToFP a floatType []
-- _toInt   a = FPToSI a intType   []
