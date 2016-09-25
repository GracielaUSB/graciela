{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Parser.Operator
  ( Un (..)
  , Bin (..)
  , Bin' (..)
  -- * unary operators
  , uMinus, not
  -- * arithmetic operators
  , power, times, div, mod, plus, bMinus, max, min
  -- * boolean operators
  , or, and, implies, consequent, beq
  -- * comparison operators
  , bne, lt, le, gt, ge, aeq, ane
  -- * membership operators
  , elem, notElem
  -- * functor operators
  , intersect, difference, union
  -- * functor relation operators
  , ssubset, ssuperset, subset, superset
  -- * multiset operators
  , multisum
  -- * sequence access operator
  , seqAt
  -- * bifunctor access operator
  , bifuncAt
  -- * sequence concatenation operator
  , concat
  ) where
--------------------------------------------------------------------------------
import           AST.Expression (BinaryOperator (..), Expression' (..),
                                 Expression'' (Binary, Unary, Value),
                                 UnaryOperator (..), Value (..))
import           AST.Type       (Expression, Type (..), basic, (=:=))
import           Error          (internal)
--------------------------------------------------------------------------------
import           Data.Char      (chr, ord)
import qualified Data.Fixed     as F (mod')
import           Data.Int       (Int32)
import           Data.Semigroup ((<>))
import           Prelude        hiding (Ordering (..), and, concat, div, elem,
                                 max, min, mod, not, notElem, or)
import qualified Prelude        as P (div, max, min, mod, not, or)

import           Debug.Trace
--------------------------------------------------------------------------------
chr' :: Int32 -> Char
chr' = chr . fromIntegral . (`P.mod` 256)
ord' :: Char -> Int32
ord' = fromIntegral . ord

--------------------------------------------------------------------------------

type UnaryOpType = Type -> Either String Type

data Un = Un
  { unSymbol :: UnaryOperator
  , unType   :: UnaryOpType
  , unFunc   :: Value -> Value }

type BinaryOpType = Type -> Type -> Either String Type

data Bin = Bin
  { binSymbol :: BinaryOperator
  , binType   :: BinaryOpType
  , binFunc   :: Value -> Value -> Value }

data Bin' = Bin'
  { binSymbol' :: BinaryOperator
  , binType'   :: BinaryOpType
  , binFunc'   :: Expression -> Expression -> Expression }

--------------------------------------------------------------------------------
arithU :: Integral a
       => (Int32 -> Int32)
       -> (Double -> Double)
       -> (Value -> Value)
arithU fi ff = f
  where
    f (IntV   v) = IntV   .                        fi        $ v
    f (FloatV v) = FloatV .                        ff        $ v
    f _          = internal "bad arithUn precalc"


arithUnType :: UnaryOpType
arithUnType GInt   = Right GInt
arithUnType GFloat = Right GFloat
arithUnType _      = Left $
  show GInt   <> ", " <>
  show GFloat

uMinus :: Un
uMinus = Un UMinus arithUnType $ arithU negate negate

--------------------------------------------------------------------------------
boolU :: (Bool -> Bool)
      -> (Value -> Value)
boolU fb = f
  where
    f (BoolV v) = BoolV . fb $ v
    f _         = internal "bad boolUn precalc"

boolUnType :: UnaryOpType
boolUnType GBool = Right GBool
boolUnType _     = Left $ show GBool

not :: Un
not = Un Not boolUnType $ boolU P.not

--------------------------------------------------------------------------------
arith :: (Int32 -> Int32 -> Int32)
      -> (Double -> Double -> Double)
      -> (Value -> Value -> Value)
arith fi ff = f
  where
    f (IntV   v) (IntV   w) = IntV (v `fi` w)
    f (CharV  v) (CharV  w) = CharV . chr' $ (ord' v `fi` ord' w)
    f (FloatV v) (FloatV w) = FloatV (v `ff` w)
    f _          _          = internal "bad arithOp precalc"

arithOpType :: BinaryOpType
arithOpType GInt   GInt   = Right GInt
arithOpType GChar  GChar  = Right GChar
arithOpType GFloat GFloat = Right GFloat
arithOpType _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

power, times, plus, bMinus, max, min :: Bin
power  = Bin Power  arithOpType $ arith (^) (**)
times  = Bin Times  arithOpType $ arith (*) (*)
plus   = Bin Plus   arithOpType $ arith (+) (+)
bMinus = Bin BMinus arithOpType $ arith (-) (-)
max    = Bin Max    arithOpType $ arith P.max P.max
min    = Bin Min    arithOpType $ arith P.min P.min

div, mod :: Bin'
div = Bin' Div arithOpType $ fraction Div P.div (/)
mod = Bin' Mod arithOpType $ fraction Mod P.mod F.mod'

fraction :: BinaryOperator
         -> (Int32 -> Int32 -> Int32)
         -> (Double -> Double -> Double)
         -> Expression -> Expression -> Expression
fraction op f g
  l@Expression { exp' = lexp, expConst = lc, expType }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (IntV m), Value (IntV n))
        | n /= 0 -> Value (IntV $ m `f` n)
      (Value (CharV m), Value (CharV n))
        | n /= '\0' -> Value . CharV . chr' $ ord' m `f` ord' n
      (Value (FloatV m), Value (FloatV n)) -> Value (FloatV $ m `g` n)
      _ -> Binary op l r
  in Expression { loc = loc l <> loc r, expConst = lc && rc, expType, exp'}

--------------------------------------------------------------------------------
boolOpType :: BinaryOpType
boolOpType GBool GBool = Right GBool
boolOpType _     _     = Left $
  show (GBool, GBool)

or, and, implies, consequent, beq, bne :: Bin'
or         = Bin' Or         boolOpType or'
and        = Bin' And        boolOpType and'
implies    = Bin' Implies    boolOpType implies'
consequent = Bin' Consequent boolOpType consequent'
beq        = Bin' BEQ        boolOpType beq'
bne        = Bin' BNE        boolOpType bne'

or', and', implies', consequent', beq', bne'
  :: Expression -> Expression -> Expression
or'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> Value (BoolV True) -- true  \/ y === true
      (Value (BoolV False), _) -> rexp               -- false \/ y ===  y
      (_, Value (BoolV True) ) -> Value (BoolV True) -- x \/ true  === true
      (_, Value (BoolV False)) -> lexp               -- x \/ false ===  x
      (_,_) -> Binary Or l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

and'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> rexp                -- true  /\ y ===  y
      (Value (BoolV False), _) -> Value (BoolV False) -- false /\ y === false
      (_, Value (BoolV True) ) -> lexp                -- x /\ true  ===  x
      (_, Value (BoolV False)) -> Value (BoolV False) -- x /\ false === false
      (_,_) -> Binary And l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

implies'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> rexp               -- true  ==> y ===  y
      (Value (BoolV False), _) -> Value (BoolV True) -- false ==> y === true
      (_, Value (BoolV True) ) -> Value (BoolV True) -- x ==> true  === true
      (_, Value (BoolV False)) -> Unary Not l        -- x ==> false === !x
      (_,_) -> Binary Implies l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

consequent'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (_, Value (BoolV True) ) -> lexp               -- x <== true  ===  x
      (_, Value (BoolV False)) -> Value (BoolV True) -- x <== false === true
      (Value (BoolV True),  _) -> Value (BoolV True) -- true  <== y === true
      (Value (BoolV False), _) -> Unary Not l        -- false <== y === !y
      (_,_) -> Binary Consequent l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

beq'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV v), Value (BoolV w)) ->
        Value (BoolV (v == w))
      (Value (BoolV True),  _) -> rexp        -- (true  === y) ===  y
      (Value (BoolV False), _) -> Unary Not r -- (false === y) === !y
      (_, Value (BoolV True) ) -> lexp        -- (x === true ) ===  x
      (_, Value (BoolV False)) -> Unary Not l -- (x === false) === !x
      (_,_) -> Binary BEQ l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

bne'
  l@Expression { exp' = lexp, expConst = lc }
  r@Expression { exp' = rexp, expConst = rc } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV v), Value (BoolV w)) ->
        Value (BoolV (v /= w))
      (Value (BoolV True),  _) -> Unary Not r -- (true  !== y) === !y
      (Value (BoolV False), _) -> rexp        -- (false !== y) ===  y
      (_, Value (BoolV True) ) -> Unary Not l -- (x !== true ) === !x
      (_, Value (BoolV False)) -> lexp        -- (x !== false) ===  x
      (_,_) -> Binary BNE l r
  in Expression { loc = loc l <> loc r, expType = GBool, expConst = lc && rc, exp'}

--------------------------------------------------------------------------------
comp :: (forall a. Ord a => a -> a -> Bool)
     -> (Value -> Value -> Value)
comp fc = f
  where
    f (IntV   v) (IntV   w) = BoolV $ v `fc` w
    f (CharV  v) (CharV  w) = BoolV $ v `fc` w
    f (FloatV v) (FloatV w) = BoolV $ v `fc` w
    f _          _          = internal "bad compOp precalc"

compOpType :: BinaryOpType
compOpType GInt   GInt   = Right GBool
compOpType GChar  GChar  = Right GBool
compOpType GFloat GFloat = Right GBool
compOpType t1@GTypeVar{} t2@GTypeVar{} | t1 =:= t2 = Right GBool
compOpType _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

{- Add support to pointer to AEQ and ANE -}
compOpType' :: BinaryOpType
compOpType' (GPointer t1) (GPointer t2) = if t1 =:= t2
  then Right GBool
  else Left $ show (GPointer t1, GPointer t1)

compOpType' GBool  GBool  = Right GBool
compOpType' GInt   GInt   = Right GBool
compOpType' GChar  GChar  = Right GBool
compOpType' GFloat GFloat = Right GBool

compOpType' t1@GTypeVar{} t2@GTypeVar{} = if t1 =:= t2
  then Right GBool
  else Left $ show (t1, t1)

compOpType' t@(GSet t1)      (GSet t2)      = if t1 =:= t2
  then Right GBool
  else Left $ show (t, t)
compOpType' t@(GSeq t1)      (GSeq t2)      = if t1 =:= t2
  then Right GBool
  else Left $ show (t, t)
compOpType' t@(GMultiset t1) (GMultiset t2) = if t1 =:= t2
  then Right GBool
  else Left $ show (t, t)
compOpType' t@(GFunc t1 t2)  (GFunc t3 t4)  = if t1 =:= t3 && t2 =:= t4
  then Right GBool
  else Left $ show (t, t)
compOpType' t@(GRel t1 t2)   (GRel t3 t4)   = if t1 =:= t3 && t2 =:= t4
  then Right GBool
  else Left $ show (t, t)

compOpType' t      _      = if t =:= basic
  then Left $
    show (GInt  , GInt  ) <> ", " <>
    show (GChar , GChar ) <> ", " <>
    show (GFloat, GFloat) <> ", " <>
    show (GBool , GBool ) <> ", or " <>
    show (GPointer (GTypeVar 0 "t"), GPointer (GTypeVar 0 "t"))
  else Left $ show (t,t)


lt, le, gt, ge, aeq, ane :: Bin
lt  = Bin LT  compOpType $ comp (<)
le  = Bin LE  compOpType $ comp (<=)
gt  = Bin GT  compOpType $ comp (>)
ge  = Bin GE  compOpType $ comp (>=)

aeq = Bin AEQ compOpType' $ comp (==)
ane = Bin ANE compOpType' $ comp (/=)

--------------------------------------------------------------------------------
elemPre :: (Value -> Value -> Value)
elemPre _ _ = internal "bad elem precalc"

elemType :: BinaryOpType
elemType t1 (GSet t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GSet t2)
elemType t1 (GMultiset t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GMultiset t2)
elemType t1 (GSeq t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GSeq t2)
elemType _ _ = Left $
  show (GUnsafeName "t", GSet      (GUnsafeName "t")) <> ", or " <>
  show (GUnsafeName "t", GMultiset (GUnsafeName "t")) <> ", or " <>
  show (GUnsafeName "t", GSeq      (GUnsafeName "t"))

elem, notElem :: Bin
elem    = Bin Elem    elemType elemPre
notElem = Bin NotElem elemType elemPre
--------------------------------------------------------------------------------

setSetPre :: (Value -> Value -> Value)
setSetPre _ _ = internal "bad set-set precalc"

setSetType :: BinaryOpType
setSetType t1@(GSet _) t2@(GSet _) = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  t3 -> Right t3
setSetType t1@(GMultiset _) t2@(GMultiset _) = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  t3 -> Right t3
setSetType _ _ = let t = GUnsafeName "t" in Left $
  show (GSet t, GSet t) <> ", or " <>
  show (GMultiset t, GMultiset t)

intersect, difference, union :: Bin
intersect  = Bin Intersection setSetType setSetPre
difference = Bin Difference   setSetType setSetPre
union      = Bin Union        setSetType setSetPre
--------------------------------------------------------------------------------

setRelPre :: (Value -> Value -> Value)
setRelPre _ _ = internal "bad set rel precalc"

setRelType :: BinaryOpType
setRelType t1@(GSet _) t2@(GSet _) = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  _ -> Right GBool
setRelType t1@(GMultiset _) t2@(GMultiset _) = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  _ -> Right GBool
setRelType _ _ = let t = GUnsafeName "t" in Left $
  show (GSet t, GSet t) <> ", or " <>
  show (GMultiset t, GMultiset t)

ssubset, ssuperset, subset, superset :: Bin
ssubset   = Bin SSubset   setRelType setRelPre
ssuperset = Bin SSuperset setRelType setRelPre
subset    = Bin Subset    setRelType setRelPre
superset  = Bin Superset  setRelType setRelPre
--------------------------------------------------------------------------------

multiPre :: (Value -> Value -> Value)
multiPre _ _ = internal "bad set-set precalc"

multiType :: BinaryOpType
multiType t1@(GMultiset _) t2@(GMultiset _) = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  t3 -> Right t3
multiType _ _ = let t = GUnsafeName "t" in Left $
  show (GMultiset t, GMultiset t)

multisum :: Bin
multisum  = Bin MultisetSum multiType multiPre
--------------------------------------------------------------------------------

seqAtPre :: (Value -> Value -> Value)
seqAtPre _ _ = internal "bad seqAt operator precalc"

seqAtType :: BinaryOpType
seqAtType (GSeq t1) GInt = Right t1
seqAtType _ _ = let t = GUnsafeName "t" in Left $
  show (GSeq t, GInt)

seqAt :: Bin
seqAt  = Bin SeqAt seqAtType seqAtPre
--------------------------------------------------------------------------------

bifuncAtPre :: (Value -> Value -> Value)
bifuncAtPre _ _ = internal "bad bifuncAt operator precalc"

bifuncAtType :: BinaryOpType
bifuncAtType (GFunc a b) c = case a <> c of
  GUndef -> Left $ show (GFunc a b, a)
  _ -> Right b
bifuncAtType (GRel a b) c = case a <> c of
  GUndef -> Left $ show (GFunc a b, a)
  _ -> Right $ GSet b
bifuncAtType _ _ = let [s, t] = GUnsafeName <$> ["s", "t"] in Left $
  show (GFunc s t, s) <> ", or " <>
  show (GRel  s t, s)

bifuncAt :: Bin
bifuncAt  = Bin SeqAt bifuncAtType bifuncAtPre
--------------------------------------------------------------------------------

concatPre :: (Value -> Value -> Value)
concatPre _ _ = internal "bad concat operator precalc"

concatType :: BinaryOpType
concatType t1@(GSeq a) t2 = case t1 <> t2 of
  GUndef -> Left $ show (t1, t1)
  t3 -> Right t3
concatType _ _ = let t = GUnsafeName "t" in Left $
  show (GSeq t, GSeq t)

concat :: Bin
concat  = Bin Concat concatType concatPre
