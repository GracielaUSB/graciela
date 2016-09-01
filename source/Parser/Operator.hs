{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Parser.Operator
  ( Un (..)
  , Bin (..)
  , Bin' (..)
  , uMinus, abs, sqrt, not, power, times, div, mod, plus
  , bMinus, max, min, or, and, implies, consequent, beq
  , bne, lt, le, gt, ge, aeq, ane, elem, notElem
  ) where
--------------------------------------------------------------------------------
import           AST.Expression (BinaryOperator (..), Expression (..),
                                 Expression' (Binary, Unary, Value),
                                 UnaryOperator (..), Value (..))
import           Type           (Type (..), (=:=))
--------------------------------------------------------------------------------
import           Data.Char      (chr, ord)
import qualified Data.Fixed     as F (mod')
import           Data.Int       (Int32)
import           Data.Monoid    ((<>))
import           Prelude        hiding (Ordering (..), abs, and, div, elem, max,
                                 min, mod, not, notElem, or, sqrt)
import qualified Prelude        as P (abs, div, max, min, mod, not, or, sqrt)

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
    f (CharV  v) = CharV  . chr' . fi . ord' $ v
    f (FloatV v) = FloatV .                        ff        $ v
    f _          = error "internal error: bad arithUn precalc"


arithUnType :: UnaryOpType
arithUnType GInt   = Right GInt
arithUnType GChar  = Right GChar
arithUnType GFloat = Right GFloat
arithUnType _      = Left $
  show GInt   <> ", " <>
  show GChar  <> ", or " <>
  show GFloat

uMinus, abs, sqrt :: Un
uMinus = Un UMinus arithUnType $ arithU negate negate
abs    = Un Abs    arithUnType $ arithU P.abs P.abs
sqrt   = Un Sqrt   arithUnType $ arithU (floor . P.sqrt. fromIntegral) P.sqrt

--------------------------------------------------------------------------------
boolU :: (Bool -> Bool)
      -> (Value -> Value)
boolU fb = f
  where
    f (BoolV v) = BoolV . fb $ v
    f _         = error "internal error: bad boolUn precalc"

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
    f _          _          = error "internal error: bad arithOp precalc"

arithOpType :: BinaryOpType
arithOpType GInt   GInt   = Right GInt
arithOpType GChar  GChar  = Right GChar
arithOpType GFloat GFloat = Right GFloat
arithOpType _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

power, times, div, mod, plus, bMinus, max, min :: Bin
power  = Bin Power  arithOpType $ arith (^) (**)
times  = Bin Times  arithOpType $ arith (*) (*)
div    = Bin Div    arithOpType $ arith P.div (/)
mod    = Bin Mod    arithOpType $ arith P.mod F.mod'
plus   = Bin Plus   arithOpType $ arith (+) (+)
bMinus = Bin BMinus arithOpType $ arith (-) (-)
max    = Bin Max    arithOpType $ arith P.max P.max
min    = Bin Min    arithOpType $ arith P.min P.min

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
or' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> Value (BoolV True) -- true  \/ y === true
      (Value (BoolV False), _) -> rexp               -- false \/ y ===  y
      (_, Value (BoolV True) ) -> Value (BoolV True) -- x \/ true  === true
      (_, Value (BoolV False)) -> lexp               -- x \/ false ===  x
      (_,_) -> Binary Or l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

and' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> rexp                -- true  /\ y ===  y
      (Value (BoolV False), _) -> Value (BoolV False) -- false /\ y === false
      (_, Value (BoolV True) ) -> lexp                -- x /\ true  ===  x
      (_, Value (BoolV False)) -> Value (BoolV False) -- x /\ false === false
      (_,_) -> Binary And l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

implies' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV True),  _) -> rexp               -- true  ==> y ===  y
      (Value (BoolV False), _) -> Value (BoolV True) -- false ==> y === true
      (_, Value (BoolV True) ) -> Value (BoolV True) -- x ==> true  === true
      (_, Value (BoolV False)) -> Unary Not l        -- x ==> false === !x
      (_,_) -> Binary Implies l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

consequent' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (_, Value (BoolV True) ) -> lexp               -- x <== true  ===  x
      (_, Value (BoolV False)) -> Value (BoolV True) -- x <== false === true
      (Value (BoolV True),  _) -> Value (BoolV True) -- true  <== y === true
      (Value (BoolV False), _) -> Unary Not l        -- false <== y === !y
      (_,_) -> Binary Consequent l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

beq' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV v), Value (BoolV w)) ->
        Value (BoolV (v == w))
      (Value (BoolV True),  _) -> rexp        -- (true  === y) ===  y
      (Value (BoolV False), _) -> Unary Not r -- (false === y) === !y
      (_, Value (BoolV True) ) -> lexp        -- (x === true ) ===  x
      (_, Value (BoolV False)) -> Unary Not l -- (x === false) === !x
      (_,_) -> Binary Implies l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

bne' l@Expression { exp' = lexp } r@Expression { exp' = rexp } =
  let
    exp' = case (lexp, rexp) of
      (Value (BoolV v), Value (BoolV w)) ->
        Value (BoolV (v /= w))
      (Value (BoolV True),  _) -> Unary Not r -- (true  !== y) === !y
      (Value (BoolV False), _) -> rexp        -- (false !== y) ===  y
      (_, Value (BoolV True) ) -> Unary Not l -- (x !== true ) === !x
      (_, Value (BoolV False)) -> lexp        -- (x !== false) ===  x
      (_,_) -> Binary Implies l r
  in Expression { loc = loc l <> loc r, expType = GBool, exp'}

--------------------------------------------------------------------------------
comp :: (forall a. Ord a => a -> a -> Bool)
     -> (Value -> Value -> Value)
comp fc = f
  where
    f (IntV   v) (IntV   w) = BoolV $ v `fc` w
    f (CharV  v) (CharV  w) = BoolV $ v `fc` w
    f (FloatV v) (FloatV w) = BoolV $ v `fc` w
    f _          _          = error "internal error: bad compOp precalc"

compOpType :: BinaryOpType
compOpType GInt   GInt   = Right GBool
compOpType GChar  GChar  = Right GBool
compOpType GFloat GFloat = Right GBool
compOpType (GPointer t1) (GPointer t2) = if t1 =:= t2 
  then Right GBool
  else Left $ show (GPointer t1,GPointer t1)
compOpType _      _      = Left $
  show (GInt  , GInt  ) <> ", " <>
  show (GChar , GChar ) <> ", or " <>
  show (GFloat, GFloat)

{- Add support to pointer to AEQ and ANE -}
compOpType' :: BinaryOpType
compOpType' (GPointer t1) (GPointer t2) = if t1 =:= t2 
  then Right GBool
  else Left $ show (GPointer t1,GPointer t1)
compOpType' a b = compOpType a b

lt, le, gt, ge, aeq, ane :: Bin
lt  = Bin LT  compOpType $ comp (<)
le  = Bin LE  compOpType $ comp (<=)
gt  = Bin GT  compOpType $ comp (>)
ge  = Bin GE  compOpType $ comp (>=)

aeq = Bin AEQ compOpType' $ comp (==)
ane = Bin ANE compOpType' $ comp (/=)

--------------------------------------------------------------------------------
elemPre :: (Value -> Value -> Value)
elemPre _ _ = error "internal error: bad elem precalc"

elemType :: BinaryOpType
elemType t1 (GSet t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GSet t2)
elemType t1 (GMultiset t2)
  | t1 =:= t2 = Right GBool
  | otherwise = Left $ show (t2, GMultiset t2)
elemType _ _ = Left $
  show (GUnsafeName "t", GSet      (GUnsafeName "t")) <> ", or " <>
  show (GUnsafeName "t", GMultiset (GUnsafeName "t"))

elem, notElem :: Bin
elem    = Bin Elem    elemType elemPre
notElem = Bin NotElem elemType elemPre
