{-|
Module      : Language.Graciela.LLVM.Abort
Description : Graciela-lib "System"-call tags.
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

These functions generate code for different situations which might trigger
the program to stop or to warn the user.
-}

{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}

module Language.Graciela.LLVM.Abort
  ( Abort (..)
  , abort
  , abortString
  , waCall
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.Location
--------------------------------------------------------------------------------
import qualified Data.Sequence                      as Seq (singleton)
import           LLVM.General.AST
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import           LLVM.General.AST.Type              (Type (VoidType))
--------------------------------------------------------------------------------

abortString :: String
-- | Graciela-lib for the abort function name.
abortString = "_abort"
--------------------------------------------------------------------------------

-- | Used to build the args for an abort or a warning.
args :: Integer -> SourcePos -> [(Operand, [ParameterAttribute])]
args i pos =
  [ (ConstantOperand $ C.Int 32 i, [])
  , posConstant . sourceLine  $ pos
  , posConstant . sourceColumn $ pos
  ]
  where
    posConstant :: Pos -> (Operand, [a])
    posConstant = (,[]) . ConstantOperand . C.Int 32 . fromIntegral . unPos

waCall :: String -> Integer -> SourcePos -> LLVM ()
waCall func i pos = addInstruction $ Do Call
  { tailCallKind        = Nothing
  , callingConvention   = CC.C
  , returnAttributes    = []
  , function            = callable voidType func
  , arguments           = args i pos
  , functionAttributes  = []
  , metadata            = [] }
--------------------------------------------------------------------------------

-- | Enum type for the different abort conditions.
data Abort
  = If                 -- ^ A conditional had no true guard.
  | Manual             -- ^ An `abort` instruction was manually called.
  | Post               -- ^ A postcondition failed.
  | Assert             -- ^ An assertion failed.
  | Invariant          -- ^ An invariant failed.
  | NonDecreasingBound -- ^ A bound didn't decrease between iterations or recursion.
  | NegativeBound      -- ^ A bound function took a negative value.
  | DivisionByZero     -- ^ A division by zero was attempted.
  | Overflow           -- ^ A value overflowed.
  | Underflow          -- ^ A value underflowed.
  | EmptyRange         -- ^ A quantification disallowing empty ranges received one.
  | NullPointerAccess  -- ^ A null pointer was dereferenced.
  | RepInvariant       -- ^ The representation invariant was falsified.
  | NegativeIndex      -- ^ A negative index was used to access an array.
  | OutOfBoundsIndex   -- ^ An index outside the bounds of an array was used to access it.
  | BadArrayArg        -- ^ A procedure/function was declared with an array of a given size,
                       -- but a different one was used at callsite. This check cannot always
                       -- be done at compile time.
  | NegativeRoot       -- The function sqrt was called with a negative argument.
  | NegativeExponent   -- The integer operation ^ was performed with a negative exponent.
  | BadAbstractCouple  -- If the precondition do not fails, but the abstract pre fails, then its a bad couple
  | CoupInvariant
  | AbstractPost
  deriving (Eq, Ord, Show, Enum)

-- | Generates a call to the appropriate abort.
abort :: Abort -> SourcePos -> LLVM ()
abort reason pos = do
  waCall abortString (fromIntegral . fromEnum $ reason) pos
  terminate $ Unreachable []
