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

module LLVM.Abort
  ( Abort (..)
  , Warning (..)
  , abort
  , abortString
  , warn
  , warnString
  ) where
--------------------------------------------------------------------------------
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           Location
--------------------------------------------------------------------------------
import qualified Data.Sequence                      as Seq (singleton)
import           LLVM.General.AST
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import           LLVM.General.AST.Type              (Type (VoidType))
--------------------------------------------------------------------------------

abortString, warnString :: String
-- | Graciela-lib for the abort function name.
abortString = "_abort"
-- | Graciela-lib for the warn function name.
warnString = "_warn"
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
  | AManual            -- ^ An `abort` instruction was manually called.
  | Post               -- ^ A postcondition failed.
  | Assert             -- ^ An assertion failed.
  | Invariant          -- ^ An invariant failed.
  | NondecreasingBound -- ^ A bound didn't decrease between iterations or recursion.
  | NegativeBound      -- ^ A bound function took a negative value.
  | DivisionByZero     -- ^ A division by zero was attempted.
  | Overflow           -- ^ A value overflowed.
  | Underflow          -- ^ A value underflowed.
  | EmptyRange         -- ^ A quantification disallowing empty ranges received one.
  | NullPointerAccess  -- ^ A null pointer was dereferenced.
  | RepInvariant
  deriving (Eq, Ord, Show, Enum)

-- | Generates a call to the appropriate abort.
abort :: Abort -> SourcePos -> LLVM ()
abort reason pos = do
  waCall abortString (fromIntegral . fromEnum $ reason) pos
  terminate' $ Unreachable []
--------------------------------------------------------------------------------

-- | Enum type for the different warning conditions.
data Warning
  = WManual     -- ^ A `warn` instruction was manually called.
  | Pre         -- ^ A precondition failed.
  | Forall      -- ^ A universal quantifier returned false.
  | Existential -- ^ An existential quantifier returned false.
  deriving (Eq, Ord, Show, Enum)

-- | Generates a call to the appropriate warning.
warn :: Warning -> SourcePos -> LLVM ()
warn = waCall warnString . fromIntegral . fromEnum
