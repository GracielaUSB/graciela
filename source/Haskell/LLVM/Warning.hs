module LLVM.Warning
  ( Warning (..)
  , warn
  , warnString
  ) where
--------------------------------------------------------------------------------
import           LLVM.Abort (waCall)
import           LLVM.Monad (LLVM)
import           Location   (SourcePos)
--------------------------------------------------------------------------------

-- | Graciela-lib for the warn function name.
warnString :: String
warnString = "_warn"
--------------------------------------------------------------------------------

-- | Enum type for the different warning conditions.
data Warning
  = Manual      -- ^ A `warn` instruction was manually called.
  | Pre         -- ^ A precondition failed.
  | Post        -- ^ A postcondition failed after its precondition failed as well.
  | Invariant
  | RepInvariant
  -- | Forall      -- ^ A universal quantifier returned false.
  -- | Existential -- ^ An existential quantifier returned false.
  deriving (Eq, Ord, Show, Enum)

-- | Generates a call to the appropriate warning.
warn :: Warning -> SourcePos -> LLVM ()
warn = waCall warnString . fromIntegral . fromEnum
