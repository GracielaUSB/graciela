{-|
Module      : Language.Graciela.Type
Description : Graciela typesystem
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Implements the Graciela typesystem.
-}

{-# LANGUAGE LambdaCase #-}

module Type
  ( ArgMode (..)
  , Type (..)
  , (=:=)
  ) where
--------------------------------------------------------------------------------
import           Data.Int    (Int32)
import           Data.List   (intercalate, nub)
import           Data.Monoid ((<>))
import           Data.Text   (Text, pack, takeWhile, unpack)
import           Prelude     hiding (takeWhile)
--------------------------------------------------------------------------------

-- | The mode in which an argument is passed to a graciela procedure.
data ArgMode
  = In    -- ^ Input argument.
  | Out   -- ^ Output argument.
  | InOut -- ^ Input/Output argument.
  | Ref   -- ^ Pass-by-reference argument.
  deriving (Eq)

-- | 'Show' instance for Argument modes.
instance Show ArgMode where
  show = \case
    In    -> "In"
    Out   -> "Out"
    InOut -> "In/Out"
    Ref   -> "Ref"

-- | Graciela Types. Special types for polymorphism are also included.
data Type
  = GUndef              -- ^ Undefined type, for error propagation.
  | GSet      Type      -- ^ Set type.
  | GMultiset Type      -- ^ Multiset (bag) type.
  | GSeq      Type      -- ^ Sequence (ordered set) type.
  | GFunc     Type Type -- ^ Func type, for abstract functions.
  | GRel      Type Type -- ^ Relation type.
  | GTuple   [Type]     -- ^ N-tuple type.
  | GTypeVar  Text      -- ^ A named type variable.

  | GAny                -- ^ Any type, for full polymorphism.
  | GOneOf     [Type]   -- ^ Any type within a collection, for
                        -- restricted polymorphism
  | GUnsafeName Text    -- ^ A named type, only used for error messages.

  | GInt    -- ^ Basic integer type.
  | GFloat  -- ^ Basic floating-point number type.
  | GBool   -- ^ Basic boolean type.
  | GChar   -- ^ Basic character type.

  | GString -- ^ Basic string type.

  | GFullDataType
    { typeName ::  Text
    , types    :: [Type]}
  | GDataType
    { typeName ::  Text
    , types    :: [Type]}
  | GPointer Type -- ^ Pointer type.

  | GArray
    { size      :: Int32
    , innerType :: Type
    } -- ^ Sized array type.
  deriving (Eq, Ord)


-- | Operator for checking whether two types match.
(=:=) :: Type -> Type -> Bool
a =:= b = (a <> b) /= GUndef


-- | Graciela Types form a Monoid under the `more specific` operator,
-- with the type @GAny@ as the identity.
instance Monoid Type where
  mempty = GAny
  a `mappend` b | a == b = a
  GAny        `mappend` a           = a
  a           `mappend` GAny        = a

  GOneOf as   `mappend` GOneOf bs   = case as `merge` bs of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      as `merge` bs = nub [ c | a <- as, b <- bs, let c = a `mappend` b, c /= GUndef ]
  GOneOf as   `mappend` a           = case a `matchIn` as of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      a `matchIn` as = nub [ c | b <- as, let c = a `mappend` b, c /= GUndef ]
  a           `mappend` GOneOf as   = case a `matchIn` as of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      a `matchIn` as = nub [ c | b <- as, let c = a `mappend` b, c /= GUndef ]

  GUndef      `mappend` a           = GUndef
  a           `mappend` GUndef      = a

  GSet a      `mappend` GSet b      = case a `mappend` b of
    GUndef -> GUndef
    c      -> GSet c
  GMultiset a `mappend` GMultiset b = case a `mappend` b of
    GUndef -> GUndef
    c      -> GMultiset c
  GSeq a      `mappend` GSeq b      = case a `mappend` b of
    GUndef -> GUndef
    c      -> GSeq c
  GPointer a  `mappend` GPointer b  = case a `mappend` b of
    GUndef -> GUndef
    c      -> GPointer c

  GArray s a  `mappend` GArray t b
    | s /= t = GUndef
    | s == t = case a `mappend` b of
      GUndef -> GUndef
      c      -> GArray (s `min` t) c

  GFunc a c   `mappend` GFunc b d   = case (a `mappend` b, c `mappend` d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GFunc e f
  GRel a c    `mappend` GRel b d    = case (a `mappend` b, c `mappend` d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GRel e f

  GTuple as   `mappend` GTuple bs   = if length as == length bs
    then let cs = zipWith mappend as bs
      in if GUndef `elem` cs
        then GUndef
        else GTuple cs
    else GUndef

  GTypeVar a `mappend` GTypeVar b =
    if a == b then GTypeVar a else GUndef

  GFullDataType a fs `mappend` GFullDataType b _ =
    if a == b then GFullDataType a fs else GUndef

  GDataType a fs `mappend` GDataType b _ =
    if a == b then GFullDataType a fs else GUndef

  GDataType a fs `mappend` GFullDataType b _ =
    if a == takeWhile (/= '-') b then GFullDataType b fs else GUndef

  GFullDataType a fs `mappend` GDataType b _ =
    if takeWhile (/= '-') a == b then GFullDataType a fs else GUndef

  GUnsafeName a `mappend` GUnsafeName b =
    if a == b then GUnsafeName a else GUndef

  _ `mappend` _ = GUndef

instance Show Type where
  show t' = "\ESC[0;32m" <> show' t' <> "\ESC[m"
    where
      show' = \case
        GUndef          -> "\ESC[0;31m" <> "undefined" <> "\ESC[0;32m"
        GInt            -> "int"
        GFloat          -> "double"
        GBool           -> "boolean"
        GChar           -> "char"
        GString         -> "string"
        GPointer     t  -> "pointer to " <> show' t
        GArray    s  t  -> "array[" <> show s <> "] of " <> show' t
        GSet      t     -> "set of " <> show' t
        GMultiset t     -> "multiset of " <> show' t
        GSeq      t     -> "sequence of " <> show' t
        GFunc     ta tb -> "function " <> show' ta <> " -> " <> show' tb
        GRel      ta tb -> "relation " <> show' ta <> " -> " <> show' tb

        GTuple    ts    ->
          "tuple (" <> (unwords . fmap show' $ ts) <> ")"
        GTypeVar  n     -> unpack n
        
        GFullDataType n f   -> 
          "data type " <> unpack n <> " " <> (intercalate " " $ fmap show' f)
        
        GDataType n f   -> 
          "data type " <> unpack n <> " " <> (intercalate " " $ fmap show' f)

        GAny            -> "any type"
        GOneOf       as -> "one of " <> show as

        GUnsafeName t     -> unpack t


