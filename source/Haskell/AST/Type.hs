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

module  AST.Type
  ( ArgMode (..)
  , Type' (..)
  , TypeArgs'
  , (=:=)
  , fillType
  , isTypeVar
  , isDataType
  ) where
--------------------------------------------------------------------------------
import           Data.Array     (Array (..), bounds, (!))
import           Data.Foldable  (toList)
import           Data.Int       (Int32)
import           Data.Ix        (inRange)
import           Data.List      (intercalate, nub)
import           Data.Map       (Map)
import           Data.Map       as Map (elems)
import           Data.Monoid    (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Sequence  (Seq)
import qualified Data.Sequence  as Seq (zipWith)
import           Data.Text      (Text, pack, takeWhile, unpack)
import           Prelude        hiding (takeWhile)
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

type TypeArgs' e = Array Int (Type' e)

-- | Graciela Types. Special types for polymorphism are also included.
data Type' e
  = GUndef                        -- ^ Undefined type, for error propagation.
  | GSet      (Type' e)           -- ^ Set type.
  | GMultiset (Type' e)           -- ^ Multiset (bag) type.
  | GSeq      (Type' e)           -- ^ Sequence (ordered set) type.
  | GFunc     (Type' e) (Type' e) -- ^ Func type, for abstract functions.
  | GRel      (Type' e) (Type' e) -- ^ Relation type.
  | GTuple    (Seq (Type' e))     -- ^ N-tuple type.
  | GTypeVar  Int Text            -- ^ A named type variable.

  | GAny                          -- ^ Any type, for full polymorphism.
  | GOneOf    [Type' e]           -- ^ Any type within a collection, for
                                  -- restricted polymorphism
  | GUnsafeName Text              -- ^ A named type, only used for error messages.

  | GInt    -- ^ Basic integer type.
  | GFloat  -- ^ Basic floating-point number type.
  | GBool   -- ^ Basic boolean type.
  | GChar   -- ^ Basic character type.

  | GString -- ^ Basic string type.

  | GFullDataType
    { typeName :: Text
    , types    :: TypeArgs' e }
  | GDataType
    { typeName :: Text
    , abstName :: Maybe Text}
  | GPointer (Type' e) -- ^ Pointer type.

  | GArray
    { dimensions :: Seq (Either Text e)
    , innerType  :: Type' e
    } -- ^ Sized array type.
  deriving (Eq, Ord)


fillType :: TypeArgs' e -> Type' e -> Type' e
fillType typeArgs t@(GTypeVar i _) =
  if inRange (bounds typeArgs) i
    then typeArgs ! i
    else t
fillType typeArgs (GArray s t) =
    GArray s (fillType typeArgs t)

fillType typeArgs (GFullDataType n as) =
  GFullDataType n (fillType typeArgs <$> as)

fillType typeArgs (GSet t) = GSet (fillType typeArgs t)
fillType typeArgs (GMultiset t) = GMultiset (fillType typeArgs t)
fillType typeArgs (GSeq t) = GSeq (fillType typeArgs t)
fillType typeArgs (GFunc t1 t2) =
   GFunc (fillType typeArgs t1) (fillType typeArgs t2)
fillType typeArgs (GRel t1 t2) =
   GRel (fillType typeArgs t1) (fillType typeArgs t2)
fillType typeArgs (GTuple ts) = GTuple (fmap (fillType typeArgs) ts)

fillType _ t = t


isTypeVar t = case t of
  GTypeVar _ _ -> True
  _            -> False

isDataType t = case t of
  GFullDataType _ _ -> True
  GDataType _ _ -> True
  _ -> False

-- | Operator for checking whether two types match.
(=:=) :: Eq e => Type' e -> Type' e -> Bool
a =:= b = a <> b /= GUndef


-- | Graciela Types form a Monoid under the `more specific` operator,
-- with the type @GAny@ as the identity.

instance Eq e => Monoid (Type' e) where
  mempty = GAny
  mappend = (<>)
instance Eq e => Semigroup (Type' e) where
  a <> b | a == b = a
  GAny        <> a           = a
  a           <> GAny        = a

  GOneOf as   <> GOneOf bs   = case as `merge` bs of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      as `merge` bs = nub [ c | a <- as, b <- bs, let c = a <> b, c /= GUndef ]
  GOneOf as   <> a           = case a `matchIn` as of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      a `matchIn` as = nub [ c | b <- as, let c = a <> b, c /= GUndef ]
  a           <> GOneOf as   = case a `matchIn` as of
    []  -> GUndef
    [c] -> c
    cs  -> GOneOf cs
    where
      a `matchIn` as = nub [ c | b <- as, let c = a <> b, c /= GUndef ]

  GUndef      <> a           = GUndef
  a           <> GUndef      = a

  GSet a      <> GSet b      = case a <> b of
    GUndef -> GUndef
    c      -> GSet c
  GMultiset a <> GMultiset b = case a <> b of
    GUndef -> GUndef
    c      -> GMultiset c
  GSeq a      <> GSeq b      = case a <> b of
    GUndef -> GUndef
    c      -> GSeq c
  GPointer a  <> GPointer b  = case a <> b of
    GUndef -> GUndef
    c      -> GPointer c

  GArray s a  <> GArray t b    -- To match, the dimensions must be exactly the same.
    | length s == length t && and (Seq.zipWith (==~) s t) = case a <> b of
      GUndef -> GUndef
      c      -> GArray s c
    | otherwise = GUndef
    where
      Right x ==~ Right y = x == y
      _       ==~ _       = True

  GFunc a c   <> GFunc b d   = case (a <> b, c <> d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GFunc e f
  GRel a c    <> GRel b d    = case (a <> b, c <> d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GRel e f

  GTuple as   <> GTuple bs   = if length as == length bs
    then let cs = Seq.zipWith (<>) as bs
      in if GUndef `elem` cs
        then GUndef
        else GTuple cs
    else GUndef

  a@(GTypeVar ai an) <> GTypeVar bi bn =
    if ai == bi && an == bn then a else GUndef

  GFullDataType a fs <> GFullDataType b fs' =
    if a == b  && fs == fs'
      then GFullDataType a fs
      else GUndef

  t@(GDataType a (Just a')) <> GDataType b b' =
    if a == b || a' == b
      then t
      else GUndef

  GDataType a a' <> t@(GDataType b (Just b')) =
    if a == b  || a == b'
      then t
      else GUndef

  t@(GDataType a a') <> GDataType b _ =
    if a == b
      then t
      else GUndef

  GDataType a _ <> GFullDataType b fs =
    if a == b then GFullDataType b fs else GUndef

  GFullDataType a fs <> GDataType b _=
    if a == b then GFullDataType a fs else GUndef

  GUnsafeName a <> GUnsafeName b =
    if a == b then GUnsafeName a else GUndef

  _ <> _ = GUndef


instance Show (Type' e) where
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
        GArray    ds t  -> show (length ds) <> "-D array of " <> show' t
        GSet      t     -> "set of " <> show' t
        GMultiset t     -> "multiset of " <> show' t
        GSeq      t     -> "sequence of " <> show' t
        GFunc     ta tb -> "function " <> show' ta <> " -> " <> show' tb
        GRel      ta tb -> "relation " <> show' ta <> " -> " <> show' tb

        GTuple    ts    ->
          "tuple (" <> (unwords . toList $ show' <$> ts) <> ")"
        GTypeVar  i n   -> "`" <> unpack n <> "`" -- "#" <> show i <> " ("

        GFullDataType n targs   ->
          unpack n <> " (" <> unwords (fmap show' (toList targs)) <> ")"

        GDataType n _ -> unpack n

        GAny            -> "any type"
        GOneOf       as -> "one of " <> show as

        GUnsafeName t     -> unpack t
