{-|
Module      : Language.Graciela.Type
Description : Graciela typesystem
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Implements the Graciela typesystem.
-}

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module  AST.Type
  ( ArgMode (..)
  , Type (..)
  , TypeArgs
  , (=:=)
  , fillType
  , basic
  , highLevel
  , hasDT
  , hasTypeVar
  , notIn
  , objMode
  ) where
--------------------------------------------------------------------------------
import           AST.Expression (Expression (..), Expression' (..), Value (..))
import           AST.Object     (Object (..), Object' (..))
import qualified AST.Object     as O (inner)
import           Common
--------------------------------------------------------------------------------
import           Data.Array     (Array, bounds, (!))
import           Data.Ix        (inRange)
import           Data.List      (intercalate, nub)
import           Data.Sequence  (Seq)
import qualified Data.Sequence  as Seq (zipWith)
import           Data.Text      (Text)
import           Prelude        hiding (takeWhile)
--------------------------------------------------------------------------------

objMode :: Object -> Maybe ArgMode
objMode (Object _ _ Variable { mode }) = mode
objMode (Object _ _ o)                 = objMode (O.inner o)

notIn :: Object -> Bool
notIn obj = objMode obj /= Just In
-------------------------------------------------------------------------------------

-- | The mode in which an argument is passed to a graciela procedure.
data ArgMode
  = In    -- ^ Input argument.
  | Out   -- ^ Output argument.
  | InOut -- ^ Input/Output argument.
  | Ref   -- ^ Pass-by-reference argument.
  | Const
  deriving (Eq)

-- | 'Show' instance for Argument modes.
instance Show ArgMode where
  show = \case
    In    -> "In"
    Out   -> "Out"
    InOut -> "In/Out"
    Const -> "Const"
    Ref   -> "Ref"

type TypeArgs = Array Int Type

-- | Graciela Types. Special types for polymorphism are also included.
data Type
  = GUndef                        -- ^ Undefined type, for error propagation.
  | GSet      { innerType :: Type } -- ^ Set type.
  | GMultiset { innerType :: Type } -- ^ Multiset (bag) type.
  | GSeq      { innerType :: Type } -- ^ Sequence (ordered set) type.
  | GFunc     Type Type -- ^ Func type, for abstract functions.
  | GRel      Type Type -- ^ Relation type.
  | GTuple    Type Type -- ^ 2-tuple type.
  | GATuple             -- ^ Used to match with tuples.
  | GTypeVar  Int Text            -- ^ A named type variable.
  | GATypeVar                     -- ^ Used to match with type variables.

  | GAny                          -- ^ Any type, for full polymorphism.
  | GOneOf    [Type]              -- ^ Any type within a collection, for
                                  -- restricted polymorphism
  | GUnsafeName Text              -- ^ A named type, only used for error messages.

  | GInt    -- ^ Basic integer type.
  | GFloat  -- ^ Basic floating-point number type.
  | GBool   -- ^ Basic boolean type.
  | GChar   -- ^ Basic character type.

  | GString -- ^ Basic string type.

  | GFullDataType
    { typeName :: Text
    , typeArgs :: TypeArgs }
  | GDataType
    { typeName :: Text
    , abstName :: Maybe Text
    , typeArgs :: TypeArgs }
  | GADataType -- ^ Used to match with both kinds of Data Types

  | GPointer Type -- ^ Pointer type.

  | GArray
    { dimensions :: Seq Expression
    , innerType  :: Type
    } -- ^ Sized array type.

  | GRawName

  | I64 -- ^ Used for casts
  deriving (Eq)


fillType :: TypeArgs -> Type -> Type
fillType typeArgs t@(GTypeVar i _) =
  if inRange (bounds typeArgs) i
    then typeArgs ! i
    else t
fillType typeArgs (GArray s t) =
    GArray s (fillType typeArgs t)

fillType typeArgs (GPointer t) =
  GPointer (fillType typeArgs t)

fillType typeArgs (GFullDataType n as) =
  GFullDataType n (fillType typeArgs <$> as)

fillType typeArgs (GDataType n an as) =
  let 
    mk = if any (=:= GATypeVar) typeArgs
      then GDataType n an
      else GFullDataType n
  in mk (fillType typeArgs <$> as)

fillType typeArgs (GSet t) = GSet (fillType typeArgs t)
fillType typeArgs (GMultiset t) = GMultiset (fillType typeArgs t)
fillType typeArgs (GSeq t) = GSeq (fillType typeArgs t)
fillType typeArgs (GFunc t1 t2) =
   GFunc (fillType typeArgs t1) (fillType typeArgs t2)
fillType typeArgs (GRel t1 t2) =
   GRel (fillType typeArgs t1) (fillType typeArgs t2)
fillType typeArgs (GTuple a b) = GTuple (fillType typeArgs a) (fillType typeArgs b)

fillType _ t = t


-- isTypeVar t = case t of
--   GTypeVar _ _ -> True
--   _            -> False
--
-- isDataType t = case t of
--   GFullDataType {} -> True
--   GDataType {}     -> True
--   _                -> False

hasDT :: Type -> Maybe Type
hasDT t@GDataType {}     = Just t
hasDT t@GFullDataType {} = Just t
hasDT (GArray _ t)       = hasDT t
hasDT (GPointer t)       = hasDT t
hasDT _                  = Nothing

hasTypeVar :: Type -> Bool
hasTypeVar GTypeVar{}               = True
hasTypeVar GDataType {typeArgs}     = any hasTypeVar typeArgs
hasTypeVar GFullDataType {typeArgs} = any hasTypeVar typeArgs
hasTypeVar (GArray _ t)             = hasTypeVar t
hasTypeVar (GPointer t)             = hasTypeVar t
hasTypeVar _                        = False

basic :: Type
basic = GOneOf [GBool, GChar, GInt, GFloat]

highLevel :: Type
highLevel = GOneOf [GSet GAny, GMultiset GAny, GSeq GAny, GFunc GAny GAny, GRel GAny GAny, GATuple]

-- | Operator for checking whether two types match.
(=:=) :: Type -> Type -> Bool
a =:= b = a <> b /= GUndef


-- | Graciela Types form a Monoid under the `more specific` operator,
-- with the type @GAny@ as the identity.

instance Monoid Type where
  mempty = GAny
  mappend = (<>)
instance Semigroup Type where
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

  GSet a      <> GSet b      = case  a <> b of
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
      Expression { exp' = Value (IntV n0)} ==~ Expression { exp' = Value (IntV n1)} = n0 == n1
      _ ==~ _ = True

  GFunc a c   <> GFunc b d   = case (a <> b, c <> d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GFunc e f
  GRel a c    <> GRel b d    = case (a <> b, c <> d) of
    (GUndef, _) -> GUndef
    (_, GUndef) -> GUndef
    (e, f)      -> GRel e f

  GTuple a0 a1 <> GTuple b0 b1 =
    let (c0, c1) = (a0 <> b0, a1 <> b1)
    in if c0 == GUndef || c1 == GUndef
      then GUndef
      else GTuple c0 c1

  a@(GTuple _ _) <> GATuple = a
  GATuple <> a@(GTuple _ _) = a
  GATuple <> GATuple = GATuple

  a@(GTypeVar ai an) <> GTypeVar bi bn =
    if ai == bi && an == bn then a else GUndef

  a@(GTypeVar _ _) <> GATypeVar = a
  GATypeVar <> a@(GTypeVar _ _) = a
  GATypeVar <> GATypeVar = GATypeVar

  GFullDataType a fs <> GFullDataType b fs' =
    if a == b  && fs == fs'
      then GFullDataType a fs
      else GUndef

  t@(GDataType a (Just a') _) <> GDataType b Nothing _ =
    if a' == b
      then t
      else GUndef

  GDataType a Nothing _ <> t@(GDataType b (Just b') _) =
    if a == b'
      then t
      else GUndef

  t@(GDataType a _ ta) <> GDataType b _ tb =
    if a == b
      then t
      else GUndef

  t@(GDataType a n _) <> GFullDataType b fs = case n of
    Just name | name == b -> t
    _ -> if a == b then GFullDataType b fs else GUndef

  GFullDataType a fs <> t@(GDataType b n _) = case n of
    Just name | name == a -> t
    _ -> if a == b then GFullDataType a fs else GUndef

  a@(GFullDataType _ _) <> GADataType = a
  GADataType <> a@(GFullDataType _ _) = a
  a@(GDataType _ _ _) <> GADataType = a
  GADataType <> a@(GDataType _ _ _) = a
  GADataType <> GADataType = GADataType

  GUnsafeName a <> GUnsafeName b =
    if a == b then GUnsafeName a else GUndef

  _ <> _ = GUndef


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
        GArray    ds t  -> show (length ds) <> "-D array of " <> show' t
        GSet      t     -> "set of " <> show' t
        GMultiset t     -> "multiset of " <> show' t
        GSeq      t     -> "sequence of " <> show' t
        GFunc     ta tb -> "function " <> show' ta <> " -> " <> show' tb
        GRel      ta tb -> "relation " <> show' ta <> " -> " <> show' tb

        GTuple      a b ->
          "tuple (" <> show' a <> ", " <> show' b <> ")"
        GTypeVar  i n   -> unpack n

        GFullDataType n targs   ->
          unpack n <> "(" <> intercalate "," (fmap show' (toList targs)) <> ")"

        GDataType n na targs -> unpack n <> "(" <> intercalate "," (fmap show' (toList targs)) <> ") " -- <> show na

        GAny            -> "any type"
        GOneOf       as -> "one of " <> show as

        GUnsafeName t     -> unpack t

        GRawName -> "Function Identifier"

        GATypeVar  -> "a type variable"
        GADataType -> "a data type"
        GATuple    -> "a tuple"

        I64               -> "64-bit int"
