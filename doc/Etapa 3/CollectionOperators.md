# Collection Operators

## Set
union, ∪                   :: Set a -> Set a -> Set a
intersect, intersection, ∩ :: Set a -> Set a -> Set a
difference, \\             :: Set a -> Set a -> Set a
elem, ∈                   :: a     -> Set a -> Bool
notelem, ∉                :: a     -> Set a -> Bool
cardinality, card          :: Set a -> Int
ssubset, ⊂, ⊊             :: Set a -> Set a -> Bool
subset, ⊆                 :: Set a -> Set a -> Bool
ssuperset, ⊃, ⊋           :: Set a -> Set a -> Bool
superset, ⊇               :: Set a -> Set a -> Bool
==                         :: Set a -> Set a -> Bool
!=                         :: Set a -> Set a -> Bool
cross                      :: Set a -> Set b -> Set (a, b)
func                       :: Set (a, b) -> Func a b <!-- fails if an element of the codomain appears twice -->
rel                        :: Set (a, b) -> Rel a b
toMultiset                 :: Set a -> Multiset a

## Multiset
union, ∪                   :: Multiset a -> Multiset a -> Multiset a  
msum, ⊎                    :: Multiset a -> Multiset a -> Multiset a  
intersect, intersection, ∩ :: Multiset a -> Multiset a -> Multiset a
difference, \\             :: Multiset a -> Multiset a -> Multiset a
elem, ∈                    :: a     -> Multiset a -> Bool
notelem, ∉                 :: a     -> Multiset a -> Bool
cardinality, card          :: Multiset a -> Int
ssubset, ⊂, ⊊              :: Multiset a -> Multiset a -> Bool
subset, ⊆                  :: Multiset a -> Multiset a -> Bool
ssuperset, ⊃, ⊋            :: Multiset a -> Multiset a -> Bool
superset, ⊇                :: Multiset a -> Multiset a -> Bool
==                         :: Multiset a -> Multiset a -> Bool
!=                         :: Multiset a -> Multiset a -> Bool
cross                      :: Multiset a -> Multiset b -> Multiset (a, b)
multiplicity               :: Multiset a -> a -> Int
scalar, ⊗                  :: Int -> Multiset a -> Multiset a
toSet                      :: Multiset a -> Set a

## Sequence
(#)                       :: Sequence a -> Int -> a <!-- fails if i is out of range -->
elem, ∈                   :: a     -> Sequence a -> Bool
notelem, ∉                :: a     -> Sequence a -> Bool
++, ⧺                      :: Sequence a -> Sequence a -> Sequence a
cardinality, card          :: Sequence a -> Int
==                         :: Sequence a -> Sequence a -> Bool
!=                         :: Sequence a -> Sequence a -> Bool
toSet                      :: Sequence a -> Set a
toMultiset                 :: Sequence a -> Multiset a

## Func
@                          :: Func a b -> a -> b <!-- fails if x is not in domain -->
domain                     :: Func a b -> Set a
codomain                   :: Func a b -> Set b
compose, ∘                 :: Func a b -> Func b c -> Func a c
inverse                    :: Func a b -> Rel b a
toSet                      :: Func a b -> Set (a, b)

## Rel
@                          :: Rel a b -> a -> Set b
domain                     :: Rel a b -> Set a
codomain                   :: Rel a b -> Set b
compose, ∘                 :: Rel a b -> Rel b c -> Rel a c
inverse                    :: Rel a b -> Rel b a
toSet                      :: Rel a b -> Set (a, b)

## DONE (Op)
notelem, ∉                :: a     -> Multiset a -> Bool
                            | a     -> Sequence a -> Bool
                            | a     -> Set a -> Bool
elem, ∈                   :: a     -> Multiset a -> Bool
                            | a     -> Sequence a -> Bool
                            | a     -> Set a -> Bool
!=                         :: Multiset a -> Multiset a -> Bool
                            | Sequence a -> Sequence a -> Bool
                            | Set a -> Set a -> Bool
==                         :: Multiset a -> Multiset a -> Bool
                            | Sequence a -> Sequence a -> Bool
                            | Set a -> Set a -> Bool

ssubset, ⊂, ⊊             :: Multiset a -> Multiset a -> Bool
                            | Set a -> Set a -> Bool
ssuperset, ⊃, ⊋           :: Multiset a -> Multiset a -> Bool
                            | Set a -> Set a -> Bool
subset, ⊆                 :: Multiset a -> Multiset a -> Bool
                            | Set a -> Set a -> Bool
superset, ⊇               :: Multiset a -> Multiset a -> Bool
                            | Set a -> Set a -> Bool

difference, \\             :: Multiset a -> Multiset a -> Multiset a
                            | Set a -> Set a -> Set a
intersect, intersection, ∩ :: Multiset a -> Multiset a -> Multiset a
                            | Set a -> Set a -> Set a
union, ∪                   :: Multiset a -> Multiset a -> Multiset a
                            | Set a -> Set a -> Set a
msum, ⊎                    :: Multiset a -> Multiset a -> Multiset a
(#)                        :: Sequence a -> Int -> a <!-- fails if x is not in domain -->
@                          :: Func a b -> a -> b <!-- fails if x is not in domain -->
                            | Rel a b -> a -> Set b
++, ⧺                      :: Sequence a -> Sequence a -> Sequence a

## DONE (Function)
inverse()                  :: Func a b -> Rel b a
                            | Rel a b -> Rel b a
toMultiset()               :: Sequence a -> Multiset a
                            | Set a -> Multiset a
toSet()                    :: Func a b -> Set (a, b)
                            | Multiset a -> Set a
                            | Rel a b -> Set (a, b)
                            | Sequence a -> Set a
multiplicity()             :: Multiset a -> a -> Int
rel()                      :: Set (a, b) -> Rel a b
cardinality, card()        :: Multiset a -> Int
                            | Sequence a -> Int
                            | Set a -> Int
codomain()                 :: Func a b -> Set b
                            | Rel a b -> Set b
domain()                   :: Func a b -> Set a
                            | Rel a b -> Set a -->
func()                     :: Set (a, b) -> Func a b <!-- fails if an element of the codomain appears twice -->


## NOT GONNA DO
scalar, ⊗                 :: Int -> Multiset a -> Multiset a
cross                      :: Multiset a -> Multiset b -> Multiset (a, b)
                            | Set a -> Set b -> Set (a, b)
compose, ∘                 :: Func a b -> Func b c -> Func a c
                            | Rel a b -> Rel b c -> Rel a c
