{-|
Module      : SymbolTable
Description : Tabla de Simbolos
Copyright   : Graciela

Modulo donde se encuentra todo lo referente al manejo de la tabla de simbolos
del compilador
-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module SymbolTable
  ( Entry
  , Scope (..)
  , SymbolTable
  , closeScope
  , defocus
  , depth
  , empty
  , focus
  , goDownFirst
  , goDownLast
  , goNext
  , goPrevious
  , goUp
  , insertST
  , insertSymbol
  , isLocal
  , isSymbol
  , local
  , lookup
  , openScope
  , root
  ) where
--------------------------------------------------------------------------------
import           Entry
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Data.List       (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq, ViewL ((:<)), ViewR ((:>)),
                                  (<|), (><), (|>))
import qualified Data.Sequence   as Seq
import           Data.Text       (Text, unpack)
import           Prelude         hiding (lookup)
--------------------------------------------------------------------------------
-- Symbol Table Entry ------------------
type Entry = Entry' SymbolTable

type Entries = Map Text Entry

-- Symbol Table Scope ------------------
data Scope = Scope
  { sLoc      :: Location
  , sEntries  :: Entries
  , sChildren :: Scopes
  }

type Scopes = Seq Scope

instance Treelike Scope where
  toTree Scope { sLoc, sEntries, sChildren } =
    Node ("Scope " <> show sLoc) $
      if Map.null sEntries
        then toForest sChildren
        else Node "Symbols"
          (toForest . sortOn _loc . Map.elems $ sEntries) :
            toForest sChildren


lookup' :: Text -> Scope -> Either Text Entry
lookup' key Scope { sEntries } =
  case Map.lookup key sEntries of
    Just entry' -> Right entry'
    Nothing    -> Left "Not found."


insert' :: Text -> Entry -> Scope -> Scope
insert' key entry' s @ Scope { sEntries } =
  s { sEntries = Map.insert key entry' sEntries }


insertST' :: Scope -> Scope -> Scope
insertST' newScope s @ Scope { sChildren } =
  s { sChildren = sChildren |> newScope }


close' :: SourcePos -> Scope -> Scope
close' pos scope @ Scope { sLoc = Location (from, _) } =
  scope { sLoc = Location (from, pos) }


empty' :: SourcePos -> Scope
empty' pos
  = Scope
    { sLoc      = Location (pos, pos)
    , sEntries  = Map.empty
    , sChildren = Seq.empty
    }


-- Symbol Table ------------------------
data Breadcrumb
  = Breadcrumb
    { bScope :: (Location, Entries)
    , bLeft  :: Scopes
    , bRight :: Scopes
    }


type SymbolTable = (Scope, [Breadcrumb])


---- Starter Symbol Table ----
empty :: SourcePos -> SymbolTable
empty = focus . empty'


---- Moving around -----------
goDownFirst :: SymbolTable -> Either Text SymbolTable
goDownFirst (Scope { sLoc, sEntries, sChildren }, bs)
  | Seq.null sChildren = Left "No embedded scopes."
  | otherwise =
    Right (x, Breadcrumb (sLoc, sEntries) Seq.empty xs : bs)
  where
    x :< xs = Seq.viewl sChildren


goDownLast :: SymbolTable -> Either Text SymbolTable
goDownLast (Scope { sLoc, sEntries, sChildren }, bs)
  | Seq.null sChildren = Left "No embedded scopes."
  | otherwise =
    Right (x, Breadcrumb (sLoc, sEntries) xs Seq.empty : bs)
  where
    xs :> x = Seq.viewr sChildren


goNext :: SymbolTable -> Either Text SymbolTable
goNext (_, []) =
    Left "Root scope has no siblings."
goNext (s, Breadcrumb { bScope, bLeft, bRight } : bs)
  | Seq.null bRight = Left "Already at last scope."
  | otherwise = Right (r, Breadcrumb bScope (bLeft |> s) bRight' : bs)
  where
    r :< bRight' = Seq.viewl bRight


goPrevious :: SymbolTable -> Either Text SymbolTable
goPrevious (_, []) =
  Left "Root scope has no siblings."
goPrevious (s, Breadcrumb { bScope, bLeft, bRight } : bs)
  | Seq.null bRight = Left "Already at first scope."
  | otherwise = Right (l, Breadcrumb bScope bLeft' (s <| bRight) : bs)
    where
      bLeft' :> l = Seq.viewr bLeft


goUp :: SymbolTable -> Either Text SymbolTable
goUp (_, []) =
  Left "Already at root scope."
goUp (s, Breadcrumb { bScope = (sLoc, sEntries), bLeft, bRight } : bs) =
  Right (Scope sLoc sEntries ((bLeft |> s) >< bRight), bs)


root :: SymbolTable -> Either a SymbolTable -- we want to stay in the monad
root (s, []) = Right (s, [])
root st      = root . (\(Right x) -> x) . goUp $ st


---- (de)focusing ------------
focus :: Scope -> SymbolTable
focus = (,[])


defocus :: SymbolTable -> Scope
defocus = fst


---- Using the table ---------
isSymbol :: Text -> SymbolTable -> Bool
isSymbol key st = case lookup key st of
  Left _ -> False
  Right _ -> True


isLocal ::  Text -> SymbolTable -> Bool
isLocal key st = case local key st of
  Left _ -> False
  Right _ -> True


lookup :: Text -> SymbolTable -> Either Text Entry
lookup key (s, []) =
  lookup' key s
lookup key st@(s, _) =
  case lookup' key s of
    Left      _ -> goUp st >>= lookup key
    bRightEntry -> bRightEntry


local :: Text -> SymbolTable -> Either Text Entry
local key (s, _) =
  lookup' key s


depth :: SymbolTable -> Int
depth (s, []) = 0
depth st      = depth . (\(Right x) -> x) . goUp $ st


insertSymbol :: Text -> Entry -> SymbolTable -> SymbolTable
insertSymbol key entry' (s, bs) =
  (insert' key entry' s, bs)


insertST :: Scope -> SymbolTable -> SymbolTable
insertST newST (s, bs) =
  (insertST' newST s, bs)


openScope :: SourcePos -> SymbolTable -> SymbolTable
openScope p = (\(Right x) -> x) . goDownLast . insertST (empty' p)


closeScope' :: SourcePos -> SymbolTable -> Either Text SymbolTable
closeScope' p (s, bs) = goUp (close' p s, bs)


closeScope :: SourcePos -> SymbolTable -> SymbolTable
closeScope p st = (\(Right x) -> x) $ closeScope' p st
