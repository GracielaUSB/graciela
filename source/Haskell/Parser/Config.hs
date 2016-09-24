{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Config
  ( Config (..)
  , defaultConfig
  -- * LLVM Function strings
  , sqrtIString
  , sqrtFString
  , absIString
  , absFString
  , toSetMultiString
  , toSetSeqString
  , toSetFuncString
  , toSetRelString
  , toMultiSetString
  , toMultiSeqString
  , funcString
  , relString
  , cardSetString
  , cardMultiString
  , cardSeqString
  , cardFuncString
  , cardRelString
  , domainFuncString
  , domainRelString
  , codomainFuncString
  , codomainRelString
  , inverseFuncString
  , inverseRelString
  , multiplicityMultiString
  , multiplicitySeqString
  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression        (Value (BoolV))
import           AST.Struct
import           AST.Type
import           Entry
import           Error
import           Location
import           SymbolTable
import           Token
--------------------------------------------------------------------------------
import           Control.Lens          (at, (&~), (?=))
import           Data.Foldable         (foldl')
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map (empty, fromList, mapWithKey)
import           Data.Semigroup        ((<>))
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq (empty)
import           Data.Set              (Set)
import qualified Data.Set              as Set (empty)
import           Data.Text             (Text, pack)
import           Text.Megaparsec.Error (ParseError (..))
import           Text.Megaparsec.Pos   (unsafePos)
--------------------------------------------------------------------------------

data Config = Config
  { nativeTypes     :: Map Text (Type, Location)
  , nativeFunctions :: Map Text Definition
  , nativeSymbols   :: SymbolTable
  }

defaultConfig :: Config
defaultConfig = Config
  { nativeTypes
  , nativeFunctions
  , nativeSymbols = foldl' auxInsert emptyGlobal symbols }

  where
    nativeTypes = Map.empty &~ do
      at "int"     ?= (GInt,   gracielaDef)
      at "float"   ?= (GFloat, gracielaDef)
      at "boolean" ?= (GBool,  gracielaDef)
      at "char"    ?= (GChar,  gracielaDef)

    symbols =
      [ ("otherwise", Const GBool (BoolV True)) ] :: [(Text, Entry')]

    auxInsert st (k, e') = insertSymbol k (Entry k gracielaDef e') st

    nativeFunctions = Map.mapWithKey wrap $ Map.empty &~ do
      at "abs"          ?= (absG         , [])
      at "card"         ?= (cardG        , [])
      at "cardinality"  ?= (cardG        , [])
      at "codomain"     ?= (codomainG    , [])
      at "domain"       ?= (domainG      , [])
      at "func"         ?= (funcG        , [])
      at "inverse"      ?= (inverseG     , [])
      at "multiplicity" ?= (multiplicityG, [0])
      at "rel"          ?= (relG         , [])
      at "sqrt"         ?= (sqrtG        , [])
      at "toMultiset"   ?= (toMultisetG  , [])
      at "toSet"        ?= (toSetG       , [])

    absG, cardG, codomainG, domainG  :: Seq Type -> Either Error (Type, Text, Bool)
    funcG, inverseG, multiplicityG   :: Seq Type -> Either Error (Type, Text, Bool)
    relG, sqrtG, toMultisetG, toSetG :: Seq Type -> Either Error (Type, Text, Bool)

    wrap defName (signatures, casts) = Definition
      { defLoc  = gracielaDef
      , defName
      , pre     = undefined
      , post    = undefined
      , bound   = Nothing
      , def'    = GracielaFunc signatures casts }

    badArg = BadFunctionArgumentType'
      { fPos     = gracielaDef'
      , paramNum = undefined
      , fName    = undefined
      , pTypes   = undefined
      , aType    = undefined }
    badNumArgs = BadFuncNumberOfArgs
      { fPos    = gracielaDef'
      , fName   = undefined
      , nParams = undefined
      , nArgs   = undefined }

    sqrtG [ GInt   ] = Right (GInt,   pack sqrtIString, True)
    sqrtG [ GFloat ] = Right (GFloat, pack sqrtFString, True)
    sqrtG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "sqrt"
      , pTypes = [GInt, GFloat]
      , aType  = a }
    sqrtG args = Left badNumArgs
      { fName = "sqrt"
      , nParams = 1
      , nArgs = length args}

    absG [ GInt   ] = Right (GInt,   pack absIString, True)
    absG [ GFloat ] = Right (GFloat, pack absFString, False)
    absG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "abs"
      , pTypes = [GInt, GFloat]
      , aType  = a }
    absG args = Left badNumArgs
      { fName = "abs"
      , nParams = 1
      , nArgs = length args}

    toSetG [ GMultiset a ] = Right (GSet a, pack toSetMultiString, False)
    toSetG [ GSeq a      ] = Right (GSet a, pack toSetSeqString, False)
    toSetG [ GFunc a b   ] = Right (GSet (GTuple [a, b]), pack toSetFuncString, False)
    toSetG [ GRel  a b   ] = Right (GSet (GTuple [a, b]), pack toSetRelString, False)
    toSetG [ a ] = Left badArg
      { paramNum = 1
      , fName = "toSet"
      , pTypes =
        [ GMultiset (GUnsafeName "a")
        , GSeq      (GUnsafeName "a")
        , GFunc     (GUnsafeName "a") (GUnsafeName "b")
        , GRel      (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    toSetG args = Left badNumArgs
      { fName = "toSet"
      , nParams = 1
      , nArgs = length args}

    toMultisetG [ GSet a ] = Right (GMultiset a, pack toMultiSetString, False)
    toMultisetG [ GSeq a ] = Right (GMultiset a, pack toMultiSeqString, False)
    toMultisetG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "toMultiset"
      , pTypes =
        [ GSet (GUnsafeName "a")
        , GSeq (GUnsafeName "a") ]
      , aType = a }
    toMultisetG args = Left badNumArgs
      { fName = "toMultiset"
      , nParams = 1
      , nArgs = length args}

    funcG [ GSet (GTuple [a, b]) ] = Right (GFunc    a    b, pack funcString, True)
    funcG [ GSet GAny            ] = Right (GFunc GAny GAny, pack funcString, True)
    funcG [ a ] = Left badArg
      { paramNum = 1
      , fName = "func"
      , pTypes =
        [ GTuple [GUnsafeName "a", GUnsafeName "b"] ]
      , aType = a }
    funcG args = Left badNumArgs
      { fName = "func"
      , nParams = 1
      , nArgs = length args}

    relG [ GSet (GTuple [a, b]) ] = Right (GRel    a    b, pack relString, False)
    relG [ GSet GAny            ] = Right (GRel GAny GAny, pack relString, False)
    relG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "rel"
      , pTypes =
        [ GTuple [GUnsafeName "a", GUnsafeName "b"] ]
      , aType = a }
    relG args = Left badNumArgs
      { fName = "rel"
      , nParams = 1
      , nArgs = length args}

    cardG [ GSet a      ] = Right (GInt, pack cardSetString, False)
    cardG [ GMultiset a ] = Right (GInt, pack cardMultiString, False)
    cardG [ GSeq a      ] = Right (GInt, pack cardSeqString, False)
    cardG [ GFunc a b   ] = Right (GInt, pack cardFuncString, False)
    cardG [ GRel  a b   ] = Right (GInt, pack cardRelString, False)
    cardG [ a ] = Left badArg
      { paramNum = 1
      , fName = "card"
      , pTypes =
        [ GSet      (GUnsafeName "a")
        , GMultiset (GUnsafeName "a")
        , GSeq      (GUnsafeName "a")
        , GFunc     (GUnsafeName "a") (GUnsafeName "b")
        , GRel      (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    cardG args = Left badNumArgs
      { fName = "card"
      , nParams = 1
      , nArgs = length args}

    domainG [ GFunc a b   ] = Right (GSet a, pack domainFuncString, False)
    domainG [ GRel  a b   ] = Right (GSet a, pack domainRelString, False)
    domainG [ a ] = Left badArg
      { paramNum = 1
      , fName = "domain"
      , pTypes =
        [ GFunc (GUnsafeName "a") (GUnsafeName "b")
        , GRel  (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    domainG args = Left badNumArgs
      { fName = "domain"
      , nParams = 1
      , nArgs = length args}

    codomainG [ GFunc a b   ] = Right (GSet a, pack codomainFuncString, False)
    codomainG [ GRel  a b   ] = Right (GSet a, pack codomainRelString, False)
    codomainG [ a ] = Left badArg
      { paramNum = 1
      , fName = "codomain"
      , pTypes =
        [ GFunc (GUnsafeName "a") (GUnsafeName "b")
        , GRel  (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    codomainG args = Left badNumArgs
      { fName = "codomain"
      , nParams = 1
      , nArgs = length args}

    inverseG [ GFunc a b ] = Right (GRel b a, pack inverseFuncString, False)
    inverseG [ GRel  a b ] = Right (GRel b a, pack inverseRelString, False)
    inverseG [ a ] = Left badArg
      { paramNum = 1
      , fName = "inverse"
      , pTypes =
        [ GFunc (GUnsafeName "a") (GUnsafeName "b")
        , GRel  (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    inverseG args = Left badNumArgs
      { fName = "inverse"
      , nParams = 1
      , nArgs = length args}

    multiplicityG [ a, b ] = case b of
      GMultiset b' -> if b' =:= a
        then Right (GInt, pack multiplicityMultiString, False)
        else Left badArg
          { paramNum = 2
          , fName = "multiplicity"
          , pTypes =
            [ GMultiset a ]
          , aType = a }
      GSeq      b' -> if b' =:= a
        then Right (GInt, pack multiplicitySeqString, False)
        else Left badArg
          { paramNum = 2
          , fName = "multiplicity"
          , pTypes =
            [ GSeq a ]
          , aType = a }
      _ -> Left badArg
        { paramNum = 2
        , fName = "multiplicity"
        , pTypes =
          [ GSeq a
          , GMultiset a ]
        , aType = a }
    multiplicityG args = Left badNumArgs
      { fName = "multiplicity"
      , nParams = 2
      , nArgs = length args}

sqrtIString, sqrtFString :: String
sqrtIString = "_sqrt_i"
sqrtFString = "_sqrt_f"

absIString, absFString :: String
absIString = "_abs_i"
absFString = "_abs_f"

toSetMultiString, toSetSeqString, toSetFuncString, toSetRelString :: String
toSetMultiString = "_multiset_to_set"
toSetSeqString = "_seq_to_set"
toSetFuncString = "_func_to_set"
toSetRelString = "_rel_to_set"

toMultiSetString, toMultiSeqString :: String
toMultiSetString = "_set_to_multiset"
toMultiSeqString = "_seq_to_multiset"

funcString, relString :: String
funcString = "_func"
relString = "_rel"

cardSetString, cardMultiString, cardSeqString, cardFuncString, cardRelString :: String
cardSetString = "_card_set"
cardMultiString = "_card_multiset"
cardSeqString = "_card_seq"
cardFuncString = "_card_func"
cardRelString = "_card_rel"

domainFuncString, domainRelString, codomainFuncString, codomainRelString :: String
domainFuncString = "_domain_func"
domainRelString = "_domain_rel"
codomainFuncString = "_codomain_func"
codomainRelString = "_codomain_rel"

inverseFuncString, inverseRelString :: String
inverseFuncString = "_inverse_func"
inverseRelString = "_inverse_rel"

multiplicityMultiString, multiplicitySeqString :: String
multiplicityMultiString = "_multiplicity_multiset"
multiplicitySeqString = "_multiplicity_seq"
