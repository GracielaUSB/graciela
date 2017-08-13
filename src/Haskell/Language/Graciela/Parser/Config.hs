{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Graciela.Parser.Config
  ( Config (..)
  , defaultConfig
  -- * LLVM Function strings
  , sqrtIString
  , sqrtFString
  , absIString
  , absFString
  , isNanString
  , isInfString
  , toSetMultiString
  , toSetSeqString
  , toSetFuncString
  , toSetRelString
  , toMultiSetString
  , toMultiSeqString
  , toSeqSetString
  , toSeqMultiString
  , funcString
  , relString
  , domainFuncString
  , domainRelString
  , codomainFuncString
  , codomainRelString
  , inverseFuncString
  , inverseRelString
  , multiplicityMultiString
  , multiplicitySeqString
  , multiplicityMultiPairString
  , multiplicitySeqPairString
  , readlnString
-- * random procedure
  , randCharString
  , randIntString
  , randFloatString
  , randBoolString
  -- * LLVM Conversion function strings
  , float2intString
  , char2intString
  , pointer2intString
  , float2charString
  , int2charString
  , char2floatString
  , int2floatString  
  -- * trace pseudo-function strings
  , traceIntString
  , traceFloatString
  , traceCharString
  , traceBoolString
  , traceTypeVarString
  , traceStringIntString
  , traceStringFloatString
  , traceStringCharString
  , traceStringBoolString
  , traceStringTypeVarString
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression hiding (fName)
import           Language.Graciela.AST.Struct
import           Language.Graciela.AST.Type
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                     (at, (&~), (.=), (?=))
import           Data.Foldable                    (foldl')
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map (empty, fromList,
                                                          mapWithKey)

import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq (empty)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set (empty)
import           Data.Text                        (Text, pack)
import           Text.Megaparsec.Error            (ParseError (..))
import           Text.Megaparsec.Pos              (unsafePos)
--------------------------------------------------------------------------------

data Config = Config
  { nativeTypes      :: Map Text (Type, Location)
  , nativeFunctions  :: Map Text Definition
  , nativeProcedures :: Map Text Definition
  , nativeSymbols    :: SymbolTable }

defaultConfig :: Bool -> Bool -> Bool -> Config
defaultConfig enableTrace enableLowLevel noAssertions = Config
  { nativeTypes
  , nativeFunctions
  , nativeProcedures
  , nativeSymbols = foldl' auxInsert emptyGlobal symbols }

  where
    nativeTypes = Map.empty &~ do
      at "int"     ?= (GInt,   gracielaDef)
      at "float"   ?= (GFloat, gracielaDef)
      at "boolean" ?= (GBool,  gracielaDef)
      at "char"    ?= (GChar,  gracielaDef)

    symbols :: Map Text (Text, Entry')
    symbols = Map.mapWithKey (,) $ Map.empty &~ do
      at "otherwise" ?= Alias GBool  (BoolV True)
      at "MAX_INT"   ?= Alias GInt   (IntV maxBound)
      at "MIN_INT"   ?= Alias GInt   (IntV minBound)
      
      at "pi"        ?= Alias GFloat (FloatV pi)
      at "\960"      ?= Alias GFloat (FloatV pi)
      at "tau"       ?= Alias GFloat (FloatV (2*pi))
      at "\964"      ?= Alias GFloat (FloatV (2*pi))
      at "unbound"   .= if noAssertions 
        then Just $ Alias GInt (IntV 0) 
        else Nothing

    auxInsert st (k , e') = insertSymbol k (Entry k gracielaDef e') st

    nativeProcedures :: Map Text Definition
    nativeProcedures = Map.mapWithKey wrap $ Map.empty &~ do
      at "read"      ?= languageProcedure "read"
      at "write"     ?= languageProcedure "write"
      at "free"      ?= languageProcedure "free"
      at "new"       ?= languageProcedure "new"
      at "random"    ?= randomG
      
      where 
        wrap defName signatures = Definition
          { defLoc  = gracielaDef
          , defName
          , isDecl  = False
          , pre     = undefined
          , post    = undefined
          , bound   = Nothing
          , def'    = GracielaProc signatures }

    languageProcedure :: String -> (Seq Type -> Either Error (Text, Seq ArgMode))
    languageProcedure name = (\s -> Right (pack name, []))

    randomG :: Seq Type -> Either Error (Text, Seq ArgMode)
    randomG [ GChar ]  = Right (pack randCharString,  [Ref])
    randomG [ GInt ]   = Right (pack randIntString,   [Ref])
    randomG [ GFloat ] = Right (pack randFloatString, [Ref])
    randomG [ GBool ]  = Right (pack randBoolString,  [Ref])
    randomG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "random"
      , pTypes = [GInt, GFloat, GChar, GBool]
      , aType  = a }
    randomG args = Left badNumArgs
      { fName = "random"
      , nParams = 1
      , nArgs = length args}

    nativeFunctions :: Map Text Definition
    nativeFunctions = Map.mapWithKey wrap $ Map.empty &~ do
      at "abs"          ?= (absG         , [])
      at "codomain"     ?= (codomainG    , [])
      at "domain"       ?= (domainG      , [])
      at "func"         ?= (funcG        , [])
      at "inverse"      ?= (inverseG     , [])
      at "isNan"        ?= (isNanG       , [])
      at "isInf"        ?= (isInfG       , [])
      at "multiplicity" ?= (multiplicityG, [0])
      at "rel"          ?= (relG         , [])
      at "sqrt"         ?= (sqrtG        , [])
      at "toMultiset"   ?= (toMultisetG  , [])
      at "toSet"        ?= (toSetG       , [])
      at "toSequence"   ?= (toSequenceG  , [])
      at "toInt"        ?= (toIntG       , [])
      at "toChar"       ?= (toCharG      , [])
      at "toFloat"      ?= (toFloatG     , [])
      at "readln"       .= if enableLowLevel
        then Just (readlnG, [])
        else Nothing
      at "trace"        .= if enableTrace
        then Just (traceG, [])
        else Nothing
      where 
        wrap defName (signatures, casts) = Definition
          { defLoc  = gracielaDef
          , defName
          , isDecl  = False
          , pre     = undefined
          , post    = undefined
          , bound   = Nothing
          , def'    = GracielaFunc signatures casts }

    traceG, toIntG, toCharG, toFloatG :: Seq Type -> Either Error (Type, Text, Bool)
    absG, codomainG, domainG, readlnG :: Seq Type -> Either Error (Type, Text, Bool)
    funcG, inverseG, multiplicityG    :: Seq Type -> Either Error (Type, Text, Bool)
    relG, sqrtG, toMultisetG, toSetG  :: Seq Type -> Either Error (Type, Text, Bool)
    toSequenceG, isNanG, isInfG       :: Seq Type -> Either Error (Type, Text, Bool)

    

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

    readlnG [ GPointer GInt ] = Right (GPointer GChar, pack readlnString, False)
    readlnG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "readln"
      , pTypes = [GPointer GInt]
      , aType  = a }
    readlnG args = Left badNumArgs
      { fName = "readln"
      , nParams = 0
      , nArgs = length args }


    traceG [ GInt   ] = Right (GInt,   pack  traceIntString,  False)
    traceG [ GFloat ] = Right (GFloat, pack  traceFloatString, False)
    traceG [ GChar  ] = Right (GChar,  pack  traceCharString,  False)
    traceG [ GBool  ] = Right (GBool,  pack  traceBoolString,  False)
    traceG [ t ]
      | t =:= GATypeVar = Right (t, pack traceTypeVarString, False)
    traceG [ GString, GInt   ] = Right (GInt,   pack  traceStringIntString,  False)
    traceG [ GString, GFloat ] = Right (GFloat, pack  traceStringFloatString, False)
    traceG [ GString, GChar  ] = Right (GChar,  pack  traceStringCharString,  False)
    traceG [ GString, GBool  ] = Right (GBool,  pack  traceStringBoolString,  False)
    traceG [ GString, t ]
      | t =:= GATypeVar = Right (t, pack traceStringTypeVarString, False)
    traceG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "toInt"
      , pTypes = [GInt, GFloat, GChar, GBool]
      , aType  = a }
    traceG args = Left badNumArgs
      { fName = "toInt"
      , nParams = 1
      , nArgs = length args}

    toIntG [ GFloat ] = Right (GInt, pack float2intString, True)
    toIntG [ GChar  ] = Right (GInt, pack  char2intString, False)
    toIntG [ a ]
      | a =:= GPointer GAny && enableLowLevel =
        Right (GInt, pack pointer2intString, False)

      | otherwise = Left badArg
        { paramNum = 1
        , fName  = "toInt"
        , pTypes = [GFloat, GChar]
        , aType  = a }
    toIntG args = Left badNumArgs
      { fName = "toInt"
      , nParams = 1
      , nArgs = length args}

    toCharG [ GFloat ] = Right (GChar, pack float2charString, True)
    toCharG [ GInt   ] = Right (GChar, pack   int2charString, True)
    toCharG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "toChar"
      , pTypes = [GFloat, GInt]
      , aType  = a }
    toCharG args = Left badNumArgs
      { fName = "toChar"
      , nParams = 1
      , nArgs = length args}

    toFloatG [ GChar ] = Right (GFloat, pack char2floatString, False)
    toFloatG [ GInt  ] = Right (GFloat, pack  int2floatString, False)
    toFloatG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "toFloat"
      , pTypes = [GChar, GInt]
      , aType  = a }
    toFloatG args = Left badNumArgs
      { fName = "toFloat"
      , nParams = 1
      , nArgs = length args}

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

    isNanG [ GFloat ] = Right (GBool, pack isNanString, False)
    isNanG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "isNan"
      , pTypes = [GFloat]
      , aType  = a }
    isNanG args = Left badNumArgs
      { fName = "isNan"
      , nParams = 1
      , nArgs = length args}

    isInfG [ GFloat ] = Right (GBool, pack isInfString, False)
    isInfG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "isInf"
      , pTypes = [GFloat]
      , aType  = a }
    isInfG args = Left badNumArgs
      { fName = "isInf"
      , nParams = 1
      , nArgs = length args}

    toSetG [ GMultiset a ] = Right (GSet a, pack toSetMultiString, False)
    toSetG [ GSeq a      ] = Right (GSet a, pack toSetSeqString, False)
    toSetG [ GFunc a b   ] = Right (GSet (GTuple a b), pack toSetFuncString, False)
    toSetG [ GRel  a b   ] = Right (GSet (GTuple a b), pack toSetRelString, False)
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

    toSequenceG [ GSet      a ] = Right (GSeq a, pack toSeqSetString,   False)
    toSequenceG [ GMultiset a ] = Right (GSeq a, pack toSeqMultiString, False)
    toSequenceG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "toSequence"
      , pTypes =
        [ GSet      (GUnsafeName "a")
        , GMultiset (GUnsafeName "a") ]
      , aType = a }
    toSequenceG args = Left badNumArgs
      { fName = "toSequence"
      , nParams = 1
      , nArgs = length args}

    funcG [ GSet (GTuple a b) ] = Right (GFunc    a    b, pack funcString, True)
    funcG [ GSet GAny         ] = Right (GFunc GAny GAny, pack funcString, True)
    funcG [ a ] = Left badArg
      { paramNum = 1
      , fName = "func"
      , pTypes =
        [ GTuple (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    funcG args = Left badNumArgs
      { fName = "func"
      , nParams = 1
      , nArgs = length args}

    relG [ GSet (GTuple a b) ] = Right (GRel    a    b, pack relString, False)
    relG [ GSet GAny         ] = Right (GRel GAny GAny, pack relString, False)
    relG [ a ] = Left badArg
      { paramNum = 1
      , fName  = "rel"
      , pTypes =
        [ GTuple (GUnsafeName "a") (GUnsafeName "b") ]
      , aType = a }
    relG args = Left badNumArgs
      { fName = "rel"
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



    multiplicityG [ a, b ] = if a =:= GOneOf [basic, GATuple, GATypeVar]
      then
        case b of
          GMultiset b' -> if b' =:= a
            then Right (GInt
                       , pack $ case b' of
                          GTuple _ _ -> multiplicityMultiPairString
                          _          -> multiplicityMultiString
                       , False)
            else Left badArg
              { paramNum = 2
              , fName = "multiplicity"
              , pTypes =
                [ GMultiset a ]
              , aType = a }
          GSeq      b' -> if b' =:= a
            then Right (GInt
                       , pack $ case b' of
                          GTuple _ _ -> multiplicitySeqPairString
                          _          -> multiplicitySeqString
                       , False)
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
      else
        Left badArg
          { paramNum = 1
          , fName = "multiplicity"
          , pTypes =
            [ GInt
            , GChar
            , GFloat
            , GBool
            , GTuple (GTypeVar 1 "a") (GTypeVar 2 "b")]
          , aType = a }
    multiplicityG args = Left badNumArgs
      { fName = "multiplicity"
      , nParams = 2
      , nArgs = length args}


randCharString, randIntString, randFloatString, randBoolString :: String
randCharString  = "_randChar"
randIntString   = "_randInt"
randFloatString = "_randFloat"
randBoolString  = "_randBool"

sqrtIString, sqrtFString :: String
sqrtIString = "_sqrt_i"
sqrtFString = "_sqrt_f"

absIString, absFString :: String
absIString = "_abs_i"
absFString = "_abs_f"

isNanString, isInfString :: String
isNanString = "_isNan"
isInfString = "_isInf"

toSetMultiString, toSetSeqString, toSetFuncString, toSetRelString :: String
toSetMultiString = "_multiset_to_set"
toSetSeqString = "_seq_to_set"
toSetFuncString = "_func_to_set"
toSetRelString = "_rel_to_set"

toMultiSetString, toMultiSeqString :: String
toMultiSetString = "_set_to_multiset"
toMultiSeqString = "_seq_to_multiset"

toSeqSetString, toSeqMultiString :: String
toSeqSetString = "_set_to_seq"
toSeqMultiString = "_multiset_to_seq"

funcString, relString :: String
funcString = "_funcFromSet"
relString = "_relationFromSet"

domainFuncString, domainRelString, codomainFuncString, codomainRelString :: String
domainFuncString   = "_domFunction"
domainRelString    = "_domRelation"
codomainFuncString = "_codomainFunction"
codomainRelString  = "_codomainRelation"

inverseFuncString, inverseRelString :: String
inverseFuncString = "_inverse_func"
inverseRelString = "_inverse_rel"

multiplicityMultiString, multiplicitySeqString :: String
multiplicityMultiString = "_countMultiset"
multiplicitySeqString   = "_countSeq"

multiplicitySeqPairString, multiplicityMultiPairString :: String
multiplicitySeqPairString   = "_countSequencePair"
multiplicityMultiPairString = "_countMultisetPair"

float2intString, char2intString   :: String
float2intString   = "_float2int"
char2intString    = "_char2int"
pointer2intString = "_pointer2int"
float2charString, int2charString  :: String
float2charString  = "_float2char"
int2charString    = "_int2char"
char2floatString, int2floatString :: String
char2floatString  = "_char2float"
int2floatString   = "_int2float"

readlnString :: String
readlnString = "_readln"

traceIntString, traceFloatString, traceCharString, traceBoolString, traceTypeVarString, traceStringIntString, traceStringFloatString, traceStringCharString, traceStringBoolString, traceStringTypeVarString :: String
traceIntString           = "_traceInt"
traceFloatString         = "_traceFloat"
traceCharString          = "_traceChar"
traceBoolString          = "_traceBool"
traceTypeVarString       = "_traceTypeVar"
traceStringIntString     = "_traceStringInt"
traceStringFloatString   = "_traceStringFloat"
traceStringCharString    = "_traceStringChar"
traceStringBoolString    = "_traceStringBool"
traceStringTypeVarString = "_traceStringTypeVar"
