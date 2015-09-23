{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aborts where

import qualified LLVM.General.AST.FloatingPointPredicate as FL 
import qualified LLVM.General.AST.IntegerPredicate       as IL 
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import LLVM.General.AST.AddrSpace
import qualified Data.Sequence                           as DS
import qualified Data.Text                               as TE
import qualified Data.Map                                as DM
import qualified Type                                    as T
import qualified AST                                     as MyAST
import LLVM.General.AST                                  as AST
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Attribute
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.Module
import Data.Foldable (toList)
import CodegenState
import SymbolTable
import Data.Either
import Data.Maybe
import Data.Word
import Data.Char
import Contents
import Location


abortString   = "_abort"

abortArgs :: (Operand, [ParameterAttribute]) -> Location -> [(Operand, [ParameterAttribute])]
abortArgs x loc = [x, (ConstantOperand $ C.Int 32 (fromIntegral $ line   loc), []), 
                      (ConstantOperand $ C.Int 32 (fromIntegral $ column loc), []) ]


callAbort :: Integer -> Location -> LLVM (Operand)
callAbort num loc = do
    let x  = (ConstantOperand $ C.Int 32 num, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc


createTagIf :: Name -> Location -> LLVM ()
createTagIf next loc = do

    callAbort 1 loc
    setLabel next $ nothing


createTagAbort :: Location -> LLVM ()
createTagAbort loc = do

    callAbort 2 loc
    return ()


createTagPre :: Name -> Location -> LLVM ()
createTagPre next loc = do

    callAbort 3 loc
    setLabel next $ branch next 


createTagPost :: Name -> Location -> LLVM ()
createTagPost next loc = do

    callAbort 4 loc
    setLabel next $ branch next 


createTagAsert :: Name -> Location -> LLVM ()
createTagAsert next loc = do

    callAbort 5 loc
    setLabel next $ branch next 


createTagInv :: Name -> Location -> LLVM ()
createTagInv next loc = do

    callAbort 6 loc
    setLabel next $ branch next 


createTagBound :: Name -> Location -> Int -> LLVM ()
createTagBound next loc 1 = do

    callAbort 7 loc
    setLabel next $ nothing

createTagBound next loc 2 = do

    callAbort 8 loc
    setLabel next $ nothing


createTagZero :: Name -> Location -> LLVM ()
createTagZero next loc = do 

    callAbort 9 loc
    setLabel next $ nothing


createTagForAll :: Name -> Location -> LLVM ()
createTagForAll next loc = do

    callAbort 10 loc
    setLabel next $ branch next 


createTagExists :: Name -> Location -> LLVM ()
createTagExists next loc = do

    callAbort 11 loc
    setLabel next $ branch next 