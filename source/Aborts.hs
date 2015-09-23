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


createTagIf :: Name -> Location -> LLVM ()
createTagIf next loc = do

    let x  = (ConstantOperand $ C.Int 32 1, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ (Do $ Unreachable [])


createTagAbort :: Location -> LLVM ()
createTagAbort loc = do

    let x  = (ConstantOperand $ C.Int 32 2, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    return ()


createTagPre :: Name -> Location -> LLVM ()
createTagPre next loc = do

    let x  = (ConstantOperand $ C.Int 32 3, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 


createTagPost :: Name -> Location -> LLVM ()
createTagPost next loc = do

    let x  = (ConstantOperand $ C.Int 32 4, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 


createTagAsert :: Name -> Location -> LLVM ()
createTagAsert next loc = do

    let x  = (ConstantOperand $ C.Int 32 5, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 


createTagInv :: Name -> Location -> LLVM ()
createTagInv next loc = do

    let x  = (ConstantOperand $ C.Int 32 6, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 


createTagBound :: Name -> Location -> Int -> LLVM ()
createTagBound next loc 1 = do

    let x  = (ConstantOperand $ C.Int 32 7, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ (Do $ Unreachable [])

createTagBound next loc 2 = do

    let x  = (ConstantOperand $ C.Int 32 8, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ (Do $ Unreachable [])


createTagZero :: Name -> Location -> LLVM ()
createTagZero next loc = do 

    let x  = (ConstantOperand $ C.Int 32 9, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ (Do $ Unreachable [])


createTagForAll :: Name -> Location -> LLVM ()
createTagForAll next loc = do

    let x  = (ConstantOperand $ C.Int 32 10, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 


createTagExists :: Name -> Location -> LLVM ()
createTagExists next loc = do

    let x  = (ConstantOperand $ C.Int 32 11, [])
    let df = Right $ definedFunction intType (Name abortString)
    caller voidType df $ abortArgs x loc
    setLabel next $ branch next 
