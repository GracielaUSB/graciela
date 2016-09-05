{-# LANGUAGE NamedFieldPuns #-}
module LLVM.Struct

where

import           AST.Struct
import           AST.Declaration
import           LLVM.Definition
import           LLVM.Type
import           LLVM.State
import qualified Type                         as T
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses, use, (%=), (+=),
                                               (.=))
import           Control.Monad                (when)
import           Data.Foldable                (toList)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq, (|>),ViewR((:>)), viewr)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text, unpack)
import           LLVM.General.AST             (BasicBlock (..), Definition(..))
import qualified LLVM.General.AST             as LLVM (Definition (..))
import           LLVM.General.AST.Type
import           LLVM.General.AST.Instruction (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

defineStruct :: Struct -> LLVM ()
defineStruct ast@Struct {structName, structDecls, structProcs} = do 
  
  currentStruct .= Just ast

  type' <- Just . StructureType False <$> 
           mapM (toLLVMType . declType . snd) (toList $ structDecls)

  let name  = Name . unpack $ structName 

  moduleDefs %= (|> TypeDefinition name type')
  mapM_ definition structProcs

  currentStruct .= Nothing
