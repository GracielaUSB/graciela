{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PostfixOperators #-}

module LLVM.Struct
  ( defineStruct
  )
  where

import           AST.Declaration              (Declaration(..))
import           AST.Expression               (Expression(..))
import           AST.Struct                   (Struct(..), Struct'(..))
import           LLVM.Abort                   (abort, abortString, warn,
                                               warnString)
import qualified LLVM.Abort                   as Abort (Abort (Post))
import qualified LLVM.Abort                   as Warning (Warning (Pre))
import           LLVM.Definition
import           LLVM.Expression
import           LLVM.Type
import           LLVM.Monad
import           LLVM.State
import           Location
import           Type                         as T                    
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
import           LLVM.General.AST             (BasicBlock (..), Definition(..),
                                               functionDefaults,Parameter (..),
                                               Terminator (..))
import           LLVM.General.AST.Global      (Global (..),functionDefaults)
import           LLVM.General.AST.Type        as LLVM
import           LLVM.General.AST.Instruction (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

data Invariant = Invariant | RepInvariant | CoupInvariant deriving (Eq)

defineStruct :: Struct -> LLVM ()
defineStruct ast@Struct {structName, structDecls, structProcs, struct'} = 
  case struct' of 
    DataType {abstract, abstractTypes, inv, repinv, coupinv} -> do

      currentStruct .= Just ast

      type' <- Just . StructureType False <$> 
               mapM (toLLVMType . declType . snd) (toList $ structDecls)

      let 
        name  = Name . unpack $ structName 
        structType = LLVM.NamedTypeReference name

      moduleDefs %= (|> TypeDefinition name type')      

      defineStructInv Invariant structName structType inv
      defineStructInv RepInvariant structName structType repinv

      mapM_ definition structProcs

      currentStruct .= Nothing

defineStructInv :: Invariant
                -> Text
                -> LLVM.Type
                -> Expression
                -> LLVM ()
defineStructInv inv name t expr@ Expression {loc = Location(pos,_)} 
  | inv == CoupInvariant = undefined
  | otherwise = do
  
    let 
      procName = (case inv of 
          Invariant -> "inv-"
          RepInvariant -> "repInv-") <> unpack name

    proc <- newLabel $ "proc" <> procName
    (proc #)
    
    openScope
    name' <- insertName "self"
    -- Evaluate the condition expression
    cond <- expression expr
    -- Create both label
    trueLabel  <- newLabel $ "condTrue"
    falseLabel <- newLabel $ "condFalse"
    -- Create the conditional branch
    terminate' CondBr
      { condition = cond
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = [] }
    -- Set the false label to the warning, then continue normally
    (falseLabel #)
    
    case inv of 
      Invariant    -> warn Warning.Pre pos
      RepInvariant -> warn Warning.Pre pos
      
    terminate' Br
      { dest      = trueLabel
      , metadata' = [] }

    -- And the true label to the next instructions
    (trueLabel #)

    terminate' $ Ret Nothing []
    closeScope

    blocks' <- use blocks
    blocks .= Seq.empty

    let selfParam = Parameter t (Name name') []

    addDefinition $ GlobalDefinition functionDefaults
          { name        = Name procName
          , parameters  = ([selfParam],False)
          , returnType  = voidType
          , basicBlocks = toList blocks'
        }
