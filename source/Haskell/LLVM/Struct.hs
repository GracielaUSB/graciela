{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}

module LLVM.Struct
  ( defineStruct
  )
  where

import           AST.Declaration              (Declaration(..))
import           AST.Expression               (Expression(..))
import           AST.Struct                   (Struct(..), Struct'(..))
import           AST.Type                     as T
import           LLVM.Abort                   (abort, abortString)
import qualified LLVM.Abort                   as Abort (Abort (RepInvariant,
                                                Invariant))
import qualified LLVM.Warning                 as Warning (Warning (Pre))
import           LLVM.Warning                 (warn)
import           LLVM.Definition
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                 (warn, warnString)
import qualified LLVM.Warning                 as Warning (Warning (Pre))
import           Location
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses, use, (%=), (+=),
                                               (.=))
import           Control.Monad                (when)
import           Data.Foldable                (toList)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq, ViewR ((:>)), viewr, (|>))
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text, unpack)
import           LLVM.General.AST             (BasicBlock (..), Definition (..),
                                               Parameter (..), Terminator (..),
                                               functionDefaults)
import           LLVM.General.AST.Global      (Global (..), functionDefaults)
import           LLVM.General.AST.Instruction (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
import           LLVM.General.AST.Type        as LLVM
--------------------------------------------------------------------------------

data Invariant = Invariant | RepInvariant | CoupInvariant deriving (Eq)

defineStruct :: Text -> (Map T.Type T.Type, Struct) -> LLVM ()
defineStruct structName (mapType, ast) = case ast of
  
  Struct {structName,structTypes, structDecls, structProcs, struct'} -> case struct' of
  
    DataType {abstract, abstractTypes, inv, repinv, coupinv} -> do

      substitutionTable .= [mapType]
      currentStruct .= Just ast

      type' <- Just . StructureType False <$>
               mapM (toLLVMType . declType . snd) (toList structDecls)

      types <- mapM toLLVMType structTypes

      let
        name  = Name $ llvmName structName types
        structType = LLVM.NamedTypeReference name

      -- error . show $ llvmName structName types
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
    name' <- insertVar "self"
    -- Evaluate the condition expression
    cond <- expression expr
    -- Create both label
    trueLabel  <- newLabel "condTrue"
    falseLabel <- newLabel "condFalse"
    -- Create the conditional branch
    terminate' CondBr
      { condition = cond
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = [] }
    -- Set the false label to the warning, then continue normally
    (falseLabel #)
    
    case inv of 
      Invariant    -> abort Abort.Invariant pos
      RepInvariant -> abort Abort.RepInvariant pos
      
    -- And the true label to the next instructions
    (trueLabel #)

    terminate' $ Ret Nothing []
    closeScope

    blocks' <- use blocks
    blocks .= Seq.empty

    let selfParam = Parameter t name' []

    addDefinition $ GlobalDefinition functionDefaults
          { name        = Name procName
          , parameters  = ([selfParam],False)
          , returnType  = voidType
          , basicBlocks = toList blocks' }
