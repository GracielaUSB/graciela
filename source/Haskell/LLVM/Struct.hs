{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}

module LLVM.Struct
  ( defineStruct
  ) where
--------------------------------------------------------------------------------
import           AST.Declaration              (Declaration (..))
import           AST.Expression               (Expression (..))
import           AST.Struct                   (Struct (..), Struct' (..))
import           AST.Type                     as T
import           LLVM.Abort                   (abort, abortString)
import qualified LLVM.Abort                   as Abort (Abort (Invariant, RepInvariant))
import           LLVM.Definition
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                 (warn)
import qualified LLVM.Warning                 as Warning (Warning (Pre))
import           Location
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses, use, (%=), (+=),
                                               (.=))
import           Control.Monad                (forM_, when)
import           Data.Foldable                (toList)
import           Data.List                    (sortOn)
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

import           Debug.Trace

data Invariant = Invariant | RepInvariant | CoupInvariant deriving (Eq)

defineStruct :: Text -> (Struct, [T.TypeArgs]) -> LLVM ()
defineStruct structBaseName (ast, typeMaps) = case ast of

  Struct {structBaseName,structTypes, structFields, structProcs, struct'} -> case struct' of
    DataType {abstract, abstractTypes, inv, repinv, coupinv} ->
      forM_ typeMaps $ \typeMap -> do
        substitutionTable .= [typeMap]
        currentStruct .= Just ast

        type' <- Just . StructureType False <$>
                mapM  (toLLVMType . (\(_,x,_) -> x)) (sortOn (\(i,_,_) -> i) . toList $ structFields)

        types <- mapM toLLVMType structTypes
        let
          name  = Name $ llvmName structBaseName types
          structType = LLVM.NamedTypeReference name

        moduleDefs %= (|> TypeDefinition name type')

        defineStructInv Invariant structBaseName structType inv
        defineStructInv RepInvariant structBaseName structType repinv

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
