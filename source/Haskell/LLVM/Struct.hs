{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}

module LLVM.Struct
  ( defineStruct
  ) where
--------------------------------------------------------------------------------
import           AST.Declaration              (Declaration (..))
import           AST.Expression               (Expression' (..))
import           AST.Struct                   (Struct (..), Struct' (..))
import           AST.Type
import           LLVM.Abort                   (abort, abortString)
import qualified LLVM.Abort                   as Abort (Abort (Invariant, RepInvariant))
import           LLVM.Definition
import           LLVM.Expression
import           LLVM.Monad
import           LLVM.State
import           LLVM.Type
import           LLVM.Warning                 (warn)
import qualified LLVM.Warning                 as Warning (Warning (Invariant, Pre, RepInvariant))
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
import qualified LLVM.General.AST.Constant    as C (Constant (..))
import qualified LLVM.General.AST.Float       as LLVM (SomeFloat (Double))
import           LLVM.General.AST.Global      (Global (basicBlocks, name, parameters, returnType),
                                               functionDefaults)
import           LLVM.General.AST.Instruction (Instruction (..), Named (..),
                                               Terminator (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
import           LLVM.General.AST.Type        as LLVM
--------------------------------------------------------------------------------

import           Debug.Trace

data Invariant = Invariant | RepInvariant | CoupInvariant deriving (Eq)

defineStruct :: Text -> (Struct, [TypeArgs]) -> LLVM ()
defineStruct structBaseName (ast, typeMaps) = case ast of

  Struct {structBaseName,structTypes, structFields, structProcs, struct'} -> case struct' of
    DataType {abstract, abstractTypes, inv, repinv, coupinv} ->
      forM_ typeMaps $ \typeMap -> do
        substitutionTable .= [typeMap]
        currentStruct .= Just ast

        type' <- Just . StructureType True <$>
                mapM  (toLLVMType . (\(_,x,_) -> x)) (sortOn (\(i,_,_) -> i) . toList $ structFields)

        types <- mapM toLLVMType structTypes
        let
          name  = llvmName structBaseName types
          structType = LLVM.NamedTypeReference (Name name)

        moduleDefs %= (|> TypeDefinition (Name name) type')

        defaultConstructor name structType typeMap
        defineStructInv Invariant name structType inv
        defineStructInv RepInvariant name structType repinv

        mapM_ definition structProcs

        currentStruct .= Nothing

defaultConstructor :: String -> LLVM.Type -> TypeArgs -> LLVM ()
defaultConstructor name structType typeMap = do
  let
    procName = "init" <> name
  proc <- newLabel procName

  (proc #)

  Just Struct { structFields } <- use currentStruct

  openScope
  selfName <- insertVar "self"

  let
    self = LocalReference structType selfName

  forM_ (toList structFields) $ \(field, t, expr) -> do
    let
      filledT = fillType typeMap t
    when (filledT =:= GOneOf [GInt, GChar, GFloat, GBool, GPointer GAny]) $ do

      member <- newLabel $ "member" <> show field

      addInstruction $ member := GetElementPtr
          { inBounds = False
          , address  = self
          , indices  = ConstantOperand . C.Int 32 <$> [0, field]
          , metadata = []}

      defaultValue <- case expr of
        Nothing -> value filledT
        Just e -> expression e

      t' <- toLLVMType filledT
      addInstruction $ Do Store
          { volatile = False
          , address  = LocalReference t' member
          , value    = defaultValue
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = []
          }
    pure ()
  terminate $ Ret Nothing []
  closeScope

  blocks' <- use blocks
  blocks .= Seq.empty

  let selfParam = Parameter (ptr structType) selfName []

  addDefinition $ GlobalDefinition functionDefaults
        { name        = Name procName
        , parameters  = ([selfParam],False)
        , returnType  = voidType
        , basicBlocks = toList blocks' }

  where
    value t = case t of
      GBool    -> pure . ConstantOperand $ C.Int 1 0
      GChar    -> pure . ConstantOperand $ C.Int 8 0
      GInt     -> pure . ConstantOperand $ C.Int 32 0
      GFloat   -> pure . ConstantOperand . C.Float $ LLVM.Double 0
      t@(GPointer _) -> ConstantOperand . C.Null  <$> toLLVMType t


defineStructInv :: Invariant
                -> String
                -> LLVM.Type
                -> Expression
                -> LLVM ()
defineStructInv inv name t expr@ Expression {loc = Location(pos,_)}
  | inv == CoupInvariant = undefined
  | otherwise = do

    let
      procName = (<> name) (case inv of
          Invariant -> "inv-"
          RepInvariant -> "repInv-")

    proc <- newLabel $ "proc" <> procName
    (proc #)

    openScope
    name' <- insertVar "self"
    -- Evaluate the condition expression
    condInv <- expression expr
    -- Create both label
    trueLabel  <- newLabel "condTrue"
    falseLabel <- newLabel "condFalse"

    precondTrue  <- newLabel "precondTrue"
    precondFalse <- newLabel "precondFalse"
    -- Create the conditional branch
    terminate CondBr
      { condition = condInv
      , trueDest  = trueLabel
      , falseDest = falseLabel
      , metadata' = [] }
    -- Set the false label to the warning, then continue normally
    (falseLabel #)

    terminate CondBr
      { condition = LocalReference boolType (Name "cond")
      , trueDest  = precondTrue
      , falseDest = precondFalse
      , metadata' = [] }

    (precondTrue #)
    case inv of
      Invariant    -> abort Abort.Invariant pos
      RepInvariant -> abort Abort.RepInvariant pos

    (precondFalse #)

    case inv of
      Invariant    -> warn Warning.Invariant pos
      RepInvariant -> warn Warning.RepInvariant pos

    terminate Br
        { dest      = trueLabel
        , metadata' = [] }

    -- And the true label to the next instructions
    (trueLabel #)

    terminate $ Ret Nothing []
    closeScope

    blocks' <- use blocks
    blocks .= Seq.empty

    let
      selfParam    = Parameter (ptr t) name' []
      precondParam = Parameter boolType (Name "cond") []

    addDefinition $ GlobalDefinition functionDefaults
          { name        = Name procName
          , parameters  = ([selfParam, precondParam],False)
          , returnType  = voidType
          , basicBlocks = toList blocks' }
