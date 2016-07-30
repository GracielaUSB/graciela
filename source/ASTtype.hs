module ASTtype where
--------------------------------------------------------------------------------
import           AST
import           Limits
import           ReduceAST
import           SymbolTable
import           Type
import           TypeError
import           VerTypes
--------------------------------------------------------------------------------
import           Control.Applicative      (liftA2)
import           Control.Monad.RWS.Strict (RWS, ask, evalRWS)
import           Data.Maybe               (fromJust)
import           Data.Range.Range         (Range (..), intersection, invert,
                                           union)
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
import           Text.Megaparsec.Pos      (SourcePos)
--------------------------------------------------------------------------------

runTVerifier :: SymbolTable -> AST -> (AST, Seq TypeError)
runTVerifier sTable sTree = evalRWS (verTypeAST sTree) sTable []


verTypeAST :: AST -> RWS SymbolTable (Seq TypeError) [String] AST
verTypeAST (AST.Program name pos defs accs _) = do
    defs' <- mapM verTypeAST defs
    accs' <- verTypeAST accs
    let defsT = map tag defs'
    let accsT = tag accs'
    checkT   <- verProgram defsT accsT
    return $ AST.Program name pos defs' accs' checkT


verTypeAST (Constant pos True  max _) =
    return $ Constant pos True  max GInt
verTypeAST (Constant pos False max _) =
    return $ Constant pos False max GFloat


verTypeAST (Arithmetic t pos lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verArithmetic (tag lexpr') (tag rexp') pos t
    return $ Arithmetic t pos lexpr' rexp' checkT


verTypeAST (Relational t pos lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verRelational (tag lexpr') (tag rexp') pos t
    return $ Relational t pos lexpr' rexp' checkT


verTypeAST (Boolean    t pos lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verBoolean (tag lexpr') (tag rexp') pos t
    return $ Boolean t pos lexpr' rexp' checkT


verTypeAST (Conversion t pos exp _) = do
    exp'   <- verTypeAST exp
    checkT <- verConversion t
    return $ Conversion t pos exp' checkT


verTypeAST (Unary op pos exp _) = do
    exp'   <- verTypeAST exp
    checkT <- verUnary op (tag exp') pos
    return $ Unary op pos exp' checkT


verTypeAST (Block pos st decs accs _) = do
    decs'  <- mapM verTypeAST decs
    accs'  <- mapM verTypeAST accs
    checkT <- verBlock (map tag accs')
    return $ Block pos st decs accs' checkT


verTypeAST (Skip  pos _) = return $ Skip  pos GEmpty
verTypeAST (Abort pos _) = return $ Abort pos GEmpty


verTypeAST (Ran var t pos _) = do
    checkT <- verRandom var t pos
    return $ Ran var t pos checkT


verTypeAST (Write ln exp pos _) = do
    exp'   <- verTypeAST exp
    checkT <- verWrite (tag exp')
    return $ Write ln exp' pos checkT


verTypeAST (ArrCall pos name args t) = do
    args'  <- mapM verTypeAST args
    checkT <- verArrayCall name (map tag args') t pos
    return $ ArrCall pos name args' checkT


verTypeAST (Guard exp action pos _) = do
    exp'    <- verTypeAST exp
    action' <- verTypeAST action
    checkT  <- verGuard (tag exp') (tag action') pos
    return $ Guard exp' action' pos checkT


verTypeAST (GuardExp exp action pos _) = do
    exp'    <- verTypeAST exp
    action' <- verTypeAST action
    checkT  <- verGuardExp (tag exp') (tag action') pos
    return $ GuardExp exp' action' pos checkT


verTypeAST (States t pos expr _) = do
    expr'  <- verTypeAST expr
    checkT <- verState (tag expr') pos t
    return $ States t pos expr' checkT


verTypeAST (GuardAction pos assert action _) = do
    assert' <- verTypeAST assert
    action' <- verTypeAST action
    checkT  <- verGuardAction (tag assert') (tag action')
    return $ GuardAction pos assert' action' checkT


verTypeAST (LAssign idlist explist pos _) = do
    explist' <- mapM verTypeAST explist
    idlist'  <- mapM verTypeAST idlist
    checkT   <- verLAssign (map (fromJust . astToId) idlist')
                 (map tag idlist') (map tag explist') (map position idlist)
    return $ LAssign idlist' explist' pos checkT


verTypeAST (Cond guard pos _) = do
    guard' <- mapM verTypeAST guard
    checkT <- verCond (map tag guard') pos
    return $ Cond guard' pos checkT


verTypeAST (Rept guard inv bound pos _) = do
    guard' <- mapM verTypeAST guard
    inv'   <- verTypeAST inv
    bound' <- verTypeAST bound
    checkT <- verRept (map tag guard') (tag inv') (tag bound')
    return $ Rept guard' inv' bound' pos checkT


verTypeAST (ProcCall name sb pos args _) = do
    args'  <- mapM verTypeAST args
    poss   <- getPosArgs args
    checkT <- verProcCall name sb args' pos poss
    sbc <- ask
    return (ProcCallCont name sb pos args' (myFromJust (lookUpRoot name sbc)) checkT)


verTypeAST (FCallExp name sb pos args _) = do
    args'  <- mapM verTypeAST args
    poss   <- getPosArgs args
    checkT <- verCallExp name sb (map tag args') pos poss
    return $ FCallExp name sb pos args' checkT


verTypeAST (DefFun name st pos body ret bound params _) = do
    body'  <- verTypeAST body
    bound' <- verTypeAST bound
    checkT <- verDefFun name (tag body') (tag bound') pos
    return $ DefFun name st pos body' ret bound' params checkT


verTypeAST (DefProc name st accs pre post bound cdec args _) = do
    accs'  <- verTypeAST accs
    pre'   <- verTypeAST pre
    post'  <- verTypeAST post
    bound' <- verTypeAST bound
    cdec'  <- mapM verTypeAST cdec
    checkT <- verDefProc (tag accs') (tag pre') (tag post') (tag bound') (map tag cdec')
    return $ DefProc name st accs' pre' post' bound' cdec' args checkT


verTypeAST (ConsAssign pos xs es t) = do
    es'    <- mapM verTypeAST es
    checkT <- verConsAssign xs pos (map tag es') t
    return $ ConsAssign pos xs es checkT


verTypeAST (Quant op var pos range term _) = do
    range' <- verTypeAST range
    term'  <- verTypeAST term
    if tag range' == GError || tag term' == GError
        then return $ Quant op var pos range' term' GError
        else do
            checkT <- verQuant op (tag range') (tag term') pos
            case checkT of
                GError   -> return (Quant op var pos range' term' checkT)
                _ -> do
                    let id = var
                    r <- occursCheck range id
                    if r
                        then case astToRange var range' of
                            Nothing ->
                                let rang = createRange var range
                                in return $ QuantRanUn op var pos rang term' checkT
                            Just r  -> return $ QuantRan op var pos r term' checkT

                        else do
                            addTypeError $ NotOccursVar op id pos
                            return $ Quant op var pos range' term' GError
verTypeAST ast = return ast

myFromJust (Just t) = t

astToRange :: Text -> AST -> Maybe [Range Integer]
astToRange id (Relational c _ l r _) =
    let lr = reduceAST id l
        rr = reduceAST id r
    in if lr == NonReducible || rr == NonReducible
         then Nothing
         else buildRange c lr rr


astToRange id (Boolean c _ l r _) =
    let lr = astToRange id l
        rr = astToRange id r
    in case c of
        Dis     -> liftA2 union lr rr
        Con     -> liftA2 intersection lr rr
        Implies -> liftA2 union (fmap invert lr) rr
        Conse   -> liftA2 union (fmap invert rr) lr


astToRange id (Unary Not _ e _) =
    let r = astToRange id e in fmap invert r


astToRange _ _ = Nothing


buildRange :: OpRel -> Reducibility -> Reducibility -> Maybe [Range Integer]
buildRange Less     (QuanVariable id) (Reducible n) = return $ return $ UpperBoundRange $ n - 1 -- i < n
buildRange Less     (Reducible n) (QuanVariable id) = return $ return $ LowerBoundRange $ n + 1 -- n < i
buildRange Greater  (QuanVariable id) (Reducible n) = return $ return $ LowerBoundRange $ n + 1 -- i > n
buildRange Greater  (Reducible n) (QuanVariable id) = return $ return $ UpperBoundRange $ n - 1 -- n > i
buildRange LEqual   (QuanVariable id) (Reducible n) = return $ return $ UpperBoundRange n -- i <= n
buildRange LEqual   (Reducible n) (QuanVariable id) = return $ return $ LowerBoundRange n -- n <= i
buildRange GEqual   (QuanVariable id) (Reducible n) = return $ return $ LowerBoundRange n -- i >= n
buildRange GEqual   (Reducible n) (QuanVariable id) = return $ return $ UpperBoundRange n -- n >= i
buildRange Ine      (Reducible n) (QuanVariable id) = return $ invert [SingletonRange n]
buildRange Ine      (QuanVariable id) (Reducible n) = return $ invert [SingletonRange n]
buildRange _ _ _                                    = return $ return InfiniteRange


getPosArgs :: [AST] -> MyVerType [SourcePos]
getPosArgs args = return $ fmap position args


--drawASTtype :: (AST, DS.Seq TypeError) -> String
--drawASTtype (ast, err) = case (DS.null err) of
--                         { True  ->  show ast
--                         ; False -> (show ast) ++ (drawTypeError err)
--                         }



createRange :: Text -> AST -> UnknownRange
createRange var (Boolean    op  _ lexp rexp _) =
    let l = createRange var lexp
        r = createRange var rexp
    in case op of
        Dis     -> SetRange Union    l r
        Con     -> SetRange Intersec l r
        Implies -> SetRange Union    l r --FALTAA
        Conse   -> SetRange Union    l r



createRange var (Relational op _ lexp rexp _) =
    let (l, r) = checkNode var op lexp rexp
    in case op of
        Less    -> TupleRange l r
        LEqual  -> TupleRange l r
        Greater -> TupleRange r l
        GEqual  -> TupleRange r l




checkNode :: Text -> OpRel -> AST -> AST -> (AST, AST)
checkNode id op lexp@(Id pos id' t) rexp =
    if id == id'
      then case op of
          Less -> (Int pos minInteger t, sub1 rexp)
          LEqual -> (Int pos minInteger t, rexp)
          Greater -> (Int pos maxInteger t, sum1 rexp)
          GEqual -> (Int pos maxInteger t, rexp)
      else
          let pos = position rexp
              ty  = tag rexp
          in case op of
              Less -> (sum1 lexp, Int pos maxInteger ty)
              LEqual -> (lexp, Int pos maxInteger ty)
              Greater -> (sub1 lexp, Int pos minInteger ty)
              GEqual -> (lexp, Int pos minInteger ty)

checkNode id op lexp (Id pos id' t) =
    case op of
        Less    -> (sum1 lexp, Int pos maxInteger t)
        LEqual  -> (lexp, Int pos maxInteger t)
        Greater -> (sub1 lexp, Int pos minInteger t)
        GEqual  -> (lexp, Int pos minInteger t)



sub1 :: AST -> AST
sub1 exp =
    let pos = position exp
        ty  = tag exp
    in Arithmetic Sub pos exp (Int pos 1 ty) ty


sum1 :: AST -> AST
sum1 exp =
    let pos = position exp
        ty  = tag exp
    in Arithmetic Sum pos exp (Int pos 1 ty) ty
