module ASTtype where
--------------------------------------------------------------------------------
import           AST
import           Limits
import           Location
import           MyTypeError
import           ReduceAST
import           SymbolTable
import           Type
import           TypeState
import           VerTypes
--------------------------------------------------------------------------------
import           Control.Applicative      (liftA2)
import           Control.Monad.RWS.Strict (RWS, ask, evalRWS)
import           Data.Maybe               (fromJust)
import           Data.Range.Range         (Range (..), intersection, invert,
                                           union)
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
--------------------------------------------------------------------------------

runTVerifier :: SymbolTable -> AST Type -> (AST Type, Seq MyTypeError)
runTVerifier sTable sTree = evalRWS (verTypeAST sTree) sTable []


verTypeAST :: AST Type -> RWS SymbolTable (Seq MyTypeError) [String] (AST Type)
verTypeAST (AST.Program name loc defs accs _) = do
    defs' <- mapM verTypeAST defs
    accs' <- verTypeAST accs
    let defsT = map tag defs'
    let accsT = tag accs'
    checkT   <- verProgram defsT accsT
    return $ AST.Program name loc defs' accs' checkT


verTypeAST (Constant loc True  max _) =
    return $ Constant loc True  max GInt
verTypeAST (Constant loc False max _) =
    return $ Constant loc False max GFloat


verTypeAST (Arithmetic t loc lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verArithmetic (tag lexpr') (tag rexp') loc t
    return $ Arithmetic t loc lexpr' rexp' checkT


verTypeAST (Relational t loc lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verRelational (tag lexpr') (tag rexp') loc t
    return $ Relational t loc lexpr' rexp' checkT


verTypeAST (Boolean    t loc lexpr rexp _) = do
    lexpr' <- verTypeAST lexpr
    rexp'  <- verTypeAST rexp
    checkT <- verBoolean (tag lexpr') (tag rexp') loc t
    return $ Boolean t loc lexpr' rexp' checkT


verTypeAST (Conversion t loc exp _) = do
    exp'   <- verTypeAST exp
    checkT <- verConversion t
    return $ Conversion t loc exp' checkT


verTypeAST (Unary op loc exp _) = do
    exp'   <- verTypeAST exp
    checkT <- verUnary op (tag exp') loc
    return $ Unary op loc exp' checkT


verTypeAST (Block loc st decs accs _) = do
    decs'  <- mapM verTypeAST decs
    accs'  <- mapM verTypeAST accs
    checkT <- verBlock (map tag accs')
    return $ Block loc st decs accs' checkT


verTypeAST (Skip  loc _) = return $ Skip  loc GEmpty
verTypeAST (Abort loc _) = return $ Abort loc GEmpty


verTypeAST (Ran var t loc _) = do
    checkT <- verRandom var t loc
    return $ Ran var t loc checkT


verTypeAST (Write ln exp loc _) = do
    exp'   <- verTypeAST exp
    checkT <- verWrite (tag exp')
    return $ Write ln exp' loc checkT


verTypeAST (ArrCall loc name args t) = do
    args'  <- mapM verTypeAST args
    checkT <- verArrayCall name (map tag args') t loc
    return $ ArrCall loc name args' checkT


verTypeAST (Guard exp action loc _) = do
    exp'    <- verTypeAST exp
    action' <- verTypeAST action
    checkT  <- verGuard (tag exp') (tag action') loc
    return $ Guard exp' action' loc checkT


verTypeAST (GuardExp exp action loc _) = do
    exp'    <- verTypeAST exp
    action' <- verTypeAST action
    checkT  <- verGuardExp (tag exp') (tag action') loc
    return $ GuardExp exp' action' loc checkT


verTypeAST (States t loc expr _) = do
    expr'  <- verTypeAST expr
    checkT <- verState (tag expr') loc t
    return $ States t loc expr' checkT


verTypeAST (GuardAction loc assert action _) = do
    assert' <- verTypeAST assert
    action' <- verTypeAST action
    checkT  <- verGuardAction (tag assert') (tag action')
    return $ GuardAction loc assert' action' checkT


verTypeAST (LAssign idlist explist loc _) = do
    explist' <- mapM verTypeAST explist
    idlist'  <- mapM verTypeAST idlist
    checkT   <- verLAssign (map (fromJust . astToId) idlist')
                 (map tag idlist') (map tag explist') (map AST.location idlist)
    return $ LAssign idlist' explist' loc checkT


verTypeAST (Cond guard loc _) = do
    guard' <- mapM verTypeAST guard
    checkT <- verCond (map tag guard') loc
    return $ Cond guard' loc checkT


verTypeAST (Rept guard inv bound loc _) = do
    guard' <- mapM verTypeAST guard
    inv'   <- verTypeAST inv
    bound' <- verTypeAST bound
    checkT <- verRept (map tag guard') (tag inv') (tag bound')
    return $ Rept guard' inv' bound' loc checkT


verTypeAST (ProcCall name sb loc args _) = do
    args'  <- mapM verTypeAST args
    locs   <- getLocArgs args
    checkT <- verProcCall name sb args' loc locs
    sbc <- ask
    return (ProcCallCont name sb loc args' (myFromJust (lookUpRoot name sbc)) checkT)


verTypeAST (FCallExp name sb loc args _) = do
    args'  <- mapM verTypeAST args
    locs   <- getLocArgs args
    checkT <- verCallExp name sb (map tag args') loc locs
    return $ FCallExp name sb loc args' checkT


verTypeAST (DefFun name st loc body ret bound params _) = do
    body'  <- verTypeAST body
    bound' <- verTypeAST bound
    checkT <- verDefFun name (tag body') (tag bound') loc
    return $ DefFun name st loc body' ret bound' params checkT


verTypeAST (DefProc name st accs pre post bound cdec args _) = do
    accs'  <- verTypeAST accs
    pre'   <- verTypeAST pre
    post'  <- verTypeAST post
    bound' <- verTypeAST bound
    cdec'  <- mapM verTypeAST cdec
    checkT <- verDefProc (tag accs') (tag pre') (tag post') (tag bound') (map tag cdec')
    return $ DefProc name st accs' pre' post' bound' cdec' args checkT


verTypeAST (ConsAssign loc xs es t) = do
    es'    <- mapM verTypeAST es
    checkT <- verConsAssign xs loc (map tag es') t
    return $ ConsAssign loc xs es checkT


verTypeAST (Quant op var loc range term _) = do
    range' <- verTypeAST range
    term'  <- verTypeAST term
    if tag range' == GError || tag term' == GError
        then return $ Quant op var loc range' term' GError
        else do
            checkT <- verQuant op (tag range') (tag term') loc
            case checkT of
                GError   -> return (Quant op var loc range' term' checkT)
                _ -> do
                    let id = var
                    r <- occursCheck range id
                    if r
                        then case astToRange var range' of
                            Nothing ->
                                let rang = createRange var range
                                in return $ QuantRanUn op var loc rang term' checkT
                            Just r  -> return $ QuantRan op var loc r term' checkT

                        else do
                            addNotOccursVarError op id loc
                            return $ Quant op var loc range' term' GError
verTypeAST ast = return ast

myFromJust (Just t) = t

astToRange :: Text -> AST Type -> Maybe [Range Integer]
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


getLocArgs :: [AST Type] -> MyVerType [Location]
getLocArgs args = return $ fmap AST.location args


--drawASTtype :: (AST Type, DS.Seq MyTypeError) -> String
--drawASTtype (ast, err) = case (DS.null err) of
--                         { True  ->  show ast
--                         ; False -> (show ast) ++ (drawTypeError err)
--                         }



createRange :: Text -> AST Type -> UnknownRange
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




checkNode :: Text -> OpRel -> AST Type -> AST Type -> (AST Type, AST Type)
checkNode id op lexp@(ID loc id' t) rexp =
    if id == id'
      then case op of
          Less -> (Int loc minInteger t, sub1 rexp)
          LEqual -> (Int loc minInteger t, rexp)
          Greater -> (Int loc maxInteger t, sum1 rexp)
          GEqual -> (Int loc maxInteger t, rexp)
      else
          let loc = location rexp
              ty  = tag rexp
          in case op of
              Less -> (sum1 lexp, Int loc maxInteger ty)
              LEqual -> (lexp, Int loc maxInteger ty)
              Greater -> (sub1 lexp, Int loc minInteger ty)
              GEqual -> (lexp, Int loc minInteger ty)

checkNode id op lexp (ID loc id' t) =
    case op of
        Less    -> (sum1 lexp, Int loc maxInteger t)
        LEqual  -> (lexp, Int loc maxInteger t)
        Greater -> (sub1 lexp, Int loc minInteger t)
        GEqual  -> (lexp, Int loc minInteger t)



sub1 :: AST Type -> AST Type
sub1 exp =
    let loc = location exp
        ty  = tag exp
    in Arithmetic Sub loc exp (Int loc 1 ty) ty


sum1 :: AST Type -> AST Type
sum1 exp =
    let loc = location exp
        ty  = tag exp
    in Arithmetic Sum loc exp (Int loc 1 ty) ty
