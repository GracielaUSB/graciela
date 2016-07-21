module VerTypes where
--------------------------------------------------------------------------------
import           MyTypeError
import           SymbolTable
import           Data.Maybe
import           Location
import           Contents
import           Type
import           AST
--------------------------------------------------------------------------------

import           Data.Tree 
import           Treelike
import           Debug.Trace
import           Data.Sequence            (Seq, singleton)
import           Data.List                (zip4)
import           Control.Monad.RWS.Strict (RWS, ask, tell)
import           Data.Text                (Text)
--------------------------------------------------------------------------------


-- | Tipo del Monad el cual contiene, la tabla de simbolos, una secuencia de errores y una lista de strings,
-- | usados para el manejo de los errores a momento de ejecucion
type MyVerType a = RWS (SymbolTable) (Seq (MyTypeError)) ([String]) a


addTypeError :: MyTypeError -> MyVerType Type
addTypeError error = do tell $ singleton error
                        return $ GError


checkListType :: Type -> Bool -> Type -> Bool
checkListType _ False _ = False
checkListType x True  y = x == y

checkError :: Type -> Type -> Type
checkError GError _ = GError
checkError acc t =
    let ver = verType acc t
    in if ver == GError
        then GError
        else ver

verType :: Type -> Type -> Type
verType GError _ = GError
verType _ GError = GError
verType x y       = if x == y then x else GEmpty


verArithmetic :: Type -> Type -> Location -> OpNum -> MyVerType Type
verArithmetic ltype rtype loc op = 
    case verType ltype rtype of
        GInt   -> return GInt
        GFloat -> return GFloat
        GError -> return GError
        _      -> addTypeError $ ArithmeticError ltype rtype op loc


verBoolean :: Type -> Type -> Location -> OpBool -> MyVerType Type
verBoolean ltype rtype loc op =
    case verType ltype rtype of
        GBoolean -> return GBoolean
        GError   -> return GError
        _        -> addTypeError $ BooleanError ltype rtype op loc


verRelational :: Type -> Type -> Location -> OpRel -> MyVerType Type
verRelational ltype rtype loc op =
    case verType ltype rtype of
        GError   -> return GError
        GEmpty   -> addTypeError $ RelationalError ltype rtype op loc
        _        -> return GBoolean


verConversion :: Conv -> MyVerType Type
verConversion ToInt    = return GInt
verConversion ToDouble = return GFloat
verConversion ToChar   = return GChar


verWrite :: Type -> MyVerType Type
verWrite  GError = return GError
verWrite  _      = return GEmpty


verUnary :: OpUn -> Type -> Location -> MyVerType Type
verUnary _     GError _   = return GError

verUnary Minus GInt     loc = return GInt
verUnary Minus GFloat   loc = return GFloat
verUnary Minus errType  loc = addTypeError $ UnaryError errType Minus loc

verUnary Not   GBoolean loc = return GBoolean
verUnary Not   errType  loc = addTypeError $ UnaryError errType Not loc

verUnary Abs   GInt     loc = return GInt
verUnary Abs   GFloat   loc = return GFloat
verUnary Abs   errType  loc = addTypeError $ UnaryError errType Abs loc

verUnary Sqrt  GInt     loc = return GFloat
verUnary Sqrt  GFloat   loc = return GFloat
verUnary Sqrt  errType  loc = addTypeError $ UnaryError errType Sqrt loc


verGuardAction :: Type -> Type -> MyVerType Type
verGuardAction assert action =
    if assert == GBoolean && action == GEmpty
        then return GEmpty
        else return GError


verGuard :: Type -> Type -> Location -> MyVerType Type
verGuard exp action loc =
    case action of
        GError -> return GError
        GEmpty ->
            case exp of
                GError   -> return GError
                GBoolean -> return GEmpty
                _        -> addTypeError $ GuardError exp loc


verGuardExp :: Type -> Type -> Location -> MyVerType Type
verGuardExp exp action loc =
    case action of
        GError   -> return GError
        _        -> case exp of
            GError   -> return GError
            GBoolean -> return action
            _        -> addTypeError $ GuardError exp loc


verDefProc :: Type -> Type -> Type -> Type -> [Type] -> MyVerType Type
verDefProc accs pre post bound decs =
    if pre == GBoolean && post == GBoolean && accs == GEmpty && all (== GEmpty) decs
        then return GEmpty
        else return GError


verBlock :: [Type] -> MyVerType Type
verBlock accs =
    if foldl (checkListType GEmpty) True accs
        then return GEmpty
        else return GError


verProgram :: [Type] -> Type -> MyVerType Type
verProgram defs accs =
    if foldl (checkListType GEmpty) True defs && accs == GEmpty
        then return GEmpty
        else return GError


verCond :: [Type] -> Location -> MyVerType Type
verCond guards loc =
    let checkSame  acc t = if acc == t then acc else GError
        checkT     = foldl1 checkSame guards
    in case foldl checkError GEmpty guards of
        GError   -> return GError
        _        -> case checkT of
            GError -> addTypeError $ CondError loc
            _      -> return checkT


verState :: Type -> Location -> StateCond -> MyVerType Type
verState expr loc stateCond =
    case expr of
        GError   -> return GError
        _        -> if expr == checkT
                        then return expr
                        else addTypeError $ StateError expr stateCond loc
        where 
            checkT = case stateCond of
                        Bound -> GInt
                        _     -> GBoolean

verRept :: [Type] -> Type -> Type -> MyVerType Type
verRept guard inv bound =
    let func = checkListType GEmpty
    in if foldl func True guard && inv == GBoolean && bound == GInt
        then return GEmpty
        else return GError



verRandom :: Text -> Type -> Location -> MyVerType Type
verRandom name t loc =
    if t == GInt || t == GFloat
        then return t
        else addTypeError $ RanError name t loc


verQuant :: OpQuant -> Type -> Type -> Location -> MyVerType Type
verQuant op range term loc = case range of
    GBoolean -> case op of
        ForAll    -> if term == GBoolean
            then return GBoolean
            else addTypeError $ QuantBoolError  op term loc

        Exists    -> if term == GBoolean
            then return GBoolean
            else addTypeError $ QuantBoolError  op term loc

        Product   -> if term == GInt || term == GFloat
            then return term
            else addTypeError $ QuantIntError op term loc

        Summation -> if term == GInt || term == GFloat
            then return term
            else addTypeError $ QuantIntError op term loc

        Maximum   -> if term == GInt || term == GFloat
            then return term
            else addTypeError $ QuantIntError op term loc

        Minimum   -> if term == GInt || term == GFloat
            then return term
            else addTypeError $ QuantIntError op term loc

    _ -> addTypeError $ QuantRangeError op range loc


verConsAssign :: [(Text, Location)] -> Location -> [Type] -> Type -> MyVerType Type
verConsAssign xs loc ts t =
    let f ((id, loc'), t') =
            if t' /= t
                then if t' == GError
                    then return GError
                    else addTypeError $ TypeDecError id loc' t' t
                else return GEmpty
    in if length xs /= length ts
        then addTypeError $ DiffSizeError loc
        else do
            r <- fmap (all (== GEmpty)) (mapM f (zip xs ts))
            if r
                then return GEmpty
                else return GError


verCallExp :: Text -> SymbolTable -> [Type] -> Location -> [Location] -> MyVerType Type
verCallExp name sbc args loc locarg = do
    sb <- ask
    case lookUpRoot name sb of
        Nothing -> addTypeError $ UndecFunError name True loc
        Just (FunctionCon _ _ t ln sb) -> case t of
                GFunction args' ts -> do 
                    let wtL = length args
                    let prL = length args'
                    if wtL /= prL
                        then addTypeError $ NumberArgsError name True wtL prL loc
                        else do 
                            let t = zip args args'
                            if all (uncurry (==)) t
                                then do
                                    r <- validFuncArgs ln args locarg sb sbc
                                    if r
                                        then return ts
                                        else return GError

                                else do
                                    mapM_ (\ ((arg, arg'), larg) ->
                                        if arg /= arg'
                                            then addTypeError $ FunArgError name True arg' arg larg
                                            else return GEmpty)
                                        (zip t locarg)
                                    return GError
                _ -> addTypeError $ UndecFunError name True loc

        _ -> addTypeError $ UndecFunError name True loc


----------------------------------------------------
validFuncArgs :: [Text] -> [Type] -> [Location] -> SymbolTable -> SymbolTable -> MyVerType Bool
validFuncArgs lnp lnc locarg sbp sbc = return True
---------------------------------------------------------------------


verProcCall :: Text -> SymbolTable -> [AST Type] -> Location -> [Location] -> MyVerType Type
verProcCall name sbc args'' loc locarg = do
    sb <- ask
    case lookUpRoot name sb of
        Nothing -> addTypeError $ UndecFunError name False loc -- Error por procedimiento no declarado
        Just (ProcCon _ _ t ln sb) -> case t of
            GProcedure args' -> do
                let wtL = length args''
                let prL = length args'
                if wtL /= prL
                    then addTypeError $ NumberArgsError name False wtL prL loc -- Error porque el numero de parametros en la llamada
                                                                               -- es distinto al de la declaracion
                    else do
                        let args = map tag args''
                        let t    = zip args args'
                        if all (uncurry (==)) t
                            then do
                                r <- validProcArgs name ln args'' locarg sb sbc
                                if r
                                    then return GEmpty
                                    else return GError
                            else do
                                mapM_ (\ ((arg, arg'), larg) ->
                                    if arg /= arg'
                                        then addTypeError $ FunArgError name False arg' arg larg -- Error porque los tipos uno o mas parametros
                                                                                                    -- en la llamada y declaracion no coincidieron
                                        else return GEmpty
                                    ) (zip t locarg)
                                return GError

            _ -> addTypeError $ UndecFunError name False loc -- Error por procedimiento no declarado



validProcArgs :: Text -> [Text] -> [AST Type] -> [Location] -> SymbolTable -> SymbolTable -> MyVerType Bool
validProcArgs name lnp lnc locarg sbp sbc =
    let lat = map ((getProcArgType . fromJust) . flip checkSymbol sbp) lnp
        lvt = map (isASTLValue sbc) lnc
        xs  = zip lat lvt
    in and <$> mapM compare (zip xs (zip lnc locarg))
    where
        compare ((Just Out, False), (id, loc))   =
            do addTypeError $ InvalidPar name id loc
               return False
        compare ((Just InOut, False), (id, loc)) =
            do addTypeError $ InvalidPar name id loc
               return False
        compare ((Just Ref, False), (id, loc))   =
            do addTypeError $ InvalidPar name id loc
               return False
        compare _                                =
               return True


isASTLValue :: SymbolTable -> AST a -> Bool
isASTLValue sb id =
    case astToId id of
        Nothing -> False
        Just t  ->
            case checkSymbol t sb of
                Nothing -> False -- Esto es un error grave, significa que una variable sin
                                 -- verificacion de contexto llego a la verificacion de tipos
                Just c  -> isLValue c



addLAssignError:: [Bool] -> [(Text, Type, Type, Location)] -> MyVerType Type
addLAssignError (res:rs) ((name, op1, op2, loc):xs)
    | res = addLAssignError rs xs
    | op1 == GError || op2 == GError = addLAssignError rs xs
    | otherwise = do
        addTypeError $ AssignError  name op1 op2 loc
        addLAssignError rs xs

addLAssignError [] [] = return GError


verLAssign :: [Text] -> [Type] -> [Type] -> [Location] -> MyVerType Type
verLAssign ids idlist explist locs =
    if length idlist /= length explist then
        addTypeError $ DiffSizeError $ head locs
    else
        let res = zipWith (==) idlist explist
        in if and res
            then return GEmpty
            else addLAssignError res $ zip4 ids idlist explist locs


    -- do check <- RWSS.foldM (addLAssignError loc) [] $ zip (zip idlist expArrT) explist
    --    let checkError' = (\acc t -> if not(acc == GError) && not(t == GError) then GEmpty else GError)
    --    case (foldl1 checkError' explist) of
    --    { GError   -> return GError
    --    ; otherwise -> case check of
    --                   { []        -> return GEmpty
    --                   ; otherwise -> do mapM_ addListError check
    --                                     return GError
    --                   }
    --    }


getArrayType :: Int -> Type -> Type
getArrayType 0 t = t
getArrayType n t = getArrayType (n-1) (arrayType t)

verArrayCall :: Text -> [Type] -> Type -> Location -> MyVerType Type
verArrayCall name args t loc = do 
    let waDim = getDimension t
    let prDim = length args
    if waDim >= prDim
        then case foldl checkError GInt args of
            GError   -> return GError
            GInt     -> return $ getArrayType prDim t
            _ -> do mapM_ addTypeError check
                    return GError
        else addTypeError $ ArrayDimError name waDim prDim loc
        where 
            addError acc expT = 
                if checkListType GInt True expT
                    then acc
                    else acc ++ [ArrayCallError name expT loc]
            check = foldl addError [] args

verDefFun :: Text -> Type -> Type -> Location -> MyVerType Type
verDefFun name body bound loc = do
    sb <- ask
    case lookUpRoot name sb of
        Nothing -> addTypeError $ UndecFunError name True loc
        Just c@(FunctionCon _ _ t _ _)  ->
            case t of
                GFunction _ tf ->
                    if tf == body
                        then return GEmpty
                        else addTypeError $ RetFuncError name tf body loc
                _       -> addTypeError $ UndecFunError name True loc
        _      -> addTypeError $ UndecFunError name True loc

