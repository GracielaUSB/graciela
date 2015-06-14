module ASTtype where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Applicative      as AP
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import Data.Range.Range                   as RA
import MyParseError                       as PE
import MyTypeError                        as PT
import Prelude                            as P
import SymbolTable
import Data.Char
import TypeState
import VerTypes
import Type 
import AST
import Token


runTVerifier :: SymbolTable -> AST Type -> (AST Type, DS.Seq MyTypeError)
runTVerifier sTable sTree = RWSS.evalRWS (verTypeAST sTree) sTable () 


verTypeAST :: (AST Type) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) () (AST Type)
verTypeAST (AST.Program name loc defs accs _) = do defs'    <- verTypeASTlist defs
                                                   accs'    <- verTypeASTlist accs
                                                   let defsT = map tag defs'
                                                   let accsT = map tag accs'
                                                   checkT   <- verProgram defsT accsT
                                                   return (AST.Program name loc defs' accs' checkT)


verTypeAST (Constant loc True  max _) = return (Constant loc True  max MyInt  )
verTypeAST (Constant loc False max _) = return (Constant loc False max MyFloat)


verTypeAST (Arithmetic t loc lexpr rexp _) = do lexpr' <- verTypeAST lexpr
                                                rexp'  <- verTypeAST rexp  
                                                checkT <- verArithmetic (tag lexpr') (tag rexp') loc t 
                                                return (Arithmetic t loc lexpr' rexp' checkT)


verTypeAST (Relational t loc lexpr rexp _) = do lexpr' <- verTypeAST lexpr
                                                rexp'  <- verTypeAST rexp  
                                                checkT <- verRelational (tag lexpr') (tag rexp') loc t
                                                return (Relational t loc lexpr' rexp' checkT)
                                                         

verTypeAST (Boolean    t loc lexpr rexp _) = do lexpr' <- verTypeAST lexpr
                                                rexp'  <- verTypeAST rexp  
                                                checkT <- verBoolean (tag lexpr') (tag rexp') loc t
                                                return (Boolean t loc lexpr' rexp' checkT)
                                                         

verTypeAST (Convertion t loc exp _) = do exp'   <- verTypeAST (exp) 
                                         checkT <- verConvertion t
                                         return (Convertion t loc exp' checkT)


verTypeAST (Unary op loc exp _) = do exp'   <- verTypeAST (exp) 
                                     checkT <- verUnary op (tag exp') loc
                                     return (Unary op loc exp' checkT)


verTypeAST (Block accs loc _) = do accs'  <- verTypeASTlist accs
                                   checkT <- verBlock (map tag accs')
                                   return (Block accs' loc checkT)


verTypeAST (Skip    loc _) = return (Skip  loc MyEmpty)
verTypeAST (Abort   loc _) = return (Abort loc MyEmpty)


verTypeAST (Ran var loc t) = do checkT <- verRandom t loc
                                return (Ran var loc checkT)


verTypeAST (Write ln exp loc _) = do exp'   <- verTypeAST (exp) 
                                     checkT <- verWrite (tag exp')
                                     return (Write ln exp' loc checkT)


verTypeAST (ArrCall loc name args t) = do args'  <- verTypeASTlist args
                                          checkT <- verArrayCall name (map tag args') t loc 
                                          return (ArrCall loc name args' checkT)    


verTypeAST (Guard exp action loc _) = do exp'    <- verTypeAST exp     
                                         action' <- verTypeAST action
                                         checkT  <- verGuard (tag exp') (tag action') loc
                                         return (Guard exp' action' loc checkT)
                                                  

verTypeAST (GuardExp exp action loc _) = do exp'    <- verTypeAST exp     
                                            action' <- verTypeAST action
                                            checkT  <- verGuardExp (tag exp') (tag action') loc  
                                            return (GuardExp exp' action' loc checkT)
                                                


verTypeAST (States t loc expr _) = do expr'  <- verTypeAST expr
                                      checkT <- verState (tag expr') loc t 
                                      return (States t loc expr' checkT)    


verTypeAST (GuardAction loc assert action _) = do assert' <- verTypeAST assert  
                                                  action' <- verTypeAST action
                                                  checkT  <- verGuardAction (tag assert') (tag action')
                                                  return (GuardAction loc assert' action' checkT)
                                                           


verTypeAST (LAssign idlist explist loc _) = do explist' <- verTypeASTlist explist  
                                               expArrT  <- mapM verTypeASTlist (map snd idlist)
                                               checkT   <- verLAssign (map tag explist') (map fst idlist) (fmap (map tag) expArrT) loc
                                               return (LAssign idlist explist' loc checkT)


verTypeAST (Cond guard loc _) = do guard' <- verTypeASTlist guard  
                                   checkT <- verCond (map tag guard') loc
                                   return (Cond guard' loc checkT)


verTypeAST (Rept guard inv bound loc _) = do guard' <- verTypeASTlist guard
                                             inv'   <- verTypeAST inv  
                                             bound' <- verTypeAST bound
                                             checkT <- verRept (map tag guard') (tag inv') (tag bound')
                                             return (Rept guard' inv' bound' loc checkT)
                                                      

verTypeAST (ProcCall name args loc _) = do args'  <- verTypeASTlist args  
                                           locs   <- getLocArgs args
                                           checkT <- verProcCall name (map tag args') loc locs
                                           return (ProcCall name args' loc checkT)

  
verTypeAST (FunBody loc exp _) = do exp' <- verTypeAST (exp)
                                    return (FunBody loc exp' (tag exp'))


verTypeAST (FCallExp loc name args _) = do args'  <- verTypeASTlist args
                                           locs   <- getLocArgs args
                                           checkT <- verCallExp name (map tag args') loc locs
                                           return (FCallExp loc name args' checkT)

verTypeAST (DefFun name loc body bound _) = do body'  <- verTypeAST body   
                                               bound' <- verTypeAST bound
                                               checkT <- verDefFun name (tag body') (tag bound') loc
                                               return (DefFun name loc body' bound' checkT)
                                                    

verTypeAST (DefProc name accs pre post bound _) = do accs'  <- verTypeASTlist accs 
                                                     pre'   <- verTypeAST pre  
                                                     post'  <- verTypeAST post 
                                                     bound' <- verTypeAST bound
                                                     checkT <- verDefProc (map tag accs') (tag pre'  )
                                                                          (tag     post') (tag bound')
                                                     return (DefProc name accs' pre' post' bound' checkT)
                                                              

verTypeAST ((Quant op var loc range term _)) = 
    do range' <- verTypeAST range  
       term'  <- verTypeAST term 
       checkT <- verQuant (tag range') (tag term')
       case checkT of
         MyError   -> return (Quant op var loc range' term' checkT)
         otherwise -> let id = var in
                      do r <- occursCheck range id
                         case r of 
                           True  -> case astToRange var range' of
                                           Nothing -> return $ Quant op var loc range' term' checkT
                                           Just r  -> case r of 
                                                         []   -> return $ Quant op var loc range' term' MyError
                                                         xs   -> return $ QuantRan op var loc xs term' checkT
                           False -> do addNotOccursVarError id loc
                                       return $ Quant op var loc range' term' MyError

      
verTypeAST ast = return $ ast


--Rango

astToRange id (Relational c _ l r _) = 
    let lr = reduceAST id l
        rr = reduceAST id r
    in
        if lr == NonReducible || rr == NonReducible then Nothing
        else buildRange c lr rr

astToRange id (Boolean c _ l r _) = 
    let lr = astToRange id l
        rr = astToRange id r
    in
      case c of
        Dis     -> AP.liftA2 RA.union lr rr
        Con     -> AP.liftA2 RA.intersection lr rr
        Implies -> AP.liftA2 RA.union (fmap RA.invert lr) rr
        Conse   -> AP.liftA2 RA.union (fmap RA.invert rr) lr
astToRange id (Unary Not _ e _) = 
    let r = astToRange id e
    in
      fmap RA.invert r
astToRange _ _ = Nothing


buildRange Less     (QuanVariable id) (Reducible n) = return $ return $ RA.UpperBoundRange $ n - 1 -- i < n
buildRange Less     (Reducible n) (QuanVariable id) = return $ return $ RA.LowerBoundRange $ n + 1 -- n < i
buildRange Greater  (QuanVariable id) (Reducible n) = return $ return $ RA.LowerBoundRange $ n + 1 -- i > n
buildRange Greater  (Reducible n) (QuanVariable id) = return $ return $ RA.UpperBoundRange $ n - 1 -- n > i
buildRange LEqual   (QuanVariable id) (Reducible n) = return $ return $ RA.UpperBoundRange n -- i <= n
buildRange LEqual   (Reducible n) (QuanVariable id) = return $ return $ RA.LowerBoundRange n -- n <= i
buildRange GEqual   (QuanVariable id) (Reducible n) = return $ return $ RA.LowerBoundRange n -- i >= n
buildRange GEqual   (Reducible n) (QuanVariable id) = return $ return $ RA.UpperBoundRange n -- n >= i
buildRange Equal    (Reducible n) (QuanVariable id) = return $ return $ RA.SingletonRange n
buildRange Equal    (QuanVariable id) (Reducible n) = return $ return $ RA.SingletonRange n
buildRange Ine      (Reducible n) (QuanVariable id) = return $ RA.invert $ [RA.SingletonRange n]
buildRange Ine      (QuanVariable id) (Reducible n) = return $ RA.invert $ [RA.SingletonRange n]
buildRange _ _ _ = return $ return $ InfiniteRange


reduceAST id (Arithmetic op _ l r _)  = 
    let lr = reduceAST id l
        rr = reduceAST id r
    in
      if       lr == NonReducible    || rr == NonReducible    then NonReducible
      else  if lr == QuanVariable id || rr == QuanVariable id then NonReducible
            else
              let nl = getNum lr
                  nr = getNum rr
              in
                case op of
                  Sum -> Reducible (nl + nr)
                  Sub -> Reducible (nl - nr)
                  Mul -> Reducible (nl * nr)
                  Div -> Reducible (quot nl nr)
                  Exp -> Reducible (nl ^ nr)
                  Max -> Reducible (P.max nl nr)
                  Min -> Reducible (min nl nr)
                  Mod -> Reducible (mod nl nr)
reduceAST id (Unary op _ e _)      = 
    let  re = reduceAST id e
    in if re == NonReducible || re == QuanVariable id then NonReducible
       else let ne = getNum re
            in case op of
                 Minus -> Reducible $ -ne
                 Abs   -> Reducible $ abs ne
-- Siguen faltando operadores
reduceAST id (Int  _ m _)          = Reducible m
reduceAST id (Char _ m _)          = Reducible $ (toInteger . ord) m
reduceAST id (Bool _ m _)          = Reducible $ (toInteger . fromEnum) m
reduceAST id (ID _ id' _ )         = if id' == id then QuanVariable id else NonReducible
reduceAST _  _                     = NonReducible

data Reducibility = NonReducible | Reducible { getNum :: Integer } | QuanVariable T.Text
    deriving (Show, Eq)

occursCheck :: AST a -> T.Text -> RWSS.RWS (SymbolTable) (DS.Seq MyTypeError) () (Bool)
occursCheck (Arithmetic _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Relational _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Boolean    _ _ l r _) id = AP.liftA2 (&&) (occursCheck l id) (occursCheck r id)
occursCheck (ID _ t _            ) id = return $ id == t
occursCheck (ArrCall _ _ xs _    ) id = fmap or $ mapM ((flip occursCheck) id) xs
occursCheck (FCallExp _ _ xs _   ) id = fmap or $ mapM ((flip occursCheck) id) xs
occursCheck (EmptyRange _ _      ) _  = return $ True
occursCheck _ _                       = return $ False

-- verTypeAST ((ID     loc cont t)) = return (ID     loc cont t       )
-- verTypeAST ((Int    loc cont _)) = return (Int    loc cont MyInt   )
-- verTypeAST ((Float  loc cont _)) = return (Float  loc cont MyFloat )
-- verTypeAST ((Bool   loc cont _)) = return (Bool   loc cont MyBool  )
-- verTypeAST ((Char   loc cont _)) = return (Char   loc cont MyChar  )
-- verTypeAST ((String loc cont _)) = return (String loc cont MyString)

getLocArgs args = return $ fmap AST.location args

verTypeASTlist []     = return []
verTypeASTlist (x:xs) = do r  <- verTypeAST x
                           rs <- verTypeASTlist xs
                           return (r:rs)


drawASTtype (ast, err) = case (DS.null err) of
                         { True  ->  show ast
                         ; False -> (show ast) ++ (drawTypeError err) 
                         } 