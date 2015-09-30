module ASTtype where

import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Applicative      as AP
import qualified Data.Sequence            as DS
import qualified Data.Text                as T
import Data.Range.Range                   as RA   
import MyTypeError           
import SymbolTable
import Data.Maybe
import TypeState
import ReduceAST
import VerTypes
import Location
import Type 
import AST


runTVerifier :: SymbolTable -> AST Type -> (AST Type, DS.Seq MyTypeError)
runTVerifier sTable sTree = RWSS.evalRWS (verTypeAST sTree) sTable [] 


verTypeAST :: (AST Type) -> RWSS.RWS (SymbolTable) (DS.Seq (MyTypeError)) ([String]) (AST Type)
verTypeAST (AST.Program name loc defs accs _) =
    do defs'    <- mapM verTypeAST defs
       accs'    <- mapM verTypeAST accs
       let defsT = map tag defs'
       let accsT = map tag accs'
       checkT   <- verProgram defsT accsT
       return $ AST.Program name loc defs' accs' checkT


verTypeAST (Constant loc True  max _) = return $ Constant loc True  max MyInt  
verTypeAST (Constant loc False max _) = return $ Constant loc False max MyFloat


verTypeAST (Arithmetic t loc lexpr rexp _) =
	do lexpr' <- verTypeAST lexpr
	   rexp'  <- verTypeAST rexp
	   checkT <- verArithmetic (tag lexpr') (tag rexp') loc t
	   return $ Arithmetic t loc lexpr' rexp' checkT


verTypeAST (Relational t loc lexpr rexp _) =
    do lexpr' <- verTypeAST lexpr
       rexp'  <- verTypeAST rexp
       checkT <- verRelational (tag lexpr') (tag rexp') loc t
       return $ Relational t loc lexpr' rexp' checkT
                                                         

verTypeAST (Boolean    t loc lexpr rexp _) =
    do lexpr' <- verTypeAST lexpr
       rexp'  <- verTypeAST rexp
       checkT <- verBoolean (tag lexpr') (tag rexp') loc t
       return $ Boolean t loc lexpr' rexp' checkT
                                                         

verTypeAST (Convertion t loc exp _) = 
    do exp'   <- verTypeAST (exp)
       checkT <- verConvertion t
       return $ Convertion t loc exp' checkT


verTypeAST (Unary op loc exp _) =
    do exp'   <- verTypeAST (exp)
       checkT <- verUnary op (tag exp') loc
       return $ Unary op loc exp' checkT


verTypeAST (Block loc st decs accs _) =
    do decs'  <- mapM verTypeAST decs
       accs'  <- mapM verTypeAST accs
       checkT <- verBlock (map tag accs')
       return $ Block loc st decs accs' checkT


verTypeAST (Skip  loc _) = return $ Skip  loc MyEmpty
verTypeAST (Abort loc _) = return $ Abort loc MyEmpty


verTypeAST (Ran var t loc _) = 
    do checkT <- verRandom var t loc
       return $ Ran var t loc checkT


verTypeAST (Write ln exp loc _) = 
    do exp'   <- verTypeAST (exp)
       checkT <- verWrite (tag exp')
       return $ Write ln exp' loc checkT


verTypeAST (ArrCall loc name args t) = 
    do args'  <- mapM verTypeAST args
       checkT <- verArrayCall name (map tag args') t loc
       return $ ArrCall loc name args' checkT    


verTypeAST (Guard exp action loc _) =
    do exp'    <- verTypeAST exp
       action' <- verTypeAST action
       checkT  <- verGuard (tag exp') (tag action') loc
       return $ Guard exp' action' loc checkT
                                                  

verTypeAST (GuardExp exp action loc _) =
    do exp'    <- verTypeAST exp
       action' <- verTypeAST action
       checkT  <- verGuardExp (tag exp') (tag action') loc
       return $ GuardExp exp' action' loc checkT
                                                

verTypeAST (States t loc expr _) =
    do expr'  <- verTypeAST expr
       checkT <- verState (tag expr') loc t
       return $ States t loc expr' checkT    


verTypeAST (GuardAction loc assert action _) =
    do assert' <- verTypeAST assert
       action' <- verTypeAST action
       checkT  <- verGuardAction (tag assert') (tag action')
       return $ GuardAction loc assert' action' checkT
                                                           

verTypeAST (LAssign idlist explist loc _) =
    do explist' <- mapM verTypeAST explist
       idlist'  <- mapM verTypeAST idlist
       checkT   <- verLAssign (map (fromJust . astToId) idlist')
                     (map tag idlist') (map tag explist') (map AST.location idlist)
       return $ LAssign idlist' explist' loc checkT


verTypeAST (Cond guard loc _) =
    do guard' <- mapM verTypeAST guard
       checkT <- verCond (map tag guard') loc
       return $ Cond guard' loc checkT


verTypeAST (Rept guard inv bound loc _) =
    do guard' <- mapM verTypeAST guard
       inv'   <- verTypeAST inv
       bound' <- verTypeAST bound
       checkT <- verRept (map tag guard') (tag inv') (tag bound')
       return $ Rept guard' inv' bound' loc checkT
                                                      

verTypeAST (ProcCall name sb loc args _) = 
    do args'  <- mapM verTypeAST args  
       locs   <- getLocArgs args
       checkT <- verProcCall name sb args' loc locs
       return (ProcCall name sb loc args' checkT)

  
verTypeAST (FCallExp name sb loc args _) =
    do args'  <- mapM verTypeAST args
       locs   <- getLocArgs args
       checkT <- verCallExp name sb (map tag args') loc locs
       return $ FCallExp name sb loc args' checkT


verTypeAST (DefFun name st loc body ret bound params _) =
    do body'  <- verTypeAST body
       bound' <- verTypeAST bound
       checkT <- verDefFun name (tag body') (tag bound') loc
       return $ DefFun name st loc body' ret bound' params checkT
                                                    

verTypeAST (DefProc name st accs pre post bound cdec args _) = 
    do accs'  <- mapM verTypeAST accs 
       pre'   <- verTypeAST pre  
       post'  <- verTypeAST post 
       bound' <- verTypeAST bound
       cdec'  <- mapM verTypeAST cdec
       checkT <- verDefProc (map tag accs') (tag pre') (tag post') (tag bound') (map tag cdec')
       return $ DefProc name st accs' pre' post' bound' cdec' args checkT
                                                              

verTypeAST (ConsAssign loc xs es t) =
    do es'    <- mapM verTypeAST es
       checkT <- verConsAssign xs loc (map tag es') t
       return $ ConsAssign loc xs es checkT

     
verTypeAST (Quant op var loc range term _) = 
    do range' <- verTypeAST range  
       term'  <- verTypeAST term 
       checkT <- verQuant op (tag range') (tag term') loc
       case checkT of
       { MyError   -> return (Quant op var loc range' term' checkT)
       ; otherwise -> do let id = var
                         r <- occursCheck range id
                         case r of 
                         { True  -> case astToRange var range' of
                                    { Nothing -> return $ Quant op var loc range' term' checkT
                                    ; Just r  -> return $ QuantRan op var loc r term' checkT
                                    }
                         ; False -> do addNotOccursVarError id loc
                                       return $ Quant op var loc range' term' MyError
                         }
        }


verTypeAST ast = return $ ast


astToRange :: T.Text -> AST Type -> Maybe [Range Integer]
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
       { Dis     -> AP.liftA2 RA.union lr rr
       ; Con     -> AP.liftA2 RA.intersection lr rr
       ; Implies -> AP.liftA2 RA.union (fmap RA.invert lr) rr
       ; Conse   -> AP.liftA2 RA.union (fmap RA.invert rr) lr
       }


astToRange id (Unary Not _ e _) = 
    let r = astToRange id e in fmap RA.invert r


astToRange _ _ = Nothing


buildRange :: OpRel -> Reducibility -> Reducibility -> Maybe [Range Integer]
buildRange Less     (QuanVariable id) (Reducible n) = return $ return $ RA.UpperBoundRange $ n - 1 -- i < n
buildRange Less     (Reducible n) (QuanVariable id) = return $ return $ RA.LowerBoundRange $ n + 1 -- n < i
buildRange Greater  (QuanVariable id) (Reducible n) = return $ return $ RA.LowerBoundRange $ n + 1 -- i > n
buildRange Greater  (Reducible n) (QuanVariable id) = return $ return $ RA.UpperBoundRange $ n - 1 -- n > i
buildRange LEqual   (QuanVariable id) (Reducible n) = return $ return $ RA.UpperBoundRange n -- i <= n
buildRange LEqual   (Reducible n) (QuanVariable id) = return $ return $ RA.LowerBoundRange n -- n <= i
buildRange GEqual   (QuanVariable id) (Reducible n) = return $ return $ RA.LowerBoundRange n -- i >= n
buildRange GEqual   (Reducible n) (QuanVariable id) = return $ return $ RA.UpperBoundRange n -- n >= i
buildRange Ine      (Reducible n) (QuanVariable id) = return $ RA.invert $ [RA.SingletonRange n]
buildRange Ine      (QuanVariable id) (Reducible n) = return $ RA.invert $ [RA.SingletonRange n]
buildRange _ _ _                                    = return $ return $ InfiniteRange


getLocArgs :: [AST Type] -> MyVerType [Location]
getLocArgs args = return $ fmap AST.location args


drawASTtype :: (AST Type, DS.Seq MyTypeError) -> String
drawASTtype (ast, err) = case (DS.null err) of
                         { True  ->  show ast
                         ; False -> (show ast) ++ (drawTypeError err) 
                         } 
