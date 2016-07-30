module ReduceAST where

--------------------------------------------------------------------------------
import           AST
import           Type
import           VerTypes
--------------------------------------------------------------------------------
import           Control.Applicative (liftA2)
import           Data.Char
import qualified Data.Text           as T
import           Prelude             hiding (max)
import qualified Prelude             as P (max)
--------------------------------------------------------------------------------

data Reducibility
  = NonReducible
  | Reducible { getNum :: Integer }
  | QuanVariable T.Text
  deriving (Show, Eq)


reduceAST :: T.Text -> AST -> Reducibility
reduceAST id (Arithmetic op _ l r _)
  |  lr == NonReducible
  || rr == NonReducible
  || lr == QuanVariable id
  || rr == QuanVariable id = NonReducible
  | otherwise = Reducible (nl `op'` nr)
    where
      lr = reduceAST id l
      rr = reduceAST id r
      nl = getNum lr
      nr = getNum rr
      op' = case op of
        Sum -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> quot
        Exp -> (^)
        Max -> P.max  -- the prefix shouldn't be necessary
        Min -> min
        Mod -> mod


reduceAST id (Unary op _ e _) =
    let re = reduceAST id e
    in if re == NonReducible || re == QuanVariable id
        then NonReducible
        else let ne = getNum re
            in case op of
                Minus -> Reducible $ -ne
                Abs   -> Reducible $ abs ne


-- Siguen faltando operadores
reduceAST id (Int  _  m  _) = Reducible m
reduceAST id (Char _  m  _) = Reducible $ (toInteger . ord) m
reduceAST id (Bool _  m  _) = Reducible $ (toInteger . fromEnum) m
reduceAST id (Id   _ id' _) = if id' == id then QuanVariable id else NonReducible
reduceAST _  _              = NonReducible


occursCheck :: AST a -> T.Text -> MyVerType Bool
occursCheck (Arithmetic _ _ l r _) id = liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Relational _ _ l r _) id = liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Boolean    _ _ l r _) id = liftA2 (&&) (occursCheck l id) (occursCheck r id)
occursCheck (Id             _ t _) id = return $ id == t
occursCheck (ArrCall     _ _ xs _) id = or <$> mapM (`occursCheck` id) xs
occursCheck (FCallExp  _ _ _ xs _) id = or <$> mapM (`occursCheck` id) xs
occursCheck (EmptyRange       _ _) _  = return True
occursCheck _ _                       = return False
