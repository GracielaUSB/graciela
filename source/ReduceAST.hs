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
reduceAST id (AST _ _ _ (Arithmetic op l r))
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


reduceAST id (AST _ _ _ ast') = case ast' of 
  Unary op e ->
    let re = reduceAST id e
    in if re == NonReducible || re == QuanVariable id
        then NonReducible
        else let ne = getNum re
            in case op of
                Minus -> Reducible $ -ne
                Abs   -> Reducible $ abs ne

-- Siguen faltando operadores
  Int  m -> Reducible m
  Char m -> Reducible $ (toInteger . ord) m
  Bool m -> Reducible $ (toInteger . fromEnum) m
  Id id' -> if id' == id 
      then QuanVariable id 
      else NonReducible
  _      -> NonReducible


occursCheck :: AST -> T.Text -> MyVerType Bool
occursCheck (AST _ _ _ ast') id = case ast' of
  Arithmetic _ l r -> liftA2 (||) (occursCheck l id) (occursCheck r id)
  ArrCall     _ xs -> or <$> mapM (`occursCheck` id) xs
  Boolean    _ l r -> liftA2 (&&) (occursCheck l id) (occursCheck r id)
  EmptyRange       -> return True
  FCallExp  _ _ xs -> or <$> mapM (`occursCheck` id) xs
  Id             t -> return $ id == t
  Relational _ l r -> liftA2 (||) (occursCheck l id) (occursCheck r id)
  _                -> return False
