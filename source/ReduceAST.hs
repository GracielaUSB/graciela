module ReduceAST where

import qualified Control.Applicative as AP
import qualified Data.Text           as T
import Prelude                       as P
import Data.Char
import TypeState
import Type 
import AST


data Reducibility = NonReducible | Reducible { getNum :: Integer } | QuanVariable T.Text
    deriving (Show, Eq)


reduceAST :: T.Text -> AST Type -> Reducibility
reduceAST id (Arithmetic op _ l r _)  = 
    let lr = reduceAST id l
        rr = reduceAST id r
    in if lr == NonReducible || rr == NonReducible    
         then NonReducible
         else if lr == QuanVariable id || rr == QuanVariable id 
                then NonReducible
                else let nl = getNum lr
                         nr = getNum rr

                     in case op of
                        { Sum -> Reducible (nl + nr)
                        ; Sub -> Reducible (nl - nr)
                        ; Mul -> Reducible (nl * nr)
                        ; Div -> Reducible (quot nl nr)
                        ; Exp -> Reducible (nl ^ nr)
                        ; Max -> Reducible (P.max nl nr)
                        ; Min -> Reducible (min nl nr)
                        ; Mod -> Reducible (mod nl nr)
                        }


reduceAST id (Unary op _ e _) = 
    let re = reduceAST id e
    in if re == NonReducible || re == QuanVariable id 
         then NonReducible
         else let ne = getNum re
              in case op of
                 { Minus -> Reducible $ -ne
                 ; Abs   -> Reducible $ abs ne
                 }

-- Siguen faltando operadores
reduceAST id (Int  _ m _)  = Reducible m
reduceAST id (Char _ m _)  = Reducible $ (toInteger . ord) m
reduceAST id (Bool _ m _)  = Reducible $ (toInteger . fromEnum) m
reduceAST id (ID _ id' _ ) = if id' == id then QuanVariable id else NonReducible
reduceAST _  _             = NonReducible


occursCheck :: AST a -> T.Text -> MyVerType Bool
occursCheck (Arithmetic _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Relational _ _ l r _) id = AP.liftA2 (||) (occursCheck l id) (occursCheck r id)
occursCheck (Boolean    _ _ l r _) id = AP.liftA2 (&&) (occursCheck l id) (occursCheck r id)
occursCheck (ID _ t _            ) id = return $ id == t
occursCheck (ArrCall _ _ xs _    ) id = fmap or $ mapM ((flip occursCheck) id) xs
occursCheck (FCallExp _ _ _ xs _ ) id = fmap or $ mapM ((flip occursCheck) id) xs
occursCheck (EmptyRange _ _      ) _  = return $ True
occursCheck _ _                       = return $ False
