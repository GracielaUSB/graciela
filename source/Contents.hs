module Contents
    ( Contents
    , getContentLoc
    , getContentType
    , isContentProc
    , isConstant
    , getProcArgType
    , getListNames
    , Value (..)
    , newFunction
    , newProc
    , newVariable
    , newConstant
    , newArgProc
    , isInArg
    , getContentValue
    ) where

import Location
import Type
import Data.Text               as T

data Value = 
    I Integer
  | C Char
  | D Double
  | S String
  | B Bool
    deriving (Show, Eq)

data BasicInfo = 
     BasicInfo Location Type
    deriving(Show, Eq)

data Contents s = 
    Constant    BasicInfo Value
  | Variable    BasicInfo
  | ArgProcCont BasicInfo TypeArg
  | ProcCon     BasicInfo [T.Text] s
  | FuncCon     BasicInfo
    deriving (Eq)
 
instance Show a => Show (Contents a) where
   show (Constant (BasicInfo loc t) v)        = "Es una constante, Tipo: " ++ show t  ++ ",declarada en: " ++ show loc ++ " de valor " ++ show v
   show (Variable (BasicInfo loc t))          = "Es una variable, Tipo: " ++ show t ++ ", declarada en: " ++ show loc
   show (ArgProcCont (BasicInfo loc t) argT)  = show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (ProcCon _ ln sb)                     = show ln ++ show sb
   show (FuncCon bi)                          = show bi

getContentLoc :: Contents a -> Location
getContentLoc (Constant    (BasicInfo loc _) _  ) = loc
getContentLoc (Variable    (BasicInfo loc _)    ) = loc
getContentLoc (ArgProcCont (BasicInfo loc _) _  ) = loc
getContentLoc (ProcCon     (BasicInfo loc _) _ _) = loc

getContentType :: Contents a -> Type
getContentType (Constant    (BasicInfo _ t) _  ) = t
getContentType (Variable    (BasicInfo _ t)    ) = t
getContentType (ArgProcCont (BasicInfo _ t) _  ) = t
getContentType (ProcCon     (BasicInfo _ t) _ _) = t
getContentType (FuncCon     (BasicInfo _ t)    ) = t

isContentProc :: Contents a -> Bool
isContentProc (ProcCon _ _ _) = True
isContentProc _               = False

isConstant :: Contents a -> Bool
isConstant (Constant _ _) = True
isConstant _              = False

newFunction :: Location -> [Type] -> Type -> Contents a
newFunction loc lt t = FuncCon $ BasicInfo loc $ MyFunction lt t

newProc :: Location -> [Type] -> [T.Text] -> s -> Contents s
newProc loc tys tes sb = ProcCon (BasicInfo loc $ MyProcedure tys) tes sb

getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont _ t) = Just t
getProcArgType _                 = Nothing

getListNames :: Contents a -> [T.Text]
getListNames (ProcCon _ xs _ ) = xs 

newVariable :: Location -> Type -> Contents a
newVariable loc t = Variable $ BasicInfo loc t

newConstant :: Location -> Type -> Value -> Contents a
newConstant loc t v = Constant (BasicInfo loc t) v

newArgProc :: TypeArg -> Location -> Type -> Contents a
newArgProc targ loc t = ArgProcCont (BasicInfo loc t) targ

isInArg :: Contents a -> Bool
isInArg (ArgProcCont _ In) = True
isInArg _                  = False

getContentValue :: Contents a -> Maybe Value
getContentValue (Constant _ v) = Just v
getContentValue _              = Nothing
