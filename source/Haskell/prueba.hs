{-# LANGUAGE NamedFieldPuns #-}


import Control.Applicative ((<*>))
import Data.Map (Map)
import Data.Map as Map (lookup, fromList)

data Expression 
  = Suma  { type1 :: Type, type2 :: Type } 
  | Resta { type1 :: Type, type2 :: Type } deriving (Show)

data Expression' 
  = Suma'  
    { type1' :: Map String Type -> Type 
    , type2' :: Map String Type -> Type 
    }
  | Resta'
    { type1' :: Map String Type -> Type 
    , type2' :: Map String Type -> Type 
    }



data Instruction 
  = If { expr :: Expression } 
  | Do { expr :: Expression } 
  deriving (Show)

data Instruction' 
  = If' { expr' :: Map String Type -> Expression } 
  | Do' { expr' :: Map String Type -> Expression } 




data Type = A | B | C | V String   deriving (Show)

data Struct = Struct { insts :: [Instruction] } 
          deriving (Show)

data Struct' 
  = Struct' 
    { insts' :: [ Map String Type -> Instruction ]
    } 



struct' :: Struct' -> Map String Type -> Struct
struct' (Struct' insts' ) t = Struct $ insts' <*> [t]

instruction' :: Instruction' -> Map String Type -> Instruction
instruction' inst' t = case inst' of 
  If' { expr' } -> If (expr' t)
  Do' { expr' } -> Do (expr' t)

expression' :: Expression' -> Map String Type -> Expression
expression' expr' t = case expr' of 
  Suma'  { type1', type2' } -> Suma  (type1' t) (type2' t)
  Resta' { type1', type2' } -> Resta (type1' t) (type2' t)

type' :: Type -> Map String Type -> Type
type' (V name) tMap = case name `Map.lookup` tMap of
    Nothing -> error "no existe el tipo"
    Just t  -> case t of
      V _ -> error "Aun queda polimorfico" 
      _   -> t

type' t _ = t

typeMap1, typeMap2 :: Map String Type
typeMap1 = Map.fromList [("a", A), ("b", B), ("cosaLoca", C)]
typeMap2 = Map.fromList [("a", C), ("b", A), ("cosaLoca", B)]


s = Struct' [ instruction' $
                If' $ expression' $
                  Suma' (type' $ V "a") (type' A )
            , instruction' $
                Do' $ expression' $
                  Resta' (type' $ V "a") (type' $ V "cosaLoca" )
            ]

