module SymbolTable where

import qualified Data.Maybe          as Maybe
import qualified Data.Tree           as Tr
import qualified Data.Text           as T
import qualified Data.Map            as M
import Location
import Type


data Contents = Contents { symbolLoc :: Location, symbolType :: Type } 
        deriving (Read, Eq, Show)


type Diccionario = M.Map T.Text Contents


data SymbolTable = Table { actual :: Tr.Tree (Diccionario, Maybe SymbolTable) }  
        deriving (Eq, Show)


getActual :: SymbolTable -> Diccionario
getActual tabla = fst $ Tr.rootLabel (actual tabla)


getPadre :: SymbolTable -> Maybe SymbolTable
getPadre tabla = snd $ Tr.rootLabel (actual tabla)


getHijos :: SymbolTable -> [Tr.Tree (Diccionario, Maybe SymbolTable)]
getHijos tabla = Tr.subForest (actual tabla)


getTabla :: SymbolTable -> (Diccionario, Maybe SymbolTable)
getTabla tabla = Tr.rootLabel (actual tabla)


insertTabla :: Diccionario -> SymbolTable -> SymbolTable
insertTabla dic tabla = Table (Tr.Node (dic, getPadre tabla) (getHijos tabla))


insertHijo :: SymbolTable -> SymbolTable -> SymbolTable
insertHijo hijo padre = Table (Tr.Node (getTabla padre) ((actual hijo):(getHijos padre)))


emptyTable :: SymbolTable
emptyTable =  Table (Tr.Node (M.empty, Nothing) []) 


enterScope :: SymbolTable -> SymbolTable
enterScope tabla = Table (Tr.Node (M.empty, Just tabla) [])


exitScope :: SymbolTable -> Maybe SymbolTable
exitScope tabla = fmap (insertHijo tabla) (getPadre tabla) 


checkSymbol :: T.Text -> SymbolTable -> Maybe Contents
checkSymbol valor tabla = let dic = getActual tabla in
                            case M.lookup valor dic of
                              { Just c  -> Just c
                              ; Nothing -> case getPadre tabla of
                                             { Nothing   -> Nothing
                                             ; Just sup  -> checkSymbol valor sup
                                             }
                              }


addSymbol :: T.Text -> Contents -> SymbolTable -> (Either Contents SymbolTable)
addSymbol valor content tabla =
          case checkSymbol valor tabla of
          { Just c   -> Left c
          ; Nothing  -> let newActual = M.insert (valor) (content) (getActual tabla) in
                            Right $ insertTabla newActual tabla
          }


look :: (Either String SymbolTable) -> SymbolTable 
look (Right tabla) = tabla


--Casos 

-- t0 = emptyTable
-- 
-- t1 = addSymbol (T.pack "foo") (Contents (Location 1 2 "archivo1") MyInt) t0
-- 
-- t2 = addSymbol (T.pack "bar") (Contents (Location 3 4 "archivo2") MyInt) (look t1)
-- 
-- t3 = enterScope (look t2)
-- 
-- t4 = addSymbol (T.pack "foo fighters") (Contents (Location 7 8 "archivo2") MyFloat) (t3)
-- 
-- t5 = enterScope (look t4)
-- 
-- t6 = addSymbol (T.pack "metallica") (Contents (Location 7 8 "archivo1") MyChar) (t5)
-- 
-- t7 = exitScope (look t6)
-- 
-- t8 = exitScope (Maybe.fromJust t7)
-- 
-- t9 = exitScope (Maybe.fromJust t8)
