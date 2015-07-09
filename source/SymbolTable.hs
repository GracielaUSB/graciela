module SymbolTable where

import qualified Data.Maybe          as Maybe
import qualified Data.Tree           as Tr
import qualified Data.Text           as T
import qualified Data.Map            as M
import Data.Monoid
import Location
import Contents
import Print


type Scope = Int 

newtype Diccionario = Diccionario { getMap :: M.Map T.Text (Contents SymbolTable) }
        deriving (Eq)


instance Show Diccionario where
   show (Diccionario dic)  =  if M.null dic then "No hay ningun elemento" else drawDic 0 (M.toList dic)


data SymbolTable = Table { actual :: Tr.Tree ((Diccionario, Scope), Maybe SymbolTable) }  
        deriving (Eq)


instance Show SymbolTable where
   show (Table st)  = (drawST 0 st) ++ "\n" 



getActual :: SymbolTable -> Diccionario
getActual tabla = (fst . fst) $ Tr.rootLabel (actual tabla)


getScope :: SymbolTable -> Scope
getScope tabla = (snd . fst) $ Tr.rootLabel (actual tabla)


getPadre :: SymbolTable -> Maybe SymbolTable
getPadre tabla = snd $ Tr.rootLabel (actual tabla)


getHijos :: SymbolTable -> [Tr.Tree ((Diccionario, Scope), Maybe SymbolTable)]
getHijos tabla = Tr.subForest (actual tabla)


getTabla :: SymbolTable -> ((Diccionario, Scope), Maybe SymbolTable)
getTabla tabla = Tr.rootLabel (actual tabla)


insertTabla :: Diccionario -> Scope -> SymbolTable -> SymbolTable
insertTabla dic sc tabla = Table (Tr.Node ((dic, sc), getPadre tabla) (getHijos tabla))


insertHijo :: SymbolTable -> SymbolTable -> SymbolTable
insertHijo hijo padre = Table (Tr.Node (getTabla padre) ((getHijos padre) ++ [(actual hijo)]))


emptyTable :: SymbolTable
emptyTable =  Table (Tr.Node ((Diccionario M.empty, 0), Nothing) []) 


enterScope :: SymbolTable -> SymbolTable
enterScope tabla = Table (Tr.Node ((Diccionario M.empty, getScope tabla), Just (updateScope tabla)) [])


exitScope :: SymbolTable -> Maybe SymbolTable
exitScope tabla = fmap (insertHijo tabla) (getPadre tabla) 


lookUpRoot :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
lookUpRoot = checkSymbol


checkSymbol :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
checkSymbol valor tabla = 
    let dic = getActual tabla in
        case M.lookup valor (getMap dic) of
          { Just c  -> Just c
          ; Nothing -> case getPadre tabla of
                         { Nothing  -> Nothing
                         ; Just sup -> checkSymbol valor sup
                         }
          }


updateScope :: SymbolTable -> SymbolTable
updateScope sb = sb { actual = Tr.Node ((getActual sb, (getScope sb) + 1), 
                     fmap updateScope (getPadre sb)) (Tr.subForest (actual sb)) }


addSymbol :: T.Text -> (Contents SymbolTable) -> SymbolTable -> (Either (Contents SymbolTable) SymbolTable)
addSymbol valor content tabla =
    case checkSymbol valor tabla of
    { Just c   -> Left c
    ; Nothing  -> let newActual = M.insert (valor) (content) (getMap (getActual tabla))
                      sc = getScope tabla 
                  in Right $ insertTabla (Diccionario newActual) sc tabla
    }


look :: (Either String SymbolTable) -> SymbolTable 
look (Right tabla) = tabla


--drawST level st = show (fst $ Tr.rootLabel st)
drawST level st =  putSpacesLn level `mappend` "Alcance: " `mappend` show ((snd . fst) $ Tr.rootLabel st)  
                                     `mappend` drawDic level (M.toList (getMap ((fst . fst) $ Tr.rootLabel st)))
                                     `mappend` drawSTforest (level + 4) (Tr.subForest st) 

drawSTforest level xs = foldl (\acc st -> (acc `mappend` putSpacesLn level `mappend` drawST level st)) [] xs


drawDic level xs = foldl (\acc (var,cont) -> (acc `mappend` putSpacesLn level 
  `mappend` show var `mappend` show cont )) [] xs



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
