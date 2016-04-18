{-|
Module      : SymbolTable
Description : Tabla de Simbolos
Copyright   : GraCieLa

Modulo donde se encuentra todo lo referente al manejo de la tabla de simbolos 
del compilador
-}
module SymbolTable where

import qualified Data.Tree       as Tr
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import Data.Monoid
import Location
import Contents


-- | Numero que especifica el nivel de alcance que posee un procedimiento
type Scope = Int 


-- | Coleccion de variables, por cada una de ellas se guarda un 'Contents' con su informacion necesaria
newtype Dictionary = Dictionary { getMap :: M.Map T.Text (Contents SymbolTable) }
        deriving (Eq)


-- | Instancia 'Show' para el tipo 'Dictionary'
instance Show Dictionary where
   show (Dictionary dic)  =  if M.null dic then "No hay ningun elemento" else drawDic 0 (M.toList dic)


-- | Arbol de 'Dictionary', cada puede poseer otro 'SymbolTable' con todos sus alcances anidados
newtype SymbolTable = Table { actual :: Tr.Tree ((Dictionary, Scope), Maybe SymbolTable) }  
        deriving (Eq, Show)


-- | Retorna la coleccion de variables del alcance actual
getActual :: SymbolTable -> Dictionary
getActual tabla = (fst . fst) $ Tr.rootLabel (actual tabla)


-- | Modifica la coleccion de variables del alcance actual
modifyActual :: T.Text -> (Contents SymbolTable -> Contents SymbolTable) -> SymbolTable -> SymbolTable
modifyActual id f tabla = Table $ fmap (modifyRoot id f) (actual tabla)


-- | Modifica la coleccion de variables del alcance anterior
modifyPadre :: SymbolTable -> Maybe SymbolTable -> SymbolTable
modifyPadre tabla padre = Table $ fmap (modifyRootPadre padre) (actual tabla)


-- | Modifica la coleccion de variables del alcance anterior
modifyRootPadre :: Maybe SymbolTable -> ((Dictionary, Scope), Maybe SymbolTable)
                                     -> ((Dictionary, Scope), Maybe SymbolTable)
modifyRootPadre padre ((dic, sc), _) = ((dic, sc), padre)



modifyRoot :: T.Text -> (Contents SymbolTable -> Contents SymbolTable)
              -> ((Dictionary, Scope), Maybe SymbolTable) -> ((Dictionary, Scope), Maybe SymbolTable)
modifyRoot id f ((dic, sc), p) = ((Dictionary (M.adjust f id (getMap dic)), sc), p)


getScope :: SymbolTable -> Scope
getScope tabla = (snd . fst) $ Tr.rootLabel (actual tabla)


getPadre :: SymbolTable -> Maybe SymbolTable
getPadre tabla = snd $ Tr.rootLabel (actual tabla)


getHijos :: SymbolTable -> [Tr.Tree ((Dictionary, Scope), Maybe SymbolTable)]
getHijos tabla = Tr.subForest (actual tabla)


getTabla :: SymbolTable -> ((Dictionary, Scope), Maybe SymbolTable)
getTabla tabla = Tr.rootLabel (actual tabla)


insertTabla :: Dictionary -> Scope -> SymbolTable -> SymbolTable
insertTabla dic sc tabla = Table (Tr.Node ((dic, sc), getPadre tabla) (getHijos tabla))


insertHijo :: SymbolTable -> SymbolTable -> SymbolTable
insertHijo hijo padre = Table (Tr.Node (getTabla padre) ((getHijos padre) ++ [(actual hijo)]))


emptyTable :: SymbolTable
emptyTable =  Table (Tr.Node ((Dictionary M.empty, 0), Nothing) []) 


enterScope :: SymbolTable -> SymbolTable
enterScope tabla = Table (Tr.Node ((Dictionary M.empty, getScope tabla), Just (updateScope tabla)) [])


exitScope :: SymbolTable -> Maybe SymbolTable
exitScope tabla = fmap (insertHijo tabla) (getPadre tabla) 


lookUpRoot :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
lookUpRoot = checkSymbol


checkSymbol :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
checkSymbol valor tabla = 
    let dic = getActual tabla 
    in case M.lookup valor (getMap dic) of
       { Just c  -> Just c
       ; Nothing -> case getPadre tabla of
                    { Nothing   -> Nothing
                    ; Just sup  -> checkSymbol valor sup
                    }
       }


lookUpMap :: T.Text -> (Contents SymbolTable -> Contents SymbolTable) -> SymbolTable -> SymbolTable
lookUpMap valor f tabla = 
    let dic  = modifyActual valor f tabla
        r    = fmap (lookUpMap valor f) (getPadre dic)
    
    in case r of 
       { Nothing -> dic 
       ; Just p  -> modifyPadre dic r
       }


initSymbol :: T.Text -> SymbolTable -> SymbolTable
initSymbol id sb = lookUpMap id initSymbolContent sb


updateScope :: SymbolTable -> SymbolTable
updateScope sb = sb { actual = Tr.Node ((getActual sb, (getScope sb) + 1), fmap updateScope (getPadre sb)) (Tr.subForest (actual sb)) }


addSymbol :: T.Text -> (Contents SymbolTable) -> SymbolTable -> (Either (Contents SymbolTable) SymbolTable)
addSymbol valor content tabla =
    case checkSymbol valor tabla of
    { Just c   -> Left c
    ; Nothing  -> let newActual = M.insert (valor) (content) (getMap (getActual tabla))
                      sc = getScope tabla 
                  in Right $ insertTabla (Dictionary newActual) sc tabla
    }


space :: Char
space = ' '

putSpaces :: Int -> String
putSpaces   level = take level (repeat space)

putSpacesLn :: Int -> String
putSpacesLn level = "\n" `mappend` take level (repeat space)


--drawST level st = show (fst $ Tr.rootLabel st)
drawST level st =  putSpacesLn level `mappend` "Alcance: " `mappend` show ((snd . fst) $ Tr.rootLabel st)  
                                     `mappend` drawDic level (M.toList (getMap ((fst . fst) $ Tr.rootLabel st)))
                                     `mappend` drawSTforest (level + 4) (Tr.subForest st) 

drawSTforest level xs = foldl (\acc st -> (acc `mappend` putSpacesLn level `mappend` drawST level st)) [] xs


drawDic level xs = foldl (\acc (var,cont) -> (acc `mappend` putSpacesLn level 
  `mappend` show var `mappend` show cont )) [] xs

