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
import Data.Monoid ((<>))
import Contents
import Prelude hiding (id)

-- | Numero que especifica el nivel de alcance que posee un procedimiento
type Scope = Int


-- | Coleccion de variables, por cada una de ellas se guarda un 'Contents' con su informacion necesaria
newtype Dictionary = Dictionary { getMap :: M.Map T.Text (Contents SymbolTable) }
        deriving (Eq)


-- | Instancia 'Show' para el tipo 'Dictionary'
instance Show Dictionary where
   show (Dictionary dic)  =  if M.null dic then "No hay ningun elemento" else drawDic 0 (M.toList dic)


-- | Arbol de 'Dictionary', cada puede poseer otro 'SymbolTable' con todos sus alcances anidados
newtype SymbolTable = Table { current :: Tr.Tree ((Dictionary, Scope), Maybe SymbolTable) }
        deriving (Eq, Show)


-- | Retorna la coleccion de variables del alcance actual
getCurrent :: SymbolTable -> Dictionary
getCurrent tabla = (fst . fst) $ Tr.rootLabel (current tabla)


-- | Modifica la coleccion de variables del alcance actual
modifyCurrent :: T.Text -> (Contents SymbolTable -> Contents SymbolTable) -> SymbolTable -> SymbolTable
modifyCurrent id f tabla = Table $ fmap (modifyRoot id f) (current tabla)


-- | Modifica la coleccion de variables del alcance anterior
modifyPadre :: SymbolTable -> Maybe SymbolTable -> SymbolTable
modifyPadre tabla padre = Table $ fmap (modifyRootPadre padre) (current tabla)


-- | Modifica la coleccion de variables del alcance anterior
modifyRootPadre :: Maybe SymbolTable -> ((Dictionary, Scope), Maybe SymbolTable)
                                     -> ((Dictionary, Scope), Maybe SymbolTable)
modifyRootPadre padre ((dic, sc), _) = ((dic, sc), padre)



modifyRoot :: T.Text -> (Contents SymbolTable -> Contents SymbolTable)
              -> ((Dictionary, Scope), Maybe SymbolTable) -> ((Dictionary, Scope), Maybe SymbolTable)
modifyRoot id f ((dic, sc), p) = ((Dictionary (M.adjust f id (getMap dic)), sc), p)


getScope :: SymbolTable -> Scope
getScope tabla = (snd . fst) $ Tr.rootLabel (current tabla)


getPadre :: SymbolTable -> Maybe SymbolTable
getPadre tabla = snd $ Tr.rootLabel (current tabla)


getHijos :: SymbolTable -> [Tr.Tree ((Dictionary, Scope), Maybe SymbolTable)]
getHijos tabla = Tr.subForest (current tabla)


getTabla :: SymbolTable -> ((Dictionary, Scope), Maybe SymbolTable)
getTabla tabla = Tr.rootLabel (current tabla)


insertTabla :: Dictionary -> Scope -> SymbolTable -> SymbolTable
insertTabla dic sc tabla = Table (Tr.Node ((dic, sc), getPadre tabla) (getHijos tabla))


insertHijo :: SymbolTable -> SymbolTable -> SymbolTable
insertHijo hijo padre = Table (Tr.Node (getTabla padre) (getHijos padre ++ [current hijo]))


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
    let dic = getCurrent tabla
    in case M.lookup valor (getMap dic) of
        Just c  -> Just c
        Nothing -> case getPadre tabla of
            Nothing   -> Nothing
            Just sup  -> checkSymbol valor sup


lookUpMap :: T.Text -> (Contents SymbolTable -> Contents SymbolTable) -> SymbolTable -> SymbolTable
lookUpMap valor f tabla =
    let dic  = modifyCurrent valor f tabla
        r    = fmap (lookUpMap valor f) (getPadre dic)

    in case r of
        Nothing -> dic
        Just _  -> modifyPadre dic r


initSymbol :: T.Text -> SymbolTable -> SymbolTable
initSymbol id = lookUpMap id initSymbolContent


updateScope :: SymbolTable -> SymbolTable
updateScope sb = sb { current = Tr.Node ((getCurrent sb, getScope sb + 1), fmap updateScope (getPadre sb)) (Tr.subForest (current sb)) }


addSymbol :: T.Text -> Contents SymbolTable -> SymbolTable -> Either (Contents SymbolTable) SymbolTable
addSymbol valor content tabla =
    case checkSymbol valor tabla of
      Just c   -> Left c
      Nothing  -> let newCurrent = M.insert valor content (getMap (getCurrent tabla))
                      sc = getScope tabla
                  in Right $ insertTabla (Dictionary newCurrent) sc tabla


space :: Char
space = ' '

putSpaces :: Int -> String
putSpaces level = replicate level space

putSpacesLn :: Int -> String
putSpacesLn level = "\n" <> replicate level space


drawST :: Show b
       => Int -> Tr.Tree ((Dictionary, b), b1) -> String
-- drawST level st = show (fst $ Tr.rootLabel st)
drawST level st =  putSpacesLn level <> "Alcance: " <> show ((snd . fst) $ Tr.rootLabel st)
                                     <> drawDic level (M.toList (getMap (fst . fst $ Tr.rootLabel st)))
                                     <> drawSTforest (level + 4) (Tr.subForest st)

drawSTforest :: Show b
             => Int -> Tr.Forest ((Dictionary, b), b1) -> String
drawSTforest level =
    foldl (\acc st -> acc <> putSpacesLn level <> drawST level st) []


drawDic :: (Show a, Show a1, Foldable t)
        => Int -> t (a, a1) -> String
drawDic level = foldl (\acc (var,cont) -> (acc <> putSpacesLn level
  <> show var <> show cont )) []

