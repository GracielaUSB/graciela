-- Tabla 

import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Map  as M


type Linea   =  Integer 
type Columna =  Integer 
type Offset  =  Integer

data Struct = Struct Linea Columna Offset deriving (Read, Eq, Show, Ord)

type Diccionario = M.Map T.Text Struct        

data Nodo = Dic {actual :: Diccionario, padre :: Diccionario} deriving (Read, Eq, Show, Ord)

type TablaSimbolos = Tree.Tree Nodo 




--enterScope :: TablaSimbolos -> TablaSimbolos 
--enterScope x = Tree.unfoldTree arbolito 0


-- actual (Tree.rootLabel x)

arbolito n =  if (n < 0) then (n,[]) else (n,[n-1, n-2])

--Ejemploooo

ana :: (b -> Maybe (a,b)) -> b -> [a]   
-- Its definition:
ana f x = case (f x) of
            Nothing -> []
            Just (a,y) ->  a:(ana f y)

until5 n = if (n < 5) then (Just (n+1,n+1)) else Nothing


Node {rootLabel = 2, 
			subForest = [Node {rootLabel = 1, 
								subForest = [Node {rootLabel = 0, 
													subForest = [Node {rootLabel = -1, 
																		subForest = []},
																 Node {rootLabel = -2, 
																 		subForest = []}]}
											,Node {rootLabel = -1, 
													subForest = []}]}
						,Node {rootLabel = 0, 
								subForest = [Node {rootLabel = -1, 
													subForest = []},
											Node {rootLabel = -2, 
													subForest = []}]}]}
