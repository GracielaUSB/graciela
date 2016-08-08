import Parser.Program
import Treelike
import Lexer
import SymbolTable
import Graciela
import Entry 
import Location
import Text.Megaparsec

main :: IO ()
main = do
  name <- getLine  
  let input = pack strinput
  let Right ets = runParser lexer "" input
  Just result <- parseMaybe program ets
  putStr $ drawTree $ result 