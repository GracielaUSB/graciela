module Parser.TAD 
    ( abstract
    ) where

-------------------------------------------------------------------------------
import Parser.Assertions
import Parser.Expression
import Parser.Declarations
import Parser.Procedures
import MyParseError                  as PE
import ParserState
import Parser.TokenParser
import ParserType
import Contents
import Location
import Token
import State
import Type
import AST
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP
import qualified Control.Monad       as M
import qualified Data.Text           as T
import           Text.Parsec
-------------------------------------------------------------------------------


-- Abstract -> 'abstract' ID AbstractTypes 'begin' AbstractBody 'end'
abstract :: MyParser (Maybe (AST Type))
abstract = do 
    verify TokAbstract
    try (do id <- parseID
            abstractTypes
            try (do parseBegin
                    try (do parseEnd 
                            return Nothing
                        )
                        <|> do  genNewError parseEOF PE.LexEnd
                                return Nothing
                )
                <|> do  genNewError parseEOF PE.Begin
                        return Nothing
        )
        <|> do  genNewError parseEOF PE.IDError 
                return Nothing

-- AbstractType -> '(' ListTypes ')'
-- ListTypes: lista de ids contruidas con parsec
--
-- Podria hacerce con between, pero no se como dar errores "bonitos" 
abstractTypes :: MyParser (Maybe (AST Type))
abstractTypes = do 
    try (do parseLeftParent
            try (do sepBy (parseID) (parseComma)
                    try (do parseRightParent
                            return Nothing    
                        )
                        <|> do  genNewError parseEOF PE.TokenLP 
                                return Nothing
                )
                <|> do  genNewError parseEOF PE.TokenLP  -- MEJORAR ERROR 
                        return Nothing 
        )
        <|> do  genNewError parseEOF PE.TokenRP
                return Nothing


abstractBody ::  MyParser (Maybe (AST Type))
abstractBody = undefined
