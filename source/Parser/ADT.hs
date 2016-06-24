module Parser.ADT
    ( abstractDataType
    , dataType 
    ) where

-------------------------------------------------------------------------------
import Parser.Assertions             
import Parser.Expression
import Parser.Declarations
import Parser.Procedures             (listDefProc, listArgProc)
import Parser.Instructions           
import Parser.TokenParser
import MyParseError                  as PE
import ParserState
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


-- AbstractDataType -> 'abstract' ID AbstractTypes 'begin' AbstractBody 'end'
abstractDataType :: MyParser (Maybe (AST Type))
abstractDataType = do 
    verify TokAbstract
    try (do id <- parseID
            abstractTypes
            try (do parseBegin
                    abstractBody parseEnd parseEnd
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

-- AbstractBody -> DecList Invariant ListProcDecl
abstractBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
abstractBody follow recSet = do 
    newScopeParser
    dl    <- decList followAction (recSet)
    invariant follow recSet
    procs <- listProcDecl follow recSet
    exitScopeParser
    return Nothing

    
    
-- ProcDecl -> 'proc' ID ':' '(' ListArgProc ')' Precondition Postcondition
procDecl :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
procDecl follow recSet = do
    pos <- getPosition
    try (
     do parseProc
        try (
         do id <- parseID
            try (
             do parseColon
                try (
                 do parseLeftParent
                    newScopeParser
                    targs <- listArgProc id parseRightParent parseRightParent
                    try (
                     do parseRightParent
                        try (
                           do pre  <- precondition parseTokOpenBlock (recSet <|> parseTokOpenBlock)
                              post <- postcondition parseEnd (recSet <|> parseEnd)
                              exitScopeParser
                              return Nothing
                            )
                            <|> do  genNewError follow PE.Begin
                                    return Nothing
                        )
                        <|> do genNewError follow PE.TokenRP
                               return Nothing
                    )
                    <|> do genNewError follow PE.TokenLP
                           return Nothing
                )
                <|> do genNewError follow PE.Colon
                       return Nothing
            )
            <|> do genNewError follow PE.IDError
                   return Nothing
        )
        <|> do genNewError follow PE.ProcOrFunc
               return Nothing

-- ListProcDecl ->  ProcDecl ListProcDecl
-- ListProcDecl ->  ProcDecl
listProcDecl :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
listProcDecl follow recSet =
    do lookAhead parseEOF
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do pf <- procDecl  follow recSet
              rl <- listProcDecl follow recSet
              return (AP.liftA2 (:) pf rl)
       <|> do return $ Nothing



-- dataType -> 'type' ID 'implements' ID Types 'begin' DataTypeBody 'end'
dataType :: MyParser (Maybe (AST Type))
dataType = do 
    verify TokDataType
    try (
     do id <- parseID
        try (
         do verify TokImplements
            idAbstract <- parseID
            types
            try (
             do parseBegin
                dataTypeBody parseEnd parseEnd
                try (
                 do parseEnd 
                    return Nothing
                    )
                    <|> do  genNewError parseEOF PE.LexEnd
                            return Nothing
                )
                <|> do  genNewError parseEOF PE.Begin
                        return Nothing
            )
            <|> do  genNewError parseEOF PE.Implements                                                     
                    return Nothing
        )
        <|> do  genNewError parseEOF PE.IDError 
                return Nothing

-- Types -> '(' ListTypes ')'
-- ListTypes: lista de tipos contruidas con parsec
types :: MyParser (Maybe (AST Type))
types = do 
    try (do parseLeftParent
            try (do sepBy (myType parseSemicolon parseSemicolon) (parseComma)
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

-- DataTypeBody -> DeclList RepInvariant AcInvariant ListDefProc
dataTypeBody :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type))
dataTypeBody follow recSet = do 
    newScopeParser
    dl    <- decList followAction recSet
    repInvariant -- No hace nada
    acInvariant  -- No hace nada
    procs <- listDefProc follow recSet
    exitScopeParser
    return Nothing




