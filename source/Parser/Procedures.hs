module Parser.Procedures 
    ( listDefProc
    , procOrFunc
    , followTypeFunction
    , function
    , listArgFunc
    , argFunc
    , listArgFuncAux
    , proc
    , listArgProc
    , listArgProcAux
    , argType
    , arg
    ) where

-------------------------------------------------------------------------------
import Parser.Assertions
import Parser.Expression
import Parser.Declarations
import Parser.Instructions
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


listDefProc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
listDefProc follow recSet =
    do lookAhead parseEOF
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do pf <- procOrFunc  follow recSet
              rl <- listDefProc follow recSet
              return (AP.liftA2 (:) pf rl)
       <|> do return $ Nothing


procOrFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
procOrFunc follow recSet =
    do lookAhead parseFunc
       function (follow <|> parseFunc <|> parseProc) recSet
    <|> do lookAhead parseProc
           proc (follow <|> parseFunc <|> parseProc) recSet
    <|> do genNewError (follow <|> parseFunc <|> parseProc) PE.ProcOrFunc
           do lookAhead follow
              return Nothing
           <|> do lookAhead (parseProc <|> parseFunc)
                  procOrFunc follow recSet
                  return Nothing


followTypeFunction :: MyParser Token
followTypeFunction = parseTokOpenBlock <|> parseTokLeftBound


function :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
function follow recSet =do
    pos <- getPosition                                   
    try $do M.void parseFunc
     <|> do t <- lookAhead parseID
            genNewError (return $TokId t) PE.ProcOrFunc
     <|> do (t,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseID)
            genNewError (return t) PE.ProcOrFunc

    id <- try $do parseID
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return t) PE.IDError
            return $ T.pack "No ID"      
     <|> do (t:_) <- manyTill anyToken (lookAhead parseColon)
            genNewError (return $fst t) PE.IDError                           
            return $ T.pack "No ID"           

    try $do M.void $ parseColon
     <|> do t <- lookAhead parseLeftParent                                  -- proc id   (in a :int) begin
            genNewError (return t) PE.Colon                                 --         ^
     <|> do (t:_) <- manyTill anyToken (lookAhead parseLeftParent)          -- proc id [ (in a : int) begin
            genNewError (return $fst t) PE.Colon                            --         ^

    try $do M.void parseLeftParent
     <|> do id <- lookAhead parseID                                         
            genNewError (return $ TokId id) PE.TokenLP                      -- proc id : in a :int) begin
     <|> do (t,_) <- anyToken                                               --          ^  
            manyTill  anyToken (lookAhead parseID)                          -- proc id : [[$ in a :int) begin  
            genNewError (return t) PE.TokenLP                               --           ^^^
    
    newScopeParser
    lt <- listArgFunc id parseRightParent (recSet <|> parseRightParent)

    try $do M.void parseRightParent
     <|> do t <- lookAhead $ parseArrow 
            genNewError (return t) PE.TokenRP 
     <|> do (t:_) <- manyTill  anyToken (lookAhead parseArrow)
            genNewError (return $fst t) PE.TokenRP 

    try $do M.void parseArrow
     <|> do t <- lookAhead $ parseType' 
            genNewError (return $ TokType t) PE.Arrow
     <|> do (t:_) <- manyTill  anyToken (lookAhead $ parseType')
            genNewError (return $fst t) PE.Arrow 

    t <- try $do parseType'
     <|> do t <- lookAhead parseBegin
            genNewError (return t) PE.TokenType
            return GUndef
     <|> do (t:_) <- manyTill  anyToken (lookAhead parseBegin)
            genNewError (return $fst t) PE.TokenType
            return GUndef
    
    try $do M.void $ parseBegin
     <|> do (t,_) <- lookAhead anyToken                                     -- func id : (in a :int) -> int   
            genNewError (return t) PE.Begin                                 --                               ^
     <|> do (t:_) <- manyTill anyToken (lookAhead conditionalOrExpr)        -- func id : (in a :int) -> int [][]
            genNewError (return $fst t) PE.Begin                            --                              ^^^^

    sb <- getCurrentScope
    addFunTypeParser id lt t (toLocation pos) sb
    b <- conditionalOrExpr
    exitScopeParser
    addFunTypeParser id lt t (toLocation pos) sb
    
    try $do parseEnd
            return(M.liftM5 (DefFun id sb (toLocation pos)) b (return t) (Just (EmptyAST GEmpty)) lt (return (GEmpty)))
     <|> do genNewError follow PE.LexEnd
            return Nothing
    where 
        conditionalOrExpr = (conditional CExpression parseEnd parseEnd) <|> (expr parseEnd parseEnd)
        parseType' :: MyParser Type   
        parseType' = myType (followTypeFunction) (recSet <|> followTypeFunction)

   -- do pos <- getPosition
   --    try ( do parseFunc
   --             try ( do id <- parseID
   --                      try ( do parseColon
   --                               try ( do parseLeftParent
   --                                        newScopeParser
   --                                        lt <- listArgFunc id parseRightParent (recSet <|> parseRightParent)
   --                                        try ( do parseRightParent
   --                                                 try ( do parseArrow
   --                                                          t  <- myType (followTypeFunction) (recSet <|> followTypeFunction)
   --                                                          sb <- getCurrentScope
   --                                                          addFunTypeParser id lt t (toLocation pos) sb
   --                                                          b  <- functionBody follow follow
   --                                                          exitScopeParser
   --                                                          addFunTypeParser id lt t (toLocation pos) sb
   --                                                          return(M.liftM5 (DefFun id sb (toLocation pos)) b (return t) (Just (EmptyAST GEmpty)) lt (return (GEmpty)))
   --                                                     )
   --                                                     <|> do genNewError follow PE.Arrow
   --                                                            return Nothing
   --                                            )
   --                                            <|> do genNewError follow PE.TokenRP
   --                                                   return Nothing
   --                                   )
   --                                   <|> do genNewError follow PE.TokenLP
   --                                          return Nothing
   --                          )
   --                          <|> do genNewError follow PE.Colon
   --                                 return Nothing
   --                 )
   --                 <|> do genNewError follow PE.IDError
   --                        return Nothing
   --        )
   --        <|> do genNewError follow PE.TokenFunc
   --               return Nothing


listArgFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFunc idf follow recSet =
    do lookAhead parseEOF
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do ar <- argFunc idf (follow <|> parseComma) (recSet <|> parseComma)
              rl <- listArgFuncAux idf follow recSet
              return(AP.liftA2 (:) ar rl)


argFunc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
argFunc idf follow recSet =
    try ( do id <- parseID
             try ( do parseColon
                      t  <- myType follow follow
                      pos <- getPosition
                      addFunctionArgParser idf id t (toLocation pos)
                      return $ return (id, t)
                 )
                 <|> do genNewError follow PE.Colon
                        return Nothing
        )
        <|> do genNewError follow PE.IDError
               return Nothing


listArgFuncAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgFuncAux idf follow recSet =
    do lookAhead parseEOF
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> try ( do parseComma
                    ar <- argFunc idf (follow <|> parseComma) (recSet <|> parseComma)
                    rl <- listArgFuncAux idf follow recSet
                    return(AP.liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing

proc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
proc follow recSet = do
    pos <- getPosition                                   
    try $do M.void parseProc
     <|> do t <- lookAhead parseID
            genNewError (return $TokId t) PE.ProcOrFunc
     <|> do (t,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseID)
            genNewError (return t) PE.ProcOrFunc

    id <- try $do parseID
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return t) PE.IDError
            return $ T.pack "No ID"      
     <|> do (t:_) <- manyTill anyToken (lookAhead parseColon)
            genNewError (return $fst t) PE.IDError                           
            return $ T.pack "No ID"              

    try $do M.void $ parseColon
     <|> do t <- lookAhead parseLeftParent                                  -- proc id   (in a :int) begin
            genNewError (return t) PE.Colon                                 --         ^
     <|> do (t,_) <- anyToken                                               -- proc id [ (in a : int) begin
            genNewError (return t) PE.Colon                                 --         ^

    try $do M.void parseLeftParent
     <|> do t <- lookAhead argTypes                                         
            genNewError (return t) PE.TokenLP                               -- proc id : in a :int) begin
     <|> do (t,_) <- anyToken                                               --          ^  
            manyTill  anyToken (lookAhead $ argTypes <|> parseRightParent)  -- proc id : [[$ in a :int) begin  
            genNewError (return t) PE.TokenLP                               --           ^^^
    
    newScopeParser
    targs <- listArgProc id parseRightParent parseRightParent

    try $do parseRightParent
            return Nothing
     <|> do t <- lookAhead $ parseBegin 
            genNewError (return t) PE.TokenRP 
            return Nothing
     <|> do (t:_) <- manyTill  anyToken (lookAhead parseBegin)
            genNewError (return $fst t) PE.TokenRP 
            return Nothing
                                                                            --           ^^^
    try $do M.void $ parseBegin
     <|> do t <- (lookAhead $ parseVar <|> parseTokLeftPre)                 -- proc id : (in a :int)   
            genNewError (return t) PE.Begin                                 --                       ^
     <|> do (t:_) <- manyTill anyToken (lookAhead $
                               parseVar <|> parseTokLeftPre)                -- proc id : (in a :int) [][]
            genNewError (return $fst t) PE.Begin                            --                       ^^^^

    

    dl   <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
    pre  <- precondition parseTokOpenBlock
    la   <- block parseTokLeftPost parseTokLeftPost
    post <- postcondition parseEnd
    try $do M.void parseEnd
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return t) PE.LexEnd
    sb <- getCurrentScope
    addProcTypeParser id targs (toLocation pos) sb
    exitScopeParser
    addProcTypeParser id targs (toLocation pos) sb
    return $ (M.liftM5 (DefProc id sb) la pre post (Just (EmptyAST GEmpty)) dl) 
                AP.<*> targs AP.<*> (return GEmpty)

    where 
        argTypes :: MyParser Token   
        argTypes = choice  [ parseIn
                          , parseOut
                          , parseInOut
                          , parseInOut
                          ] 


    -- do pos <- getPosition
    --    try (
    --       do parseProc
    --          try (
    --            do id <- parseID
    --               try (
    --                 do parseColon
    --                    try (
    --                      do parseLeftParent
    --                         newScopeParser
    --                         targs <- listArgProc id parseRightParent parseRightParent
    --                         try (
    --                           do parseRightParent
    --                              try (
    --                                do parseBegin
    --                                   dl   <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
    --                                   pre  <- precondition parseTokOpenBlock
    --                                   la   <- block parseTokLeftPost parseTokLeftPost
    --                                   post <- postcondition parseEnd
    --                                   try (
    --                                     do parseEnd
    --                                        sb   <- getCurrentScope
    --                                        addProcTypeParser id targs (toLocation pos) sb
    --                                        exitScopeParser
    --                                        addProcTypeParser id targs (toLocation pos) sb
    --                                        return $ (M.liftM5 (DefProc id sb) la pre post (Just (EmptyAST GEmpty)) dl) AP.<*> targs AP.<*> (return GEmpty)
    --                                       )
    --                                       <|> do genNewError follow PE.LexEnd
    --                                              return Nothing
    --                                   )
    --                                   <|> do genNewError follow PE.Begin
    --                                          return Nothing
    --                              )
    --                              <|> do genNewError follow PE.TokenRP
    --                                     return Nothing
    --                         )
    --                         <|> do genNewError follow PE.TokenLP
    --                                return Nothing
    --                    )
    --                    <|> do genNewError follow PE.Colon
    --                           return Nothing
    --               )
    --               <|> do genNewError follow PE.IDError
    --                      return Nothing
    --        )
    --        <|> do genNewError follow PE.ProcOrFunc
    --               return Nothing

listArgProc :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProc id follow recSet =do 
    try $do lookAhead follow
            return $ return []
     <|> do lookAhead parseEOF
            return Nothing
     <|> do ar <- (arg id (follow <|> parseComma) (recSet <|> parseComma)) `sepBy` (parseComma)
            return (Just $ foldr aux [] ar)
      where 
          aux Nothing  l = l
          aux (Just x) l = x:l


listArgProcAux :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Type)])
listArgProcAux id follow recSet =
    do lookAhead follow
       return $ return []
       <|> do lookAhead parseEOF
              return Nothing
       <|> try (
             do parseComma
                ar <- arg id (follow <|> parseComma) (recSet <|> parseComma)
                rl <- listArgProcAux id follow recSet
                return(AP.liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing


argType :: MyParser Token -> MyParser Token -> MyParser (Maybe TypeArg)
argType follow recSet =
    do lookAhead (parseIn <|> parseOut <|> parseInOut <|> parseRef)
       do parseIn
          return (return (In))
          <|> do parseOut
                 return (return (Out))
          <|> do parseInOut
                 return (return InOut)
          <|> do parseRef
                 return (return Ref)
    <|> do genNewError follow TokenArg
           return Nothing


arg :: T.Text -> MyParser Token -> MyParser Token -> MyParser (Maybe (T.Text, Type))
arg pid follow recSet =
    do at <- argType parseTokID (recSet <|> parseTokID)
       try (
         do id  <- parseID
            parseColon
            t   <- myType follow recSet
            pos <- getPosition
            addArgProcParser id pid t (toLocation pos) at
            return $ return (id, t)
           )
           <|> do genNewError follow PE.IDError
                  return Nothing

