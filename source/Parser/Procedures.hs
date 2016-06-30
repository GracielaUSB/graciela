module Parser.Procedures
    ( arg
    , argFunc
    , argType
    , followTypeFunction
    , function
    , listArgFunc
    , listArgFuncAux
    , listArgProc
    , listArgProcAux
    , listDefProc
    , panicMode
    , panicModeID
    , proc
    , procOrFunc
    ) where

-------------------------------------------------------------------------------
import AST
import Contents
import Location
import MyParseError                  as PE
import Parser.Assertions
import Parser.Declarations
import Parser.Expression
import Parser.Instructions
import Parser.TokenParser
import ParserState
import Parser.ParserType
import State
import Token
import Type
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
       <|> return Nothing


procOrFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
procOrFunc follow recSet =
    try $ do
        lookAhead (parseProc <|> parseTokID)
        proc

    <|> try (do lookAhead (parseFunc <|> parseTokID)
                function (follow <|> parseFunc <|> parseProc) recSet)
    <|> do genNewError (follow <|> parseFunc <|> parseProc) PE.ProcOrFunc
           do lookAhead follow
              return Nothing
           <|> do lookAhead (parseProc <|> parseFunc)
                  procOrFunc follow recSet
                  return Nothing


                  -- choice [function (follow) recSet, proc]
                  --         <|> do parseEnd
                  --                return Nothing


followTypeFunction :: MyParser Token
followTypeFunction = parseTokOpenBlock <|> parseTokLeftBound


function :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST Type) )
function follow recSet = do
    pos <- getPosition

    do M.void parseFunc
        <|> do try $ do t <- parseID
                        lookAhead parseID
                        genNewError (return $TokId t) PE.ProcOrFunc
        <|> do t <- lookAhead parseID
               genNewError (return $TokId t) PE.ProcOrFunc
        <|> do (t:_) <- manyTill anyToken (lookAhead $ parseID)
               genNewError (return $fst t) PE.ProcOrFunc

    id <- panicModeID parseColon                                            -- ID
    -- panicMode parseColon parseLeftParent PE.Colon                           -- :
    panicMode parseLeftParent (parseTokID <|> parseRightParent) PE.TokenLP  -- (
    newScopeParser
    lt <- listArgFunc id parseRightParent (parseRightParent)                -- arguments
    panicMode parseRightParent parseTokLeftPre PE.TokenRP                   -- )

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
        parseType' = myType (followTypeFunction) (followTypeFunction)

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

proc :: {-MyParser Token -> MyParser Token ->-} MyParser (Maybe (AST Type) )
proc {-follow recSet-} = do
    pos <- getPosition

    M.void parseProc
        <|> do try $ do t <- parseID
                        lookAhead parseID
                        genNewError (return $TokId t) PE.ProcOrFunc
        <|> do t <- lookAhead parseID
               genNewError (return $TokId t) PE.ProcOrFunc
        <|> do (t:_) <- manyTill anyToken (lookAhead $ parseID)
               genNewError (return $fst t) PE.Begin                            -- proc

    id <- panicModeID parseColon                                            -- ID
    -- panicMode parseColon parseLeftParent PE.Colon                           -- :
    panicMode parseLeftParent (argTypes <|> parseRightParent) PE.TokenLP    -- (
    newScopeParser
    targs <- listArgProc id parseRightParent parseRightParent               -- arguments
    panicMode parseRightParent parseTokLeftPre PE.TokenRP                   -- )
    notFollowedBy parseArrow
    try $do M.void $ parseBegin
     <|> do t <- (lookAhead $ parseVar <|> parseTokLeftPre)
            genNewError (return t) PE.Begin                                 -- begin
     <|> do (t:_) <- manyTill anyToken (lookAhead $
                               parseVar <|> parseTokLeftPre)
            genNewError (return $fst t) PE.Begin
    dl   <- decListWithRead parseTokLeftPre (parseTokLeftPre)               -- declarations
    pre  <- precondition parseTokOpenBlock                                  -- pre
    la   <- block parseTokLeftPost parseTokLeftPost                         -- body
    post <- postcondition parseEnd                                          -- post
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
        argTypes = choice   [ parseIn
                            , parseOut
                            , parseInOut
                            , parseRef
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

-- Deberian estar en el lugar adecuando, hasta ahora aqui porq no le he usado en archivos q no dependen de Procedure

panicModeID :: MyParser Token -> MyParser T.Text
panicModeID follow =
        try parseID
    <|> do t <- lookAhead $ follow
           genNewError (return t) PE.IDError
           return $ T.pack "No ID"
    <|> do (t:_) <- manyTill anyToken (lookAhead follow)
           genNewError (return $fst t) PE.IDError
           return $ T.pack "No ID"


panicMode :: MyParser Token -> MyParser Token -> ExpectedToken -> MyParser ()
panicMode token follow err =
        try (M.void token)
    <|> do t <- lookAhead follow
           genNewError (return t) err
    <|> do (t:_) <- anyToken `manyTill` lookAhead follow
           genNewError (return $ fst t) err
