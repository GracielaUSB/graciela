module Parser.Procedures
  ( arg
  , argFunc
  , argumentType
  , followTypeFunction
  , function
  , listArgFunc
  , listArgFuncAux
  , listArgProc
  , listArgProcAux
  , listDefProc
  , proc
  , procOrFunc
  ) where

-------------------------------------------------------------------------------
import           AST
import           Contents
import           Graciela
import           MyParseError        as PE
import           Parser.Assertions
import           Parser.Declarations
import           Parser.Expression
import           Parser.Instructions
import           Parser.Token
import           Parser.Type
import           Parser.State
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Applicative as AP (liftA2)
import           Control.Monad       (void, liftM5)
import qualified Data.Text           as T
import           Text.Megaparsec     hiding (Token)
-------------------------------------------------------------------------------

listDefProc :: Graciela Token -> Graciela Token -> Graciela (Maybe [AST])
listDefProc follow recSet =
    do lookAhead eof
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do pf <- procOrFunc  follow recSet
              rl <- listDefProc follow recSet
              return (liftA2 (:) pf rl)
       <|> return Nothing


procOrFunc :: Graciela Token -> Graciela Token -> Graciela (Maybe AST )
procOrFunc follow recSet =
    try $ do
        lookAhead (match TokProc <|> match TokId)
        proc

    <|> try (do lookAhead (match TokFunc <|> match TokId)
                function (follow <|> match TokFunc <|> match TokProc) recSet)
    <|> do genNewError (follow <|> match TokFunc <|> match TokProc) PE.ProcOrFunc
           do lookAhead follow
              return Nothing
           <|> do lookAhead (match TokProc <|> match TokFunc)
                  procOrFunc follow recSet
                  return Nothing

                  -- choice [function (follow) recSet, proc]
                  --         <|> do parseEnd
                  --                return Nothing


followTypeFunction :: Graciela Token
followTypeFunction = match TokOpenBlock <|> match TokLeftBound


function :: Graciela Token -> Graciela Token -> Graciela (Maybe AST )
function follow recSet = do
    pos <- getPosition

    do match TokFunc
    id <- identifier
    match TokLeftPar (match TokId <|> (match TokRightPar)) PE.TokenLP  -- (
    newScopeParser
    lt <- listArgFunc id (match TokRightPar) (match TokRightPar)                  -- arguments
    match TokRightPar 
    match TokArrow
    tname <- identifier
    retType <- getType
    when (retType == GError) $ void $genCustomError ("El tipo `"++unpack tname++"` no existe.")
    match TokBegin
    sb <- getCurrentScope
    addFunTypeParser id lt retType pos sb
    b <- conditionalOrExpr
    exitScopeParser
    addFunTypeParser id lt retType pos sb
    match TokEnd
            
    where
        conditionalOrExpr =  conditional CExpression (match TokEnd) (match TokEnd) 
                         <|> expr (match TokEnd) (match TokEnd)


listArgFunc :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, Type)])
listArgFunc idf follow recSet =
    do lookAhead eof
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> do ar <- argFunc idf (follow <|> match TokComma) (recSet <|> match TokComma)
              rl <- listArgFuncAux idf follow recSet
              return(liftA2 (:) ar rl)


argFunc :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe (T.Text, Type))
argFunc idf follow recSet =
    try ( do id <- identifier
             try ( do match TokColon
                      t  <- myType follow follow
                      pos <- getPosition
                      addFunctionArgParser idf id t pos
                      return $ return (id, t)
                 )
                 <|> do genNewError follow PE.Colon
                        return Nothing
        )
        <|> do genNewError follow PE.IdError
               return Nothing


listArgFuncAux :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, Type)])
listArgFuncAux idf follow recSet =
    do lookAhead eof
       return Nothing
       <|> do lookAhead follow
              return $ return []
       <|> try ( do match TokComma
                    ar <- argFunc idf (follow <|> match TokComma) (recSet <|> match TokComma)
                    rl <- listArgFuncAux idf follow recSet
                    return(liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing

proc :: {-Graciela Token -> Graciela Token ->-} Graciela (Maybe AST )
proc {-follow recSet-} = do
    pos <- getPosition
    match TokProc
    id <- identifier
    match TokLeftPar
    newScopeParser
    targs <- listArgProc id (match TokRightPar) (match TokRightPar)               -- arguments
    match TokRightPar
    notFollowedBy $ match TokArrow
    try $do match TokBegin
    dl   <- decListWithRead (match TokLeftPre) (match TokLeftPre)
    pre  <- precondition $ match TokOpenBlock
    la   <- block (match TokLeftPost) (match TokLeftPost)
    post <- postcondition $ match TokEnd
    try $do match TokEnd
    sb   <- getCurrentScope
    addProcTypeParser id targs pos sb
    exitScopeParser
    addProcTypeParser id targs pos sb
    return $ liftM5 (DefProc id sb) la pre post (Just (EmptyAST GEmpty)) dl
                <*> targs <*> return GEmpty
    where
        argTypes :: Graciela Token
        argTypes = choice   [ match TokIn
                            , match TokOut
                            , match TokInOut
                            , match TokRef
                            ]

listArgProc :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, Type)])
listArgProc id follow recSet =do
    try $do lookAhead follow
            return $ return []
     <|> do lookAhead eof
            return Nothing
     <|> do ar <- arg id (follow <|> match TokComma) (recSet <|> match TokComma) `sepBy` match TokComma
            return (Just $ foldr aux [] ar)
      where
          aux Nothing  l = l
          aux (Just x) l = x:l


listArgProcAux :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe [(T.Text, Type)])
listArgProcAux id follow recSet =
    do lookAhead follow
       return $ return []
       <|> do lookAhead eof
              return Nothing
       <|> try (
             do match TokComma
                ar <- arg id (follow <|> match TokComma) (recSet <|> match TokComma)
                rl <- listArgProcAux id follow recSet
                return(liftA2 (:) ar rl)
               )
               <|> do genNewError follow PE.Comma
                      return Nothing

argumentType :: Graciela Token -> Graciela Token -> Graciela (Maybe TypeArg)
argumentType follow recSet =
    do lookAhead (match TokIn <|> match TokOut <|> match TokInOut <|> match TokRef)
       do match TokIn
          return (return In)
          <|> do match TokOut
                 return (return Out)
          <|> do match TokInOut
                 return (return InOut)
          <|> do match TokRef
                 return (return Ref)
    <|> do genNewError follow TokenArg
           return Nothing


arg :: T.Text -> Graciela Token -> Graciela Token -> Graciela (Maybe (T.Text, Type))
arg pid follow recSet =
    do at <- argumentType (match TokId) (recSet <|> match TokId)
       try (
         do id  <- identifier
            match TokColon
            t   <- myType follow recSet
            pos <- getPosition
            addArgProcParser id pid t pos at
            return $ return (id, t)
           )
           <|> do genNewError follow PE.IdError
                  return Nothing

-- Deberian estar en el lugar adecuando, hasta ahora aqui porq no le he usado en archivos q no dependen de Procedure

panicModeId :: Graciela Token -> Graciela T.Text
panicModeId token follow =
        try identifier
    <|> do t <- lookAhead follow
           genNewError (return t) PE.IdError
           return $ T.pack "No Id"
    <|> do (t:_) <- anyToken `manyTill` lookAhead follow
           genNewError (return $fst t) PE.IdError
           return $ T.pack "No Id"


panicMode :: Graciela Token -> Graciela Token -> ExpectedToken -> Graciela ()
panicMode token follow err =
        try (void token)
    <|> do t <- lookAhead follow
           genNewError (return t) err
    <|> do (t:_) <- anyToken `manyTill` lookAhead follow
           genNewError (return $ fst t) err
