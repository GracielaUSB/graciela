{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cola.Cola
  ( correrSimulacion
  , Comandos (..)
  ) where
--------------------------------------------------------------------------------
import           Shared
--------------------------------------------------------------------------------
import           Control.Monad.RWS
import           Data.Char                 (chr)
import           Data.Foldable             (toList)
import           Data.Int                  (Int32)
import           Data.Sequence             (Seq, ViewL ((:<)), (|>))
import qualified Data.Sequence             as Seq
import           Data.Text                 hiding (drop, length, null,
                                            takeWhile, unlines)
import           Data.Typeable
import           Prelude                   hiding (unwords)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
--------------------------------------------------------------------------------

tam :: Int
tam = 100

nLines :: Int
nLines = tam*5
--------------------------------------------------------------------------------

data Comando
  = Encolar Basic
  | Desencolar
  | Vacia
  | Cabeza
  | Imprimir
  | Salir
  deriving (Eq, Ord)

instance Show Comando where
  show = \case
    Encolar t  -> "e " <> showIn t
    Desencolar -> "d"
    Vacia      -> "v"
    Cabeza     -> "c"
    Imprimir   -> "p"
    Salir      -> "s"

data Comandos = Comandos Basic [Comando] deriving (Eq, Ord)

instance Arbitrary Comandos where
  arbitrary = do
    w <- elements [BBool False, BChar '\0', BFloat 0, BInt 0]
    Comandos w <$> vectorOf nLines (arb' w)
    where
      arb' w = frequency
        [ (600, Encolar <$> arb'' w)
        , (200, pure Desencolar)
        , (100, pure Vacia)
        , (150, pure Cabeza)
        , (100, pure Imprimir)
        , (5  , pure Salir) ]
      arb'' w = case w of
        BBool  {} -> BBool  <$> (arbitrary :: Gen Bool)
        BChar  {} -> BChar  <$> choose ('!', '~')
        BFloat {} -> BFloat <$> (arbitrary :: Gen Double)
        BInt   {} -> BInt   <$> (arbitrary :: Gen Int32)

instance Show Comandos where
  show (Comandos w cs) =
    letter <> "\n" <> show nLines <> "\n" <> unlines (show <$> cs)
    where
      letter = case w of
        BBool  {} -> "b"
        BChar  {} -> "c"
        BFloat {} -> "f"
        BInt   {} -> "i"
--------------------------------------------------------------------------------

correrSimulacion :: Comandos -> String
correrSimulacion cs = unpack . snd $ evalRWS (simular cs) tam Seq.empty
  where
    simular :: Comandos -> RWS Int Text (Seq Basic) ()
    simular (Comandos _ cs) =
      forM_ (takeWhile (/= Salir) cs <> [Imprimir]) simularUno

    simularUno :: Comando -> RWS Int Text (Seq Basic) ()
    simularUno = \case
      Encolar t  -> do
        maxn <- ask
        n <- gets length
        if n < maxn
          then modify (|> t)
          else tell "No.\n"
      Desencolar -> do
        gets null >>= \x -> when x (tell "No.\n")
        modify (Seq.drop 1)
      Vacia      -> gets null >>= tell . toLower . pack . show >> tell "\n"
      Cabeza     -> do
        gets Seq.viewl >>= \case
          Seq.EmptyL -> tell "No.\n"
          t :< _     -> tell (pack . show $ t) >> tell "\n"
      Imprimir   -> do
        gets (unwords . toList . fmap (pack . show)) >>= tell
        tell "\n"
      Salir      -> pure ()
