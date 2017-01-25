{-# LANGUAGE CPP #-}

module Language.Graciela.OS 
  ( isLinux
  , isMac
  , isWindows 
  ) where

-- OS Checking -------------------------
-- Taken from hackage.haskell.org/package/extra-1.5.1/docs/System-Info-Extra.html

-- > isWindows == (os == "mingw32")
isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

-- | Return 'True' on Mac OS X and 'False' otherwise.
isMac :: Bool
#if defined(darwin_HOST_OS)
isMac = True
#else
isMac = False
#endif

-- | Return 'True' on Mac OS X and 'False' otherwise.
isLinux :: Bool
#if defined(linux_HOST_OS)
isLinux = True
#else
isLinux = False
#endif

