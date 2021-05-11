{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monad (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/andy/.cabal/bin"
libdir     = "/home/andy/.cabal/lib/x86_64-linux-ghc-8.10.1/monad-0.1.0.0-inplace"
dynlibdir  = "/home/andy/.cabal/lib/ghc-8.10.1/monad-0.1.0.0-inplace"
datadir    = "/home/andy/.cabal/share/x86_64-linux-ghc-8.10.1/monad-0.1.0.0"
libexecdir = "/home/andy/.cabal/libexec/x86_64-linux-ghc-8.10.1/monad-0.1.0.0"
sysconfdir = "/home/andy/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
