{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_addition (
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

bindir     = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/bin"
libdir     = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/lib/x86_64-osx-ghc-8.10.4/addition-0.1.0.0-HcpxTb6qiOm47omS6YdHYs"
dynlibdir  = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/share/x86_64-osx-ghc-8.10.4/addition-0.1.0.0"
libexecdir = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/libexec/x86_64-osx-ghc-8.10.4/addition-0.1.0.0"
sysconfdir = "/Users/andy/Projects/Haskell/from-first-principle/chapter14.Testing/addition/.stack-work/install/x86_64-osx/1dc7cd460c5004a74396be83b7f1032dc7c136c9ce8fa89541287e4525936e39/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
