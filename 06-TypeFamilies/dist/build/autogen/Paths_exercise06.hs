{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercise06 (
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

bindir     = "/Users/oishikg/.cabal/bin"
libdir     = "/Users/oishikg/.cabal/lib/x86_64-osx-ghc-8.6.5/exercise06-0.1.0.0-ySUvtv6CtOKy5fdhAAdiD"
dynlibdir  = "/Users/oishikg/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/oishikg/.cabal/share/x86_64-osx-ghc-8.6.5/exercise06-0.1.0.0"
libexecdir = "/Users/oishikg/.cabal/libexec/x86_64-osx-ghc-8.6.5/exercise06-0.1.0.0"
sysconfdir = "/Users/oishikg/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercise06_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercise06_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercise06_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercise06_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercise06_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercise06_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
