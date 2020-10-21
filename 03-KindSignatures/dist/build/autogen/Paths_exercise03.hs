{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercise03 (
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
libdir     = "/Users/oishikg/.cabal/lib/x86_64-osx-ghc-8.6.5/exercise03-0.1.0.0-Ew2Ns1beViL5A45Szpr0wD"
dynlibdir  = "/Users/oishikg/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/oishikg/.cabal/share/x86_64-osx-ghc-8.6.5/exercise03-0.1.0.0"
libexecdir = "/Users/oishikg/.cabal/libexec/x86_64-osx-ghc-8.6.5/exercise03-0.1.0.0"
sysconfdir = "/Users/oishikg/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercise03_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercise03_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercise03_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercise03_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercise03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercise03_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
