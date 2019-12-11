{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_calculator (
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

bindir     = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/bin"
libdir     = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/lib/x86_64-osx-ghc-8.6.5/calculator-0.1.0.0-Df4OSwgVTPSAHGImoyrHHv-calculator"
dynlibdir  = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/share/x86_64-osx-ghc-8.6.5/calculator-0.1.0.0"
libexecdir = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/libexec/x86_64-osx-ghc-8.6.5/calculator-0.1.0.0"
sysconfdir = "/Users/ethanleba/Documents/haskellexercises/calculator/calculator/.stack-work/install/x86_64-osx/c4135d3a1a72b98f0cb5115db8366b50bb4830ad5ade7df90c3b5e358fb09d1e/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "calculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "calculator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "calculator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "calculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
