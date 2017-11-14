{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_breakout (
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

bindir     = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\bin"
libdir     = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\lib\\x86_64-windows-ghc-8.0.2\\breakout-0.1.0.0-KalZ2SmhAcs6JDFHIhjRIs"
dynlibdir  = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\share\\x86_64-windows-ghc-8.0.2\\breakout-0.1.0.0"
libexecdir = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\libexec"
sysconfdir = "C:\\Users\\msinton\\git\\breakout\\.stack-work\\install\\d1995115\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "breakout_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "breakout_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "breakout_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "breakout_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "breakout_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "breakout_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
