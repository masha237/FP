{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_hw1 (
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

bindir     = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/bin"
libdir     = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/lib/x86_64-linux-ghc-9.0.2/hw1-0.1.0.0-3pNIpedeSfz6Q4QI1PIV9w"
dynlibdir  = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/share/x86_64-linux-ghc-9.0.2/hw1-0.1.0.0"
libexecdir = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/libexec/x86_64-linux-ghc-9.0.2/hw1-0.1.0.0"
sysconfdir = "/home/masha/FP/hw1/hw1/.stack-work/install/x86_64-linux-tinfo6/136ad0007e439cc51a2bd664da275ca00a0506c5be3149b84950f0fd7e96ffea/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
