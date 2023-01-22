{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Assignment2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/bin"
libdir     = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/lib/x86_64-linux-ghc-9.2.5/Assignment2-0.1.0.0-8fZrP0sfekQ9Rz5EXC1I4C"
dynlibdir  = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/share/x86_64-linux-ghc-9.2.5/Assignment2-0.1.0.0"
libexecdir = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/libexec/x86_64-linux-ghc-9.2.5/Assignment2-0.1.0.0"
sysconfdir = "/u/nharris/cs457-func-prog/Assignment2/.stack-work/install/x86_64-linux-tinfo6/d911630e716144da2699e6edf6c25398ca8bac58aae79855b671b238a6dbc34f/9.2.5/etc"

getBinDir     = catchIO (getEnv "Assignment2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Assignment2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Assignment2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Assignment2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Assignment2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Assignment2_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
