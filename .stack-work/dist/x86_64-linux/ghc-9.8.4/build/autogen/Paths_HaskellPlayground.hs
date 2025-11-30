{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_HaskellPlayground (
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
bindir     = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/bin"
libdir     = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/lib/x86_64-linux-ghc-9.8.4/HaskellPlayground-0.1.0.0-44xUqQLNpJ1IUavgJzq7Z"
dynlibdir  = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/lib/x86_64-linux-ghc-9.8.4"
datadir    = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/share/x86_64-linux-ghc-9.8.4/HaskellPlayground-0.1.0.0"
libexecdir = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/libexec/x86_64-linux-ghc-9.8.4/HaskellPlayground-0.1.0.0"
sysconfdir = "/home/programming/HaskellPlayground/.stack-work/install/x86_64-linux/9120d2fd4ec6967d70cea87c524224f81b8ff8c878d1f2f2d34ca65b1709625e/9.8.4/etc"

getBinDir     = catchIO (getEnv "HaskellPlayground_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "HaskellPlayground_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "HaskellPlayground_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "HaskellPlayground_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellPlayground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellPlayground_sysconfdir") (\_ -> return sysconfdir)



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
