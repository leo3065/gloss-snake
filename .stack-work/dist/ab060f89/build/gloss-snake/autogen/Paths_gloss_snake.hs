{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gloss_snake (
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
bindir     = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\bin"
libdir     = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\lib\\x86_64-windows-ghc-9.6.4\\gloss-snake-0.1.0.0-5DP4ZWMQBgx5YCHHufpPXK-gloss-snake"
dynlibdir  = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\lib\\x86_64-windows-ghc-9.6.4"
datadir    = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\share\\x86_64-windows-ghc-9.6.4\\gloss-snake-0.1.0.0"
libexecdir = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\libexec\\x86_64-windows-ghc-9.6.4\\gloss-snake-0.1.0.0"
sysconfdir = "D:\\Haskell Projects\\gloss\\gloss-snake\\.stack-work\\install\\dc27e907\\etc"

getBinDir     = catchIO (getEnv "gloss_snake_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gloss_snake_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gloss_snake_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gloss_snake_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_snake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_snake_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
