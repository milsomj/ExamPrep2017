module Paths_lab02 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\workspaceY3\\Haskell\\CS3016-1617-master\\master\\Lab02\\.stack-work\\install\\a64c1a55\\bin"
libdir     = "D:\\workspaceY3\\Haskell\\CS3016-1617-master\\master\\Lab02\\.stack-work\\install\\a64c1a55\\lib\\x86_64-windows-ghc-7.10.3\\lab02-0.1.0.0-32O1gbkGIei2gfwI1nApRF"
datadir    = "D:\\workspaceY3\\Haskell\\CS3016-1617-master\\master\\Lab02\\.stack-work\\install\\a64c1a55\\share\\x86_64-windows-ghc-7.10.3\\lab02-0.1.0.0"
libexecdir = "D:\\workspaceY3\\Haskell\\CS3016-1617-master\\master\\Lab02\\.stack-work\\install\\a64c1a55\\libexec"
sysconfdir = "D:\\workspaceY3\\Haskell\\CS3016-1617-master\\master\\Lab02\\.stack-work\\install\\a64c1a55\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lab02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lab02_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lab02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
