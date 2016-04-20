module Paths_produktservice (
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

bindir     = "/Users/christophhegemann/Documents/praxisprojekt/produktservice/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin"
libdir     = "/Users/christophhegemann/Documents/praxisprojekt/produktservice/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/lib/x86_64-osx-ghc-7.10.3/produktservice-0.1.0.0-80p7D0JC9UF464h9M4qqf7"
datadir    = "/Users/christophhegemann/Documents/praxisprojekt/produktservice/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/share/x86_64-osx-ghc-7.10.3/produktservice-0.1.0.0"
libexecdir = "/Users/christophhegemann/Documents/praxisprojekt/produktservice/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/libexec"
sysconfdir = "/Users/christophhegemann/Documents/praxisprojekt/produktservice/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "produktservice_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "produktservice_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "produktservice_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "produktservice_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "produktservice_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
