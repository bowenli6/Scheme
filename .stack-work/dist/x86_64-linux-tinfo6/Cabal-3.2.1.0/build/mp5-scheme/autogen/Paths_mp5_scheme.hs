{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp5_scheme (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/bin"
libdir     = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/lib/x86_64-linux-ghc-8.10.7/mp5-scheme-0.2.0.0-CHegeXpFnh59B9qbCKR8EF-mp5-scheme"
dynlibdir  = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/share/x86_64-linux-ghc-8.10.7/mp5-scheme-0.2.0.0"
libexecdir = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/libexec/x86_64-linux-ghc-8.10.7/mp5-scheme-0.2.0.0"
sysconfdir = "/home/bowen/Developer/CS-421-UIUC/mp5/mps/mp5-scheme/.stack-work/install/x86_64-linux-tinfo6/78e35a80d96995ba5ae4ebddf924c2494b9abf181a20b494049130c833d49555/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp5_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp5_scheme_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp5_scheme_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp5_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp5_scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp5_scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
