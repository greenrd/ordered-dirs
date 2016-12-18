module DirBackingHandles where

import System.IO (Handle, hClose)
import System.Fuse

data DirBackingHandles = DirBackingHandles { underlyingDir   :: [FilePath]
                                           , orderFileHandle :: Handle
                                           }

cleanup :: DirBackingHandles -> IO Errno
cleanup (DirBackingHandles _ f) = do
  hClose f
  return eOK
