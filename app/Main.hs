module Main where

import Control.Exception (finally, try)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (dropWhileEnd, find, genericLength)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Machine.Plan (await, yield)
import Data.Machine.Process ((~>), auto, filtered, Process, ProcessT, supply)
import Data.Machine.Runner (runT)
import Data.Machine.Source (iterated, Source)
import Data.Machine.Tee (capL, zipping)
import Data.Machine.Type (repeatedly)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Foreign.C.Types (CTime(CTime))
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), splitFileName)
import System.Fuse
import System.Posix.Types (COff(COff))
import System.IO (Handle, hClose, IOMode(ReadMode), openFile)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.IO.Machine (byLine, sourceHandle)

import DirBackingHandles

isOption :: String -> Bool
isOption ('-':_) = True
isOption _ = False

main :: IO ()
main = do
    args <- getArgs
    source <-
        maybe (fail "source not specified") return $ find (not . isOption) args
    fuseMain (orderedDirOps source) defaultExceptionHandler

orderedDirOps :: FilePath -> FuseOperations ()
orderedDirOps source =
    defaultFuseOps
    { fuseOpenDirectory = either return cleanup <=< openDirectory
    , fuseReadDirectory = readDirectory
    , fuseGetFileStat = getFileStat
    , fuseReadSymbolicLink = readSymbolicLink
    }
  where
    orderFile :: FilePath -> FilePath
    orderFile fp = source </> fp </> ".order"
    openDirectory :: FilePath -> IO (Either Errno DirBackingHandles)
    openDirectory fp = fmap (first toErrNo) . try $ do
      dl <- getDirectoryContents $ source </> fp
      fh <- openFile (orderFile fp) ReadMode
      return $ DirBackingHandles dl fh
      where
        toErrNo :: IOError -> Errno
        toErrNo e
          | isDoesNotExistError e = eNOENT
          | isPermissionError   e = ePERM
          | otherwise             = eFAULT
    readDirectory = doRead <=< openDirectory
      where
        doRead :: Either Errno DirBackingHandles -> IO (Either Errno [(FilePath, FileStat)])
        doRead (Right (DirBackingHandles d fh)) =
          Right <$> do
            let fileSet  = Set.fromList d
                lineNums :: Source Int
                lineNums = iterated succ 1
            lines <- runT (sourceHandle byLine fh ~> removeComments ~> capL lineNums zipping) `finally` hClose fh
            let nDigits = length . show $ length lines
            result <- runT . supply lines $ processOrderLine nDigits fileSet
            curTime <- liftIO getPOSIXTime
            return $ dot curTime : dotdot curTime : result
        doRead (Left l) = return $ Left l
    getFileStat fp = impl <$> readDirectory dir
      where
        (dir, filename) = splitFileName fp
        impl (Left l) = Left l
        impl (Right contents) =
          maybe (Left eNOENT) (Right . snd) $ find ((== filename) . fst) contents
    readSymbolicLink fp =
      let (dir, filename) = splitFileName fp
          target :: FilePath -> FilePath
          target symLinkName = source </> dir </> tail (dropWhile (/= '-') symLinkName)
          doReadLink :: Either Errno [(FilePath, FileStat)] -> Either Errno FilePath
          doReadLink (Left l) = Left l
          doReadLink (Right contents) =
            maybe (Left eNOENT) (Right . target) . find (== filename) $ fst <$> contents
      in doReadLink <$> readDirectory dir

dot :: POSIXTime -> (FilePath, FileStat)
dot curTime = (".", FileStat { statEntryType  = Directory
                    , statFileMode   = 0o555
                    , statLinkCount  = 2
                    , statFileOwner  = 0
                    , statFileGroup  = 0
                    , statSpecialDeviceID = 0
                    , statFileSize   = 4096
                    , statBlocks     = 0
                    , statAccessTime = CTime $ floor curTime
                    , statModificationTime = CTime $ floor curTime
                    , statStatusChangeTime = CTime $ floor curTime
                    })

dotdot = first ('.' :) . dot

removeComments :: Process String String
removeComments =
  auto (dropWhileEnd (== ' ') . takeWhile (/= '#')) ~> filtered (not . null)

processOrderLine :: Int -> Set FilePath -> ProcessT IO (Int, String) (FilePath, FileStat)
processOrderLine nDigits fileSet = repeatedly $ do
  (lineNum, filename) <- await
  when (not $ Set.member filename fileSet) . liftIO $ fail "file not found"
  curTime <- liftIO getPOSIXTime
  let fs = FileStat { statEntryType  = SymbolicLink
                    , statFileMode   = 0o777
                    , statLinkCount  = 1
                    , statFileOwner  = 0
                    , statFileGroup  = 0
                    , statSpecialDeviceID = 0
                    , statFileSize   = 0
                    , statBlocks     = 0
                    , statAccessTime = CTime $ floor curTime
                    , statModificationTime = CTime $ floor curTime
                    , statStatusChangeTime = CTime $ floor curTime
                    }
  yield (pad nDigits (show lineNum) ++ '-' : filename, fs)

pad :: Int -> String -> String
pad nDigits str = replicate (nDigits - length str) '0' ++ str
