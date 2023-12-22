module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

import System.Fuse

import Control.Monad
import Data.List (find, head, intercalate, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text (pack, splitOn, stripSuffix, unpack)
import Debug.Trace (trace)
import System.Directory.Tree

instance Show Errno where
  show (Errno i) = show i

type HT = ()

type Tree = DirTree ()

tree :: IO Tree
tree = void . Dir "" . return . dirTree <$> readDirectory "/home/s/cache/code/hfuse/notes/tree"

type Path = [String]

toPath :: String -> Path
toPath s = map unpack . splitOn (pack "/") . fromMaybe (pack s) . stripSuffix (pack "/") . pack $ s

fromPath :: Path -> String
fromPath = intercalate "/"

entName :: DirTree a -> FileName
entName (Dir n _) = n
entName (File n _) = n
entName (Failed n _) = n

entStat :: Tree -> FuseContext -> FileStat
entStat (Dir _ _) = dirStat
entStat (File _ _) = fileStat

main :: IO ()
main = do
  t <- tree
  print t
  fuseMain (helloFSOps t) defaultExceptionHandler

d :: (Show a) => String -> (String -> IO a) -> String -> IO a
d n f s = do
  out <- f s
  print (n, toPath s, s, "=", out)
  return out

helloFSOps :: Tree -> FuseOperations HT
helloFSOps t =
  defaultFuseOps
    { fuseGetFileStat = d "fuseGetFileStat" $ helloGetFileStat . findFile t . toPath
    , fuseOpen = helloOpen
    , fuseRead = helloRead
    , fuseOpenDirectory = d "fuseOpenDirectory" $ helloOpenDirectory . findFile t . toPath
    , fuseReadDirectory = d "fuseReadDirectory" $ helloReadDirectory . findFile t . toPath
    , fuseGetFileSystemStats = helloGetFileSystemStats
    }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"
helloPath :: FilePath
helloPath = "/hello"

dirStat ctx =
  FileStat
    { statEntryType = Directory
    , statFileMode =
        foldr1
          unionFileModes
          [ ownerReadMode
          , ownerExecuteMode
          , groupReadMode
          , groupExecuteMode
          , otherReadMode
          , otherExecuteMode
          ]
    , statLinkCount = 2
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

fileStat ctx =
  FileStat
    { statEntryType = RegularFile
    , statFileMode =
        foldr1
          unionFileModes
          [ ownerReadMode
          , groupReadMode
          , otherReadMode
          ]
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = fromIntegral $ B.length helloString
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

findFile :: Tree -> Path -> Maybe Tree
findFile d@(Dir name _) [name'] | name == name' = Just d
findFile (Dir name cont) (name' : rest) | name == name' = fmap fst . uncons $ mapMaybe (`findFile` rest) cont
findFile f@(File name _) (name' : rest) | name == name' = Just f
findFile _ _ = Nothing

helloGetFileStat :: Maybe Tree -> IO (Either Errno FileStat)
helloGetFileStat (Just (Dir _ _)) = Right . dirStat <$> getFuseContext
helloGetFileStat (Just (File _ _)) = Right . fileStat <$> getFuseContext
helloGetFileStat Nothing = return (Left eNOENT)

helloOpenDirectory (Just (Dir _ _)) = return eOK
helloOpenDirectory _ = return eNOENT

helloReadDirectory :: Maybe Tree -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory (Just (Dir _ contents)) = do
  ctx <- getFuseContext
  return $ Right ([(".", dirStat ctx), ("..", dirStat ctx)] ++ others ctx)
 where
  others ctx = map (\e -> (entName e, entStat e ctx)) contents
helloReadDirectory Nothing = return (Left eNOENT)

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode flags
  | path == helloPath = case mode of
      ReadOnly -> return (Right ())
      _ -> return (Left eACCES)
  | otherwise = return (Left eNOENT)

helloRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
helloRead path _ byteCount offset
  | path == helloPath =
      return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
  | otherwise = return $ Left eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $
    Right $
      FileSystemStats
        { fsStatBlockSize = 512
        , fsStatBlockCount = 1
        , fsStatBlocksFree = 1
        , fsStatBlocksAvailable = 1
        , fsStatFileCount = 5
        , fsStatFilesFree = 10
        , fsStatMaxNameLength = 255
        }
