{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax, NamedFieldPuns #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.Foldable as F
import Data.Functor.Base
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (sort, sortBy, (\\))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Traversable as T
import Data.Tree
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import System.Posix.Files
import System.Posix.Types
import Text.Pretty.Simple

import Control.Applicative
import System.Posix.IO (createFile)

type FileName = String

data File c = File { fname :: FileName, fmode :: FileMode, fcont :: c } deriving (Show)

data DirTree
  = Branch (File ()) [DirTree]
  | Leaf (File String)
  deriving (Show)

makeBaseFunctor ''DirTree

-- https://github.com/recursion-schemes/recursion-schemes/issues/146
sequencer :: (Recursive r, Corecursive r, Traversable t, Base r ~ t, Monad m) => Compose m t (m r) -> m r
sequencer = fmap embed . (sequence <=< getCompose)

unfoldM :: (Monad m, Recursive c, Corecursive c, Traversable (Base c)) => (a -> m (Base c a)) -> a -> m c
unfoldM = refold sequencer . fmap Compose

type RootWithName = (FilePath, FilePath)

load :: RootWithName -> IO (DirTreeF RootWithName)
load (root, name) = do
  isFile <- doesFileExist path
  mode <- fileMode <$> getFileStatus path
  if isFile
    then do
      contents <- readFile path
      pure $ LeafF (File name mode contents)
    else do
      cs <- listDirectory path
      pure $ BranchF (File name mode ()) (fmap (path,) cs)
 where
  path = root </> name

dump :: DirTreeF (String -> IO ()) -> (String -> IO ())
dump (LeafF (File name mode contents)) = \root -> writeFile (root </> name) contents
dump (BranchF (File name mode ()) next) = \root -> do
  let path = root </> name
  createDirectoryIfMissing False path
  mapM_ ($ path) next

renameCopy :: DirTree -> DirTree
renameCopy (Branch f@File{fname} rest) = Branch (f {fname = fname ++ "-copy" }) rest
renameCopy (Leaf f@File{fname}) = Leaf (f {fname = fname ++ "-copy" })

copy :: FilePath -> (DirTree -> DirTree) -> IO ()
copy from rename = flip (cata dump) "" . rename =<< unfoldM load ("", from)

main :: IO ()
main = do
  paths <- getArgs
  forM_ paths $ \path -> do
    copy path renameCopy
