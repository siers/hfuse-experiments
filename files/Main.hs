{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

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

type FileName = String

data File c = File { fname :: FileName, fmode :: FileMode, fcont :: c } deriving (Show)

data LeafDirTree
  = Branch (File ()) [LeafDirTree]
  | Leaf (File String)
  deriving (Show)

data DirTree c
  = TDir {name :: FileName, contents :: [DirTree c]}
  | TFile {name :: FileName, file :: c}
  deriving (Show)

makeBaseFunctor ''DirTree
makeBaseFunctor ''LeafDirTree

sequencer :: (Recursive r, Corecursive r, Traversable t, Base r ~ t, Monad m) => Compose m t (m r) -> m r
sequencer = fmap embed . (sequence <=< getCompose)

unfoldM :: (Monad m, Recursive c, Corecursive c, Traversable (Base c)) => (a -> m (Base c a)) -> a -> m c
unfoldM = refold sequencer . fmap Compose

build :: (FilePath, FilePath) -> IO (DirTreeF () (FilePath, FilePath))
build (root, name) = do
  isFile <- doesFileExist path
  if isFile
    then do
      stat <- getFileStatus path
      pure $ TFileF name ()
    else do
      cs <- listDirectory path
      pure $ TDirF name (fmap (path,) cs)
 where
  path = root </> name

type RootWithName = (FilePath, FilePath)

buildTree :: RootWithName -> IO (LeafDirTreeF RootWithName)
buildTree (root, name) = do
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

main :: IO ()
main = do
  paths <- getArgs

  forM_ paths $ \path -> do
    tree <- unfoldM build ("", path) :: IO (DirTree ())
    pPrint tree

  forM_ paths $ \path -> do
    tree <- unfoldM buildTree ("", path) :: IO LeafDirTree
    pPrint tree
