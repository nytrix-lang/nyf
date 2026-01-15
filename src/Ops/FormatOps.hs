{-# LANGUAGE OverloadedStrings #-}
module Ops.FormatOps where

import qualified Formatter
import qualified Cli
import qualified FormatConfig
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM_, filterM)
import System.Exit (exitFailure)

formatPath :: FilePath -> Cli.FormatOptions -> IO ()
formatPath path opts = do
  isDir <- doesDirectoryExist path
  if isDir
    then formatDir path opts
    else formatSingleFile path opts

formatDir :: FilePath -> Cli.FormatOptions -> IO ()
formatDir dir opts = do
  files <- findNyFiles dir
  forM_ files $ \file -> formatSingleFile file opts

findNyFiles :: FilePath -> IO [FilePath]
findNyFiles path = do
    exists <- doesDirectoryExist path
    if not exists
    then return []
    else do
      entries <- listDirectory path
      let fullPaths = map (path </>) entries
      files <- filterM (\p -> (return (takeExtension p == ".ny")) &&> doesFileExist p) fullPaths
      dirs <- filterM doesDirectoryExist fullPaths
      nestedFiles <- concat <$> mapM findNyFiles dirs
      return $ files ++ nestedFiles

formatSingleFile :: FilePath -> Cli.FormatOptions -> IO ()
formatSingleFile file opts = do
  config <- FormatConfig.loadFormatConfig (Cli.formatConfig opts)
  content <- TIO.readFile file
  let formattedContent = Formatter.formatFile config content
  if Cli.formatCheck opts
    then if content == formattedContent
         then return ()
         else do
           putStrLn $ "File " ++ file ++ " is not formatted."
           exitFailure
    else if Cli.formatDiff opts
    then printDiff content formattedContent
    else TIO.writeFile file formattedContent

printDiff :: T.Text -> T.Text -> IO ()
printDiff original formatted =
  putStrLn "Diff is not implemented yet"

-- Helper for filtering
(&&>) :: Monad m => m Bool -> m Bool -> m Bool
(&&>) a b = do
  a' <- a
  if a' then b else return False