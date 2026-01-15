{-# LANGUAGE OverloadedStrings #-}
module Ops.AstOps where

import qualified Cli
import Parser (file)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (encode)
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist, removeFile)
import System.FilePath ((</>), takeExtension, replaceExtension)
import Control.Monad (forM_, when, filterM)
import Text.Megaparsec (parse, errorBundlePretty)
import AST()

astPath :: FilePath -> Cli.AstOptions -> IO ()
astPath path opts = do
  isDir <- doesDirectoryExist path
  if isDir
    then processDir path opts
    else processFile path opts

processDir :: FilePath -> Cli.AstOptions -> IO ()
processDir dir opts = do
  files <- findNyFiles dir
  forM_ files $ \file -> processFile file opts

findNyFiles :: FilePath -> IO [FilePath]
findNyFiles path = do
    exists <- doesDirectoryExist path
    if not exists
    then return []
    else do
      entries <- listDirectory path
      let fullPaths = map (path </>) entries
      files <- filterM (\p -> return (takeExtension p == ".ny") &&> doesFileExist p) fullPaths
      dirs <- filterM doesDirectoryExist fullPaths
      nestedFiles <- concat <$> mapM findNyFiles dirs
      return $ files ++ nestedFiles

processFile :: FilePath -> Cli.AstOptions -> IO ()
processFile file opts
  | Cli.astDelete opts = deleteAst file
  | otherwise          = generateAst file opts

generateAst :: FilePath -> Cli.AstOptions -> IO ()
generateAst file opts = do
  content <- TIO.readFile file
  case parse Parser.file file content of
    Left err -> putStrLn $ "Parse error in " ++ file ++ ":\n" ++ errorBundlePretty err
    Right ast -> do
      let output = if Cli.astPretty opts then encodePretty ast else encode ast
          jsonFile = replaceExtension file ".json"
      BSL.writeFile jsonFile output
      putStrLn $ "Generated AST for " ++ file ++ " -> " ++ jsonFile

deleteAst :: FilePath -> IO ()
deleteAst file = do
  let jsonFile = replaceExtension file ".json"
  exists <- doesFileExist jsonFile
  when exists $ do
    removeFile jsonFile
    putStrLn $ "Deleted " ++ jsonFile

depsFile :: FilePath -> IO ()
depsFile _ = putStrLn "Error: AstOps.depsFile not implemented"

-- Helper for filtering
(&&>) :: Monad m => m Bool -> m Bool -> m Bool
(&&>) a b = do
  a' <- a
  if a' then b else return False
