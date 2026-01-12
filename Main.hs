module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO (stderr)
import Control.Monad (filterM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Text.Megaparsec (parse, errorBundlePretty)

--- TODO Features [0/1]
-- [ ] Make it recursive

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      isDir <- doesDirectoryExist path
      isFile <- doesFileExist path
      if isDir
        then processDirectory path
        else if isFile
          then processFile path
          else do
            putStrLn $ "Error: Not a file or directory: " ++ path
            exitFailure
    _ -> do
      putStrLn "Nytrix recursive formatter"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  nyf <file>        Format <file>"
      putStrLn "  nyf <dir>         Recursively format all .ny files in <dir>"
      putStrLn ""
      exitFailure

processDirectory :: FilePath -> IO ()
processDirectory dir = do
  contents <- listDirectory dir
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (processFile . (dir </>)) nyFiles

  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (processDirectory . (dir </>)) subdirs

processFile :: FilePath -> IO ()
processFile fp = do
  input <- TIO.readFile fp
  case parse file fp input of
    Left err -> do
      TIO.hPutStrLn stderr (T.pack $ errorBundlePretty err)
      exitFailure
    Right _ast -> do
      putStrLn $ "Parsed successfully: " ++ fp
