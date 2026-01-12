module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    _ -> do
      putStrLn "Nytrix recursive formatter"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  nyf <file>        Format <file>"
      putStrLn "  nyf <dir>         Recursively format all .ny files in <dir>"
      putStrLn ""
      exitFailure
