{-# LANGUAGE OverloadedStrings #-}
module HelpSystem
  ( printUsage
  , printVersion
  , printCommandHelp
  ) where

import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Version (showVersion)
import Paths_nyf (version)
import Control.Monad (unless)

import Cli
import qualified Colors as C

-- | Print version information
printVersion :: IO ()
printVersion = do
  putStrLn $ C.bold ++ "nyf" ++ C.reset ++ " version " ++ showVersion version
  putStrLn "Nytrix Formal Functional Formatter & Build Tool"
  putStrLn ""
  putStrLn $ C.dim ++ "Copyright (c) 2026 Nytrix Project" ++ C.reset
  putStrLn $ C.dim ++ "https://github.com/nytrix-lang/nyf  " ++ C.reset

-- | Print general usage and command overview
printUsage :: IO ()
printUsage = do
  putStrLn $ C.bold ++ "nyf" ++ C.reset ++ " - Nytrix Formal Functional Formatter"
  putStrLn ""
  putStrLn $ C.dim ++ "A highly configurable code formatter and build tool for Nytrix" ++ C.reset
  putStrLn ""
  putStrLn $ C.bold ++ "USAGE:" ++ C.reset
  putStrLn "  nyf <command> [options]"
  putStrLn ""

  -- Group commands by category
  let grouped = groupBy ((==) `on` cmdCategory) $ sortBy (compare `on` cmdCategory) commands

  mapM_ printCategory grouped

  putStrLn $ C.bold ++ "EXAMPLES:" ++ C.reset
  putStrLn $ "  nyf new my-project       " ++ C.dim ++ "# Create new project" ++ C.reset
  putStrLn $ "  nyf run                  " ++ C.dim ++ "# Build and run main.ny" ++ C.reset
  putStrLn $ "  nyf format src/          " ++ C.dim ++ "# Format all files in src/" ++ C.reset
  putStrLn $ "  nyf lint .               " ++ C.dim ++ "# Check all .ny files" ++ C.reset
  putStrLn $ "  nyf test                 " ++ C.dim ++ "# Run test suite" ++ C.reset
  putStrLn ""
  putStrLn $ "For more information: " ++ C.cyan ++ "nyf help <command>" ++ C.reset

-- | Print a category of commands
printCategory :: [CommandInfo] -> IO ()
printCategory [] = return ()
printCategory cmds@(firstCmd:_) = do
  putStrLn $ C.bold ++ cmdCategory firstCmd ++ " COMMANDS:" ++ C.reset
  mapM_ printCommandSummary cmds
  putStrLn ""

-- | Print a single command summary line
printCommandSummary :: CommandInfo -> IO ()
printCommandSummary cmd =
  let name = cmdName cmd
      padding = replicate (20 - length name) ' '
      desc = cmdShortDesc cmd
  in putStrLn $ "  " ++ C.cyan ++ name ++ C.reset ++ padding ++ desc

-- | Print detailed help for a specific command
printCommandHelp :: String -> IO ()
printCommandHelp cmdNameArg =
  case getCommandInfo cmdNameArg of
    Nothing -> do
      putStrLn $ C.red ++ "Error: Unknown command '" ++ cmdNameArg ++ "'" ++ C.reset
      putStrLn ""
      putStrLn "Run 'nyf help' to see available commands."
    Just cmd -> printDetailedHelp cmd

-- | Print detailed help for a command
printDetailedHelp :: CommandInfo -> IO ()
printDetailedHelp cmd = do
  putStrLn $ C.bold ++ "nyf " ++ cmdName cmd ++ C.reset ++ " - " ++ cmdShortDesc cmd
  putStrLn ""

  putStrLn $ C.bold ++ "DESCRIPTION:" ++ C.reset
  putStrLn $ "  " ++ cmdLongDesc cmd
  putStrLn ""

  putStrLn $ C.bold ++ "USAGE:" ++ C.reset
  mapM_ (\usage -> putStrLn $ "  " ++ C.cyan ++ usage ++ C.reset) (cmdUsage cmd)
  putStrLn ""

  unless (null $ cmdOptions cmd) $ do
    putStrLn $ C.bold ++ "OPTIONS:" ++ C.reset
    mapM_ printOption (cmdOptions cmd)
    putStrLn ""

  unless (null $ cmdExamples cmd) $ do
    putStrLn $ C.bold ++ "EXAMPLES:" ++ C.reset
    mapM_ printExample (cmdExamples cmd)
    putStrLn ""

  putStrLn $ C.dim ++ "For general help: nyf help" ++ C.reset

-- | Print an option description
printOption :: (String, String) -> IO ()
printOption (opt, desc) =
  let padding = replicate (max 0 (25 - length opt)) ' '
  in putStrLn $ "  " ++ C.yellow ++ opt ++ C.reset ++ padding ++ desc

-- | Print an example with description
printExample :: (String, String) -> IO ()
printExample (example, desc) = do
  putStrLn $ "  " ++ C.cyan ++ example ++ C.reset
  putStrLn $ "    " ++ C.dim ++ desc ++ C.reset
