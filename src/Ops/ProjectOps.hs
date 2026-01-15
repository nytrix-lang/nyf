{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ops.ProjectOps where

import System.Directory (createDirectoryIfMissing, getCurrentDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Exit (ExitCode(..))
import Control.Exception (catch, SomeException)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Templates

-- | Get git config value, return empty string if not found
getGitConfig :: String -> IO String
getGitConfig key = do
  result <- catch
    (readProcess "git" ["config", "--get", key] "")
    (\(_ :: SomeException) -> return "")
  return $ filter (/= '\n') result

createNewProject :: String -> IO ()
createNewProject projectName = do
  currentDir <- getCurrentDirectory
  let projectDir = currentDir </> projectName

  -- Get git user information
  gitUser <- getGitConfig "user.name"
  userEmail <- getGitConfig "user.email"

  -- Create basic project structure
  createDirectoryIfMissing True projectDir
  createDirectoryIfMissing True (projectDir </> "src")

  -- Write package.yaml with git info
  TIO.writeFile (projectDir </> "package.yaml")
    (Templates.packageYamlTemplate projectName gitUser gitUser userEmail)

  -- Write format.yaml
  TIO.writeFile (projectDir </> "format.yaml")
    Templates.formatYamlTemplate

  -- Write main.ny with proper template
  TIO.writeFile (projectDir </> "src" </> "main.ny")
    (Templates.mainTemplate projectName)

  -- Write README.md
  TIO.writeFile (projectDir </> "README.md")
    (Templates.readmeTemplate projectName)

  -- Write .gitignore
  TIO.writeFile (projectDir </> ".gitignore")
    Templates.gitignoreTemplate

  -- Print success message with color
  putStrLn ""
  putStrLn $ "\x1b[32m     Creating\x1b[0m binary (application) `" ++ projectName ++ "` package"
  putStrLn $ "\x1b[33m        note:\x1b[0m see project structure and configuration in package.yaml and format.yaml"
  putStrLn ""

-- Keep other stubs for now
initProject :: IO ()
initProject = putStrLn "Error: ProjectOps.initProject not implemented"

runProject :: Maybe String -> IO ()
runProject _ = putStrLn "Error: ProjectOps.runProject not implemented"

buildProject :: IO ()
buildProject = putStrLn "Error: ProjectOps.buildProject not implemented"

cleanProject :: IO ()
cleanProject = putStrLn "Error: ProjectOps.cleanProject not implemented"

watchProject :: IO ()
watchProject = putStrLn "Error: ProjectOps.watchProject not implemented"
