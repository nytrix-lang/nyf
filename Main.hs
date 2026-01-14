{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      listDirectory,
      createDirectory,
      getModificationTime,
      getCurrentDirectory,
      setCurrentDirectory,
      removeFile           )
import System.FilePath ( (</>), takeExtension, takeDirectory, splitDirectories )
import System.IO (stderr)
import System.Process ( rawSystem, readProcess )
import Control.Monad (filterM, when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_nyf (getDataDir)
import Control.Exception (catch, SomeException)
import Data.List ( isInfixOf, dropWhileEnd, isSuffixOf, intercalate )
import Data.Char (isSpace)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

import Parser (file)
import Formatter (format)
import FormatConfig (loadFormatConfig, findFormatYaml)
import Templates
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Test

--- ANSI COLOR CODES

white :: String
white = "\ESC[97m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

gray :: String
gray = "\ESC[90m"

reset :: String
reset = "\ESC[0m"

bold :: String
bold = "\ESC[1m"


--- MAIN ENTRY POINT

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] ->
      runTests

    ["new", projectName] ->
      createNewProject projectName

    ["run"] ->
      runProject

    ["format", path] ->
      formatPath path

    ["check", path] ->
      checkPath path

    ["ast", "delete", path] ->
      astDeletePath path

    ["ast", path] ->
      astPath path

    [path] ->
      checkPath path

    _ ->
      printUsage

--- TEST RUNNER

runTests :: IO ()
runTests = do
  -- First try local tests/ directory (for development)
  localTestsExist <- doesDirectoryExist "tests"

  if localTestsExist
    then do
      putStrLn "Running tests from local tests/ directory..."
      Test.runAllTests
    else do
      -- Fall back to installed data files
      dataDir <- getDataDir
      let testsDir = dataDir </> "tests"
      testsExist <- doesDirectoryExist testsDir

      if testsExist
        then do
          putStrLn $ "Running tests from " ++ testsDir ++ "..."
          -- Change to the data directory so Test module can find files
          currentDir <- getCurrentDirectory
          setCurrentDirectory dataDir
          Test.runAllTests
          setCurrentDirectory currentDir
        else do
          putStrLn "Error: No tests found"
          putStrLn "  - Local tests/ directory not found"
          putStrLn $ "  - Installed tests not found at: " ++ testsDir
          putStrLn ""
          putStrLn "If you're developing nyf, run 'nyf test' from the project root."
          putStrLn "If you installed nyf, the test files may not have been installed."
          exitFailure

--- PROJECT SCAFFOLDING

createNewProject :: String -> IO ()
createNewProject projectName = do
  -- Validate project name
  when (null projectName) $ do
    putStrLn "Error: Project name cannot be empty"
    exitFailure

  when (elem '/' projectName || elem '\\' projectName) $ do
    putStrLn "Error: Project name cannot contain path separators"
    exitFailure

  -- Check if directory already exists
  exists <- doesDirectoryExist projectName
  when exists $ do
    putStrLn $ "Error: Directory '" ++ projectName ++ "' already exists"
    exitFailure

  putStrLn $ "Creating new Nytrix project: " ++ projectName
  putStrLn ""

  -- Get git info
  mGitUser <- getGitRemote
  mUserName <- getGitUserName
  mUserEmail <- getGitUserEmail

  let gitUser = maybe "username" id mGitUser
      userName = maybe "Your Name" id mUserName
      userEmail = maybe "your.email@example.com" id mUserEmail

  -- Create directory structure
  createDirectory projectName
  createDirectory (projectName </> "src")
  createDirectory (projectName </> "build")
  putStrLn $ "✓ Created directory: " ++ projectName
  putStrLn $ "✓ Created directory: " ++ projectName </> "src"
  putStrLn $ "✓ Created directory: " ++ projectName </> "build"

  -- Create main.ny
  let mainPath = projectName </> "src" </> "main.ny"
  TIO.writeFile mainPath (mainTemplate projectName)
  putStrLn $ "✓ Created file: " ++ mainPath

  -- Create format.yaml
  let configPath = projectName </> "format.yaml"
  TIO.writeFile configPath formatYamlTemplate
  putStrLn $ "✓ Created file: " ++ configPath

  -- Create package.yaml with git info
  let packagePath = projectName </> "package.yaml"
  TIO.writeFile packagePath (packageYamlTemplate projectName gitUser userName userEmail)
  putStrLn $ "✓ Created file: " ++ packagePath

  -- Create README.md
  let readmePath = projectName </> "README.md"
  TIO.writeFile readmePath (readmeTemplate projectName)
  putStrLn $ "✓ Created file: " ++ readmePath

  -- Create .gitignore
  let gitignorePath = projectName </> ".gitignore"
  TIO.writeFile gitignorePath gitignoreTemplate
  putStrLn $ "✓ Created file: " ++ gitignorePath

  putStrLn ""
  putStrLn $ green ++ "Project created successfully!" ++ reset
  putStrLn ""
  putStrLn "Next steps:"
  putStrLn $ "  cd " ++ projectName
  putStrLn   "  nyf run              # Compile and run"
  putStrLn   "  nyf format src/      # Format your code"
  putStrLn ""

--- RUN PROJECT

-- | Find main.ny and compile/run it with nytrix
runProject :: IO ()
runProject = do
  -- Look for src/main.ny
  let mainPath = "src" </> "main.ny"
  mainExists <- doesFileExist mainPath

  unless mainExists $ do
    putStrLn "Error: src/main.ny not found"
    putStrLn "Run this command from your project root directory"
    exitFailure

  -- Check if already compiled
  let buildDir = "build"
  let executable = buildDir </> "main"

  buildExists <- doesDirectoryExist buildDir
  unless buildExists $ createDirectory buildDir

  exeExists <- doesFileExist executable

  needsCompile <- if exeExists
    then do
      mainModTime <- getModificationTime mainPath
      exeModTime <- getModificationTime executable
      return $ mainModTime > exeModTime
    else return True

  when needsCompile $ do
    putStrLn "Compiling src/main.ny..."
    -- Use nytrix to compile: nytrix -o build/main src/main.ny
    exitCode <- rawSystem "nytrix" ["-o", executable, mainPath]
    case exitCode of
      ExitSuccess -> putStrLn $ green ++ "✓ Compiled successfully" ++ reset
      ExitFailure _ -> do
        putStrLn "Compilation failed"
        exitFailure

  -- Run the executable
  putStrLn "Running..."
  putStrLn ""
  exitCode <- rawSystem executable []
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> exitWith (ExitFailure code)

--- User Git Informations

-- Get git user name
getGitUserName :: IO (Maybe String)
getGitUserName = catch
  (Just . trim <$> readProcess "git" ["config", "user.name"] "")
  (\(_::SomeException) -> return Nothing)

-- Get git user email
getGitUserEmail :: IO (Maybe String)
getGitUserEmail = catch
  (Just . trim <$> readProcess "git" ["config", "user.email"] "")
  (\(_::SomeException) -> return Nothing)

-- Get git remote origin
getGitRemote :: IO (Maybe String)
getGitRemote = catch
  (Just . extractGithubUsername . trim <$> readProcess "git" ["config", "remote.origin.url"] "")
  (\(_::SomeException) -> return Nothing)

-- Extract username from git URL
extractGithubUsername :: String -> String
extractGithubUsername url
  | "github.com:" `isInfixOf` url =
      let afterColon = drop 1 $ dropWhile (/= ':') url
          username = takeWhile (/= '/') afterColon
      in username
  | "github.com/" `isInfixOf` url =
      let gitStr = "github.com/" :: String
          afterGithub = drop (length gitStr) $ dropWhile (/= 'm') url
          username = takeWhile (/= '/') afterGithub
      in if null username then "username" else username
  | otherwise = "username"
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

--- COMMAND IMPLEMENTATIONS

-- | Format a file or directory (writes changes to disk)
formatPath :: FilePath -> IO ()
formatPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then formatDirectoryRecursive path
    else if isFile
      then formatSingleFile path
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

-- | Check/parse a file or directory (no changes to disk)
checkPath :: FilePath -> IO ()
checkPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then checkDirectoryRecursive path
    else if isFile
      then checkSingleFile path
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

-- | Generate AST JSON for a file or directory
astPath :: FilePath -> IO ()
astPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then do
      count <- astDirectoryRecursive path
      putStrLn ""
      putStrLn $ green ++ "Generated " ++ show count ++ " AST JSON file(s)" ++ reset
    else if isFile
      then do
        astSingleFile path
        putStrLn ""
        putStrLn $ green ++ "Generated 1 AST JSON file" ++ reset
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

astDirectoryRecursive :: FilePath -> IO Int
astDirectoryRecursive dir = do
  putStrLn $ "Generating AST JSON for directory: " ++ dir
  contents <- listDirectory dir

  -- Generate AST for all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (astSingleFile . (dir </>)) nyFiles
  let currentCount = length nyFiles

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  subcounts <- mapM (astDirectoryRecursive . (dir </>)) subdirs

  return $ currentCount + sum subcounts

-- | Recursively generate AST JSON for all .ny files in a directory
astDeleteDirectoryRecursive :: FilePath -> IO Int
astDeleteDirectoryRecursive dir = do
  putStrLn $ "Deleting AST JSON for directory: " ++ dir
  contents <- listDirectory dir

  -- Delete AST for all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  results <- mapM (astDeleteSingleFile . (dir </>)) nyFiles
  let currentCount = length $ filter id results

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  subcounts <- mapM (astDeleteDirectoryRecursive . (dir </>)) subdirs

  return $ currentCount + sum subcounts

-- | Generate AST JSON for a single file
astSingleFile :: FilePath -> IO ()
astSingleFile filepath = do
  putStrLn $ "Generating AST: " ++ formatFilePath filepath

  -- Read input
  input <- TIO.readFile filepath

  -- Parse
  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ "Parse error in " ++ filepath ++ ":"
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right ast -> do
      -- Generate JSON output path: replace .ny with .json
      let jsonPath = take (length filepath - 3) filepath ++ ".json"

      -- Encode AST to JSON
      let jsonData = Aeson.encode ast

      -- Write JSON file
      BL.writeFile jsonPath jsonData
      putStrLn $ green ++ "✓" ++ reset ++ " Generated AST JSON: " ++ formatFilePathJSON jsonPath

astDeleteSingleFile :: FilePath -> IO Bool
astDeleteSingleFile filepath = do
  -- Check if the .ny file exists
  let nyPath = if ".ny" `isSuffixOf` filepath
                 then filepath
                 else filepath ++ ".ny"

  nyExists <- doesFileExist nyPath

  if not nyExists
    then do
      putStrLn $ "Warning: Skipping - no corresponding .ny file found for: " ++ filepath
      return False
    else do
      -- Generate JSON path: replace .ny with .json or add .json
      let jsonPath = if ".ny" `isSuffixOf` filepath
                       then take (length filepath - 3) filepath ++ ".json"
                       else filepath ++ ".json"

      jsonExists <- doesFileExist jsonPath

      if jsonExists
        then do
          removeFile jsonPath
          putStrLn $ green ++ "✓" ++ reset ++ " Deleted AST JSON: " ++ formatFilePathJSON jsonPath
          return True
        else do
          putStrLn $ "Note: JSON file does not exist: " ++ jsonPath
          return False

-- | Delete AST JSON files for a file or directory
astDeletePath :: FilePath -> IO ()
astDeletePath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then do
      count <- astDeleteDirectoryRecursive path
      putStrLn ""
      putStrLn $ green ++ "Deleted " ++ show count ++ " AST JSON file(s)" ++ reset
    else if isFile
      then do
        deleted <- astDeleteSingleFile path
        putStrLn ""
        if deleted
          then putStrLn $ green ++ "Deleted 1 AST JSON file" ++ reset
          else putStrLn "No AST JSON file to delete"
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

--- FORMAT OPERATIONS (write to disk)

-- | Recursively format all .ny files in a directory
formatDirectoryRecursive :: FilePath -> IO ()
formatDirectoryRecursive dir = do
  putStrLn $ "Formatting directory: " ++ dir
  contents <- listDirectory dir

  -- Format all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (formatSingleFile . (dir </>)) nyFiles

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (formatDirectoryRecursive . (dir </>)) subdirs

-- | Format a single file in place
formatSingleFile :: FilePath -> IO ()
formatSingleFile filepath = do
  putStrLn $ "Formatting: " ++ formatFilePath filepath

  -- Read input
  input <- TIO.readFile filepath

  -- Load config from file's directory (searches upward)
  let dir = takeDirectory filepath
  mConfigPath <- findFormatYaml dir
  cfg <- loadFormatConfig mConfigPath

  -- Parse
  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ "Parse error in " ++ filepath ++ ":"
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right ast -> do
      -- Format
      let formatted = format cfg ast

      -- Write back
      TIO.writeFile filepath formatted
      putStrLn $ green ++ "✓" ++ reset ++ " Formatted: " ++ formatFilePath filepath

-- Helper function to format file paths with colors
formatFilePath :: FilePath -> String
formatFilePath path =
  let -- Remove .ny extension
      withoutExt = if ".ny" `isSuffixOf` path then take (length path - 3) path else path
      -- Split by directory separator and convert to dot notation
      parts = splitPath withoutExt
      -- Join with dots
      dottedPath = intercalate "." parts
      -- Split by dots to colorize
      segments = splitOn' '.' dottedPath
  in colorizeSegments segments
  where
    -- Split path into components and clean them up
    splitPath :: FilePath -> [String]
    splitPath p = filter (not . null) $ map cleanSep $ splitDirectories p
      where
        cleanSep = filter (\c -> c /= '/' && c /= '\\')

    -- Split string by character
    splitOn' :: Char -> String -> [String]
    splitOn' _ [] = []
    splitOn' delim str = case break (== delim) str of
      (part, []) -> [part]
      (part, _:rest) -> part : splitOn' delim rest

    -- Colorize the segments
    colorizeSegments :: [String] -> String
    colorizeSegments [] = ""
    colorizeSegments [x] = white ++ x ++ reset  -- Last segment is white
    colorizeSegments (x:xs) =
      yellow ++ x ++ reset ++ gray ++ "." ++ reset ++ colorizeSegments xs

formatFilePathJSON :: FilePath -> String
formatFilePathJSON path =
  let -- Remove .json extension
      withoutExt = if ".json" `isSuffixOf` path then take (length path - 5) path else path
      -- Split by directory separator and convert to dot notation
      parts = splitPath withoutExt
      -- Join with dots
      dottedPath = intercalate "." parts
      -- Split by dots to colorize
      segments = splitOn' '.' dottedPath
  in colorizeSegments segments
  where
    -- Split path into components and clean them up
    splitPath :: FilePath -> [String]
    splitPath p = filter (not . null) $ map cleanSep $ splitDirectories p
      where
        cleanSep = filter (\c -> c /= '/' && c /= '\\')

    -- Split string by character
    splitOn' :: Char -> String -> [String]
    splitOn' _ [] = []
    splitOn' delim str = case break (== delim) str of
      (part, []) -> [part]
      (part, _:rest) -> part : splitOn' delim rest

    -- Colorize the segments
    colorizeSegments :: [String] -> String
    colorizeSegments [] = ""
    colorizeSegments [x] = white ++ x ++ reset  -- Last segment is white
    colorizeSegments (x:xs) =
      yellow ++ x ++ reset ++ gray ++ "." ++ reset ++ colorizeSegments xs

--- CHECK OPERATIONS (read-only, no changes to disk)

-- | Recursively check all .ny files in a directory
checkDirectoryRecursive :: FilePath -> IO ()
checkDirectoryRecursive dir = do
  putStrLn $ "Checking directory: " ++ dir
  contents <- listDirectory dir

  -- Check all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (checkSingleFile . (dir </>)) nyFiles

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (checkDirectoryRecursive . (dir </>)) subdirs

-- | Check/parse a single file (no formatting)
checkSingleFile :: FilePath -> IO ()
checkSingleFile filepath = do
  input <- TIO.readFile filepath

  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ "Parse error in " ++ filepath ++ ":"
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right _ast -> do
      putStrLn $ green ++ "✓" ++ reset ++ " Parsed successfully: " ++ filepath

--- HELP

printUsage :: IO ()
printUsage = do
  putStrLn "Nytrix Formal Functional Formatter (nyf)"
  putStrLn ""
  putStrLn "A highly configurable code and project formatter for Nytrix."
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "  nyf new <name>          Create a new Nytrix project"
  putStrLn "  nyf run                 Compile and run src/main.ny"
  putStrLn "  nyf <file>              Parse and check <file>"
  putStrLn "  nyf <dir>               Parse and check all .ny files in <dir> recursively"
  putStrLn "  nyf check <path>        Same as above (explicit check command)"
  putStrLn "  nyf format <file>       Format <file> in place"
  putStrLn "  nyf format <dir>        Recursively format all .ny files in <dir>"
  putStrLn "  nyf ast <file>          Generate AST JSON for <file> (creates <file>.json)"
  putStrLn "  nyf ast <dir>           Recursively generate AST JSON for all .ny files in <dir>"
  putStrLn "  nyf ast delete <file>   Delete AST JSON for <file> (only if .ny exists)"
  putStrLn "  nyf ast delete <dir>    Recursively delete AST JSON for all .ny files in <dir>"
  putStrLn "  nyf test                Run all tests in tests/ directory"
  putStrLn ""
  exitFailure
