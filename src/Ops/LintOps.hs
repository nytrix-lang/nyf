{-# LANGUAGE OverloadedStrings #-}
module Ops.LintOps where

import System.Directory (getDirectoryContents, doesDirectoryExist, getCurrentDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName, takeDirectory)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

--- TODO Stuff [0/2]
-- [ ] Gray gradient for the grids
-- [ ] Find the License interactively i think it's hardcoded to MIT

-- | Parse and check syntax
lintPath :: FilePath -> IO ()
lintPath _ = putStrLn "Error: LintOps.lintPath not implemented"

-- | Comprehensive validation (parse + lint)
validatePath :: FilePath -> IO ()
validatePath _ = putStrLn "Error: LintOps.validatePath not implemented"

-- Project configuration from package.yaml and format.yaml
data ProjectConfig = ProjectConfig
  { projectName :: String
  , projectVersion :: String
  , projectAuthor :: String
  , projectLicense :: String
  , projectGithub :: String
  , projectDescription :: String
  , formatIndentSize :: Int
  , formatUseTabs :: Bool
  , formatMaxLineLength :: Int
  } deriving (Show)

-- | Show code statistics for .ny files
statsPath :: FilePath -> IO ()
statsPath path = do
  -- Default to current directory if empty
  actualPath <- if null path then getCurrentDirectory else return path

  -- Find project root
  maybeProjectRoot <- findProjectRoot actualPath

  case maybeProjectRoot of
    Nothing -> do
      putStrLn ""
      putStrLn "\x1b[1;31mError:\x1b[0m Not in a Nytrix project"
      putStrLn ""
      putStrLn "Could not find \x1b[1mpackage.yaml\x1b[0m in current directory or any parent directory."
      putStrLn ""
      putStrLn "To create a new Nytrix project, run:"
      putStrLn "  \x1b[32mnyf new\x1b[0m \x1b[36m<project-name>\x1b[0m"
      putStrLn ""
    Just projectRoot -> do
      -- Read project configuration
      config <- readProjectConfig projectRoot

      -- Find all .ny files from the project root
      nyFiles <- findNyFiles projectRoot

      if null nyFiles
        then putStrLn $ "No .ny files found in project: " ++ projectRoot
        else do
          fileStats <- mapM analyzeFile nyFiles
          displayBeautifulStats projectRoot config fileStats

-- | Read project configuration from package.yaml and format.yaml
readProjectConfig :: FilePath -> IO ProjectConfig
readProjectConfig projectRoot = do
  -- Read package.yaml
  let packagePath = projectRoot </> "package.yaml"
  packageExists <- doesFileExist packagePath
  packageLines <- if packageExists
    then fmap T.lines (TIO.readFile packagePath)
    else return []

  -- Read format.yaml
  let formatPath = projectRoot </> "format.yaml"
  formatExists <- doesFileExist formatPath
  formatLines <- if formatExists
    then fmap T.lines (TIO.readFile formatPath)
    else return []

  let getName = extractYamlValue "name:" packageLines
      getVersion = extractYamlValue "version:" packageLines
      getAuthor = extractYamlValue "author:" packageLines
      getLicense = extractYamlValue "license:" packageLines
      getGithub = extractYamlValue "github:" packageLines
      getDesc = extractYamlValue "description:" packageLines

      getIndent = fromMaybe 4 (extractYamlInt "indentSize:" formatLines)
      getTabs = fromMaybe False (extractYamlBool "useTabs:" formatLines)
      getMaxLine = fromMaybe 80 (extractYamlInt "maxLineLength:" formatLines)

  return $ ProjectConfig getName getVersion getAuthor getLicense getGithub getDesc getIndent getTabs getMaxLine

-- | Extract value from YAML-like lines
extractYamlValue :: T.Text -> [T.Text] -> String
extractYamlValue key lines' =
  case filter (T.isPrefixOf key . T.strip) lines' of
    [] -> ""
    (line:_) -> T.unpack $ T.strip $ T.drop (T.length key) $ T.dropWhile (/= ':') line

extractYamlInt :: T.Text -> [T.Text] -> Maybe Int
extractYamlInt key lines' =
  case extractYamlValue key lines' of
    "" -> Nothing
    val -> readMaybe (filter (/= '"') val)

extractYamlBool :: T.Text -> [T.Text] -> Maybe Bool
extractYamlBool key lines' =
  case extractYamlValue key lines' of
    "" -> Nothing
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

-- | Find project root by looking for package.yaml
-- Returns Nothing if no project root is found
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot startPath = do
  isFile <- doesFileExist startPath
  let startDir = if isFile then takeDirectory startPath else startPath
  searchUpwards startDir startDir (0 :: Int)
  where
    searchUpwards originalDir dir depth
      | depth > 20 = return Nothing  -- Prevent infinite loops
      | otherwise = do
          let packageFile = dir </> "package.yaml"
          hasPackage <- doesFileExist packageFile
          if hasPackage
            then return (Just dir)
            else do
              let parent = takeDirectory dir
              if parent == dir  -- We've reached the filesystem root
                then return Nothing
                else searchUpwards originalDir parent (depth + 1)

-- Find all .ny files recursively
findNyFiles :: FilePath -> IO [FilePath]
findNyFiles dir = do
  contents <- getDirectoryContents dir
  let validEntries = filter (`notElem` [".", ".."]) contents
  results <- mapM processEntry validEntries
  return (concat results)
  where
    processEntry entry = do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then findNyFiles fullPath
        else if takeExtension entry == ".ny"
          then return [fullPath]
          else return []

-- Analyze a single file
analyzeFile :: FilePath -> IO FileStats
analyzeFile path = do
  content <- TIO.readFile path
  let linesList = T.lines content
      totalLinesCount = length linesList
      nonEmptyLines = filter (not . T.null . T.strip) linesList
      codeLinesCount = length $ filter (not . isComment) nonEmptyLines
      commentLinesCount = length nonEmptyLines - codeLinesCount
      blankLinesCount = totalLinesCount - length nonEmptyLines

  return $ FileStats
    { fileName = path
    , totalLines = totalLinesCount
    , codeLines = codeLinesCount
    , commentLines = commentLinesCount
    , blankLines = blankLinesCount
    }

data FileStats = FileStats
  { fileName :: FilePath
  , totalLines :: Int
  , codeLines :: Int
  , commentLines :: Int
  , blankLines :: Int
  }

-- Check if line is a comment (starts with -- or ; or ;;;)
isComment :: T.Text -> Bool
isComment line =
  let trimmed = T.dropWhile isSpace line
  in T.isPrefixOf "--" trimmed ||
     T.isPrefixOf ";" trimmed ||
     T.isPrefixOf ";;;" trimmed

-- | Display beautiful project statistics
displayBeautifulStats :: FilePath -> ProjectConfig -> [FileStats] -> IO ()
displayBeautifulStats projectRoot config fileStats = do
  let totalFiles = length fileStats
      totalLinesSum = sum $ map totalLines fileStats
      totalCodeSum = sum $ map codeLines fileStats
      totalCommentsSum = sum $ map commentLines fileStats
      totalBlanksSum = sum $ map blankLines fileStats

      codePct = percentage totalCodeSum totalLinesSum
      commentPct = percentage totalCommentsSum totalLinesSum
      blankPct = percentage totalBlanksSum totalLinesSum

      avgLinesPerFile = if totalFiles > 0 then totalLinesSum `div` totalFiles else 0

      projectNameStr = if null (projectName config) then takeFileName projectRoot else projectName config

  putStrLn ""

  -- Project header with tree-style metadata
  putStrLn $ "\x1b[1;94m" ++ projectNameStr ++ "\x1b[0m \x1b[2m" ++ projectVersion config ++ "\x1b[0m"

  let author = if null (projectAuthor config) then "\x1b[2mnot set\x1b[0m" else removeQuotes (projectAuthor config)
      license = if null (projectLicense config) then "\x1b[2mnot set\x1b[0m" else projectLicense config
      repo = if null (projectGithub config) then "\x1b[2mnot set\x1b[0m" else "https://github.com/" ++ removeQuotes (projectGithub config)
      desc = if null (projectDescription config) then "\x1b[2mnot set\x1b[0m" else projectDescription config

  putStrLn $ "\x1b[90m├─\x1b[0m \x1b[92mAuthor:\x1b[0m      " ++ author
  putStrLn $ "\x1b[90m├─\x1b[0m \x1b[95mLicense:\x1b[0m     " ++ license
  putStrLn $ "\x1b[90m├─\x1b[0m \x1b[95mRepository:\x1b[0m  \x1b[94m" ++ repo ++ "\x1b[0m"
  putStrLn $ "\x1b[90m└─\x1b[0m \x1b[95mDescription:\x1b[0m " ++ desc

  putStrLn ""

  -- Stats overview - centered header with clean vertical divider
  putStrLn "               \x1b[1;94mProject has\x1b[0m \x1b[2m"
  putStrLn "─────────────────────┬────────────────────"
  putStrLn $ " Files               │ " ++ padLeft 18 (show totalFiles)
  putStrLn $ " Lines               │ " ++ padLeft 18 (show totalLinesSum)
  putStrLn $ " Average per file    │ " ++ padLeft 18 (show avgLinesPerFile)
  putStrLn "─────────────────────┼───────────────────"
  putStrLn $ " \x1b[94mCode\x1b[0m                │ " ++ padLeft 12 (show totalCodeSum) ++ " \x1b[2m(" ++ printfPct codePct ++ ")\x1b[0m"
  putStrLn $ " \x1b[92mComments\x1b[0m            │ " ++ padLeft 12 (show totalCommentsSum) ++ " \x1b[2m(" ++ printfPct commentPct ++ ")\x1b[0m"
  putStrLn $ " \x1b[90mBlanks\x1b[0m              │ " ++ padLeft 12 (show totalBlanksSum) ++ " \x1b[2m(" ++ printfPct blankPct ++ ")\x1b[0m"

  putStrLn ""

  -- Language breakdown bar with semicircle edges
  let barWidth = 40
      codeBar = round (codePct * fromIntegral barWidth / 100)
      commentBar = round (commentPct * fromIntegral barWidth / 100)
      blankBar = barWidth - codeBar - commentBar

  putStr "\x1b[94m◖\x1b[0m"
  putStr $ "\x1b[44m" ++ replicate codeBar ' ' ++ "\x1b[0m"        -- Blue for code
  putStr $ "\x1b[42m" ++ replicate commentBar ' ' ++ "\x1b[0m"     -- Green for comments
  putStr $ "\x1b[47m" ++ replicate blankBar ' ' ++ "\x1b[0m"       -- Light gray for blanks
  putStrLn "◗"

  putStrLn $ " \x1b[94m●\x1b[0m Code \x1b[2m" ++ printfPct codePct ++ "\x1b[0m" ++
             "   \x1b[92m●\x1b[0m Comments \x1b[2m" ++ printfPct commentPct ++ "\x1b[0m" ++
             "   \x1b[90m●\x1b[0m Blanks \x1b[2m" ++ printfPct blankPct ++ "\x1b[0m"

  putStrLn ""

  -- Format configuration - centered header with vertical divider
  putStrLn "                  \x1b[1mFormat\x1b[0m"
  putStrLn $ " \x1b[93mIndentation\x1b[0m         ┬ " ++ padLeft 18 (show (formatIndentSize config) ++ " " ++ if formatUseTabs config then "tabs" else "spaces")
  putStrLn $ " \x1b[93mMax line length\x1b[0m     │ " ++ padLeft 18 (show (formatMaxLineLength config) ++ " chars")
  putStrLn "─────────────────────┴───────────────────"

  putStrLn ""

  -- Top files - centered header with vertical divider
  unless (null fileStats) $ do
    let topFiles = take 5 $ reverse $ sortOn totalLines fileStats
        makeRelative path =
          if take (length projectRoot) path == projectRoot
            then drop (length projectRoot + 1) path
            else path

    putStrLn " \x1b[1;94mLargest Files\x1b[0m"
    putStrLn "  Lines ┬ \x1b[92mFile:\x1b[0m"
    mapM_ (\fs ->
      let relPath = makeRelative (fileName fs)
          lineStr = show (totalLines fs)
          pathColor = if totalLines fs > 100 then "\x1b[93m" else "\x1b[0m"  -- Yellow if > 100 lines
      in putStrLn $ " " ++ padLeft 6 lineStr ++ " │ " ++ pathColor ++ relPath ++ "\x1b[0m") topFiles

    putStrLn ""


-- Remove quotes from strings
removeQuotes :: String -> String
removeQuotes = filter (/= '"')

-- Helper functions
padRight :: Int -> String -> String
padRight width str = take width (str ++ repeat ' ')

padLeft :: Int -> String -> String
padLeft width str = replicate (width - length str) ' ' ++ str

percentage :: Int -> Int -> Double
percentage part total = if total == 0 then 0 else fromIntegral part / fromIntegral total * 100

printfPct :: Double -> String
printfPct pct = show (round pct :: Int) ++ "%"

-- Conditional execution
unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action
