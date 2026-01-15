{-# LANGUAGE OverloadedStrings #-}
module Docs
  ( DocAnnotation(..)
  , DocBlock(..)
  , extractDocs
  , generateDocs
  , parseDocAnnotations
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import AST

-- | Documentation annotation types (like Doxygen)
data DocAnnotation
  = FunAnnotation Text         -- @fun function_name
  | ParamAnnotation Text Text  -- @param name description
  | ReturnAnnotation Text      -- @return description
  | ExampleAnnotation Text     -- @example code
  | DescAnnotation Text        -- @desc description
  | AuthorAnnotation Text      -- @author name
  | VersionAnnotation Text     -- @version x.y.z
  | SinceAnnotation Text       -- @since version
  | DeprecatedAnnotation Text  -- @deprecated reason
  | SeeAnnotation Text         -- @see reference
  | ThrowsAnnotation Text      -- @throws exception
  | TodoAnnotation Text        -- @todo task
  deriving (Show, Eq)

-- | A documentation block with its annotations
data DocBlock = DocBlock
  { docSummary :: Text
  , docAnnotations :: [DocAnnotation]
  , docLocation :: (Int, Int)  -- (line, column)
  } deriving (Show, Eq)

-- | Extract documentation from comments
extractDocs :: [Decl] -> [(Text, DocBlock)]
extractDocs decls = concatMap extractFromDecl decls
  where
    extractFromDecl :: Decl -> [(Text, DocBlock)]
    extractFromDecl (FnDecl name _ _ (Just docStr) _) =
      case parseDocString docStr of
        Just docBlock -> [(name, docBlock)]
        Nothing -> []
    extractFromDecl (CommentDecl comment) =
      case extractDocFromComment comment of
        Just (name, docBlock) -> [(name, docBlock)]
        Nothing -> []
    extractFromDecl _ = []

-- Helper: extract function name from annotations
findFunName :: [DocAnnotation] -> Maybe Text
findFunName [] = Nothing
findFunName (FunAnnotation name : _) = Just name
findFunName (_ : rest) = findFunName rest

-- | Extract documentation from a comment
extractDocFromComment :: Comment -> Maybe (Text, DocBlock)
extractDocFromComment (BlockComment content _) =
  case parseDocString content of
    Just docBlock ->
      case findFunName (docAnnotations docBlock) of
        Just name -> Just (name, docBlock)
        Nothing   -> Nothing  -- or decide: should we allow nameless docs?
    Nothing -> Nothing
extractDocFromComment _ = Nothing

-- | Parse a docstring into a DocBlock
parseDocString :: Text -> Maybe DocBlock
parseDocString text =
  let lines' = T.lines text
      (summary, annotLines) = span (not . isAnnotationLine) lines'
      annotations = map parseAnnotationLine $ filter isAnnotationLine annotLines
  in if T.null (T.strip $ T.unlines summary)
       then Nothing
       else Just $ DocBlock
              { docSummary = T.strip $ T.unlines summary
              , docAnnotations = annotations
              , docLocation = (0, 0)  -- Would need to track actual location
              }

-- | Check if a line contains an annotation
isAnnotationLine :: Text -> Bool
isAnnotationLine line =
  let trimmed = T.strip line
  in "@" `T.isPrefixOf` trimmed

-- | Parse an annotation line
parseAnnotationLine :: Text -> DocAnnotation
parseAnnotationLine line =
  let trimmed = T.strip line
      (tag, rest) = T.break (== ' ') (T.drop 1 trimmed)
      content = T.strip rest
  in case T.toLower tag of
       "fun" -> FunAnnotation content
       "param" ->
         let (paramName, paramDesc) = T.break (== ' ') content
         in ParamAnnotation paramName (T.strip paramDesc)
       "return"     -> ReturnAnnotation     content
       "example"    -> ExampleAnnotation    content
       "desc"       -> DescAnnotation       content
       "author"     -> AuthorAnnotation     content
       "version"    -> VersionAnnotation    content
       "since"      -> SinceAnnotation      content
       "deprecated" -> DeprecatedAnnotation content
       "see"        -> SeeAnnotation        content
       "throws"     -> ThrowsAnnotation     content
       "todo"       -> TodoAnnotation       content
       _            -> DescAnnotation       line -- Unknown annotation becomes description

-- | Parse documentation annotations from text
parseDocAnnotations :: Text -> [DocAnnotation]
parseDocAnnotations text =
  let lines' = T.lines text
      annotLines = filter isAnnotationLine lines'
  in map parseAnnotationLine annotLines

-- | Generate HTML documentation
generateDocs :: [(Text, DocBlock)] -> Text
generateDocs docs =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "  <meta charset=\"UTF-8\">"
    , "  <title>Nytrix Documentation</title>"
    , "  <style>"
    , "    body { font-family: sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; }"
    , "    h1 { color: #333; }"
    , "    h2 { color: #555; border-bottom: 2px solid #ddd; padding-bottom: 10px; }"
    , "    .function { margin: 20px 0; padding: 15px; background: #f5f5f5; border-radius: 5px; }"
    , "    .annotation { margin: 10px 0; }"
    , "    .tag { font-weight: bold; color: #0066cc; }"
    , "    code { background: #e0e0e0; padding: 2px 6px; border-radius: 3px; }"
    , "    pre { background: #2d2d2d; color: #f8f8f2; padding: 15px; border-radius: 5px; overflow-x: auto; }"
    , "  </style>"
    , "</head>"
    , "<body>"
    , "  <h1>Nytrix Documentation</h1>"
    , T.unlines $ map generateFunctionDoc docs
    , "</body>"
    , "</html>"
    ]

-- | Generate documentation for a single function
generateFunctionDoc :: (Text, DocBlock) -> Text
generateFunctionDoc (name, docBlock) =
  T.unlines
    [ "  <div class=\"function\">"
    , "    <h2>" <> escapeHtml name <> "</h2>"
    , "    <p>" <> escapeHtml (docSummary docBlock) <> "</p>"
    , T.unlines $ map generateAnnotationDoc (docAnnotations docBlock)
    , "  </div>"
    ]

-- | Generate documentation for an annotation
generateAnnotationDoc :: DocAnnotation -> Text
generateAnnotationDoc ann = case ann of
  FunAnnotation name ->
    "    <div class=\"annotation\"><span class=\"tag\">Function:</span> <code>" <> escapeHtml name <> "</code></div>"
  ParamAnnotation name desc ->
    "    <div class=\"annotation\"><span class=\"tag\">Parameter:</span> <code>" <> escapeHtml name <> "</code> - " <> escapeHtml desc <> "</div>"
  ReturnAnnotation desc ->
    "    <div class=\"annotation\"><span class=\"tag\">Returns:</span> " <> escapeHtml desc <> "</div>"
  ExampleAnnotation code ->
    "    <div class=\"annotation\"><span class=\"tag\">Example:</span><pre>" <> escapeHtml code <> "</pre></div>"
  DescAnnotation desc ->
    "    <div class=\"annotation\">" <> escapeHtml desc <> "</div>"
  AuthorAnnotation author ->
    "    <div class=\"annotation\"><span class=\"tag\">Author:</span> " <> escapeHtml author <> "</div>"
  VersionAnnotation ver ->
    "    <div class=\"annotation\"><span class=\"tag\">Version:</span> " <> escapeHtml ver <> "</div>"
  SinceAnnotation ver ->
    "    <div class=\"annotation\"><span class=\"tag\">Since:</span> " <> escapeHtml ver <> "</div>"
  DeprecatedAnnotation reason ->
    "    <div class=\"annotation\" style=\"color: #cc6600;\"><span class=\"tag\">⚠ Deprecated:</span> " <> escapeHtml reason <> "</div>"
  SeeAnnotation ref ->
    "    <div class=\"annotation\"><span class=\"tag\">See also:</span> " <> escapeHtml ref <> "</div>"
  ThrowsAnnotation exc ->
    "    <div class=\"annotation\"><span class=\"tag\">Throws:</span> " <> escapeHtml exc <> "</div>"
  TodoAnnotation task ->
    "    <div class=\"annotation\" style=\"color: #666;\"><span class=\"tag\">TODO:</span> " <> escapeHtml task <> "</div>"

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;"
           . T.replace "<" "&lt;"
           . T.replace ">" "&gt;"
           . T.replace "\"" "&quot;"
           . T.replace "'" "&#39;"
