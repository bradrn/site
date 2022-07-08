{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Trans.State.Strict
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as M
import Data.ByteString.Lazy (toStrict)
import Data.Text (unpack, pack)
import Data.Yaml
import Text.Pandoc.Definition
import Text.Pandoc.Shared (blocksToInlines)
import Text.Pandoc.Walk
import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        -- compile $ pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions pandocTransform
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match (fromList ["projects.yaml"]) $ do
        route $ setExtension "html"
        compile $ do
            projectsYaml <- (fmap.fmap) toStrict getResourceLBS
            projects <- either (fail . prettyPrintParseException) pure $
                traverse (decodeEither' @[Object]) projectsYaml
            let projectCtx = Context $ \k _ i ->
                    case M.lookup (fromString k) (itemBody i) of
                        Just v
                            | Just t <- parseMaybe (withText k $ pure . unpack) v
                            -> pure $ StringField t
                        _ -> noResult $ "Tried field " ++ k
                projectsCtx = mconcat
                    [ listField "projects" projectCtx (pure $ sequenceA projects)
                    , constField "title" "Projects"
                    -- add in parts of 'defaultContext' which are @Context a@
                    , urlField "url"
                    , pathField "path"
                    , titleField "title"
                    ]
            loadAndApplyTemplate "templates/projects.html" projectsCtx projects
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = mconcat
                    [ listField "posts" postCtx (pure posts)
                    , constField "title" "Archives"
                    , defaultContext
                    ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField
                        "posts"
                        (teaserField "teaser" "content" <> postCtx)
                        (pure $ take 10 posts)
                    <> listField "posts" postCtx (return posts) <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

pandocTransform :: Pandoc -> Pandoc
pandocTransform = flip evalState 1 . walkM \case
    Note bs -> do
        i <- get
        let result = Span ("footnote" <> pack (show i), ["sidenote"], []) $
                blocksToInlines bs
        put $ i+1
        pure result
    x -> pure x
