{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import System.FilePath
import Text.Blaze.Renderer.String

import Hakyll
import Books

main :: IO ()
main = hakyllWith config $ do
    -- CSS files
    match cssFiles $ do
        route stripTopDir
        compile compressCssCompiler

    -- Static files, except CSS
    match staticFilesExceptCss $ do
        route stripTopDir
        compile copyFileCompiler

    -- Include files
    match includes $ compile readPageCompiler

    -- Template files
    match templates $ compile templateCompiler

    -- Data files
    match jsonData $ compile readPageCompiler

    -- Home page
    match "source/index.markdown" $ do
        route defaultHtml
        compile $ defaultCompiler "templates/home.html"

    -- Inner pages
    match "source/code.markdown" $ do
        route defaultHtml
        compile $ defaultCompiler "templates/inner.html"

    -- Books
    match "source/books.markdown" $ do
        route defaultHtml
        compile $ bookPageCompiler "books.json"

-- | Matches css files in static folder
cssFiles = "static/css/**"

-- | Matches all files in static folder, except CSS files
staticFilesExceptCss = predicate (\i -> matches "static/**" i && not (matches "static/css/**" i))

-- | Includes common to different kinds of pages
includes = "includes/**"

-- | Templates for home and inner pages
templates = "templates/**"

-- | Data in JSON format
jsonData = "data/*.json"

-- | Custom route to drop the topmost dir from the identifier
stripTopDir = customRoute $ joinPath . tail . splitPath . toFilePath

-- | Combination of dropping the topmost dir and adding the HTML extension
defaultHtml = stripTopDir `composeRoutes` setExtension "html"

-- | Default compiler for all pages
defaultCompiler template = pageCompiler
                       -- Google Analytics code
                       >>> setFieldPage "analytics" "includes/analytics.html"
                       -- Nav bar
                       >>> setFieldPage "nav" "includes/nav.html"
                       -- Template
                       >>> applyTemplateCompiler template

bookPageCompiler json = pageCompiler
                    >>> setFieldPage "books" "data/books.json"
                    >>> arr (changeField "books" $ renderHtml . booksJsonToHtml)
                   >>> applyTemplateCompiler "templates/books.html"
                       -- Google Analytics code
                   >>> setFieldPage "analytics" "includes/analytics.html"
                       -- Nav bar
                   >>> setFieldPage "nav" "includes/nav.html"
                       -- Templates
                   >>> applyTemplateCompiler "templates/inner.html"

config = defaultHakyllConfiguration
    {
      deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
    }