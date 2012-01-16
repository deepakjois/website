{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import System.FilePath

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

    -- Home page
    match "source/index.markdown" $ do
        route defaultHtml
        compile $ defaultCompiler "templates/home.html"

    -- Inner pages
    match "source/code.markdown" $ do
        route defaultHtml
        compile $ defaultCompiler "templates/inner.html"

    -- TODO Books

-- | Matches css files in static folder
cssFiles = "static/css/**"

-- | Matches all files in static folder, except CSS files
staticFilesExceptCss = predicate (\i -> matches "static/**" i && not (matches "static/css/**" i))

-- | Includes common to different kinds of pages
includes = "includes/**"

-- | Templates for home and inner pages
templates = "templates/**"

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



config = defaultHakyllConfiguration
    {
      deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
    }