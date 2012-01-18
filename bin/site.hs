{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>))
import System.FilePath (joinPath, splitPath)
import Data.List.Split (splitOn)
import Data.String (fromString)
import Text.Blaze.Renderer.String (renderHtml)
import Hakyll

import Books (booksJsonToHtml)

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
  match innerPages $ do
    route defaultHtml
    compile $ defaultCompiler "templates/inner.html"

  -- Books
  forM_ bookPages $ \b -> match (parseGlob b) $ do
      route defaultHtml
      compile $ bookPageCompiler (parseIdentifier $ jsonFile b)
        where jsonFile page = "data/" ++
                              (takeWhile (/= '.') . last . splitOn "/") (page) ++
                              ".json"


-- *****************
-- Files
-- *****************

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

-- | Inner pages (currently only code.markdown)
innerPages = list ["source/code.markdown", "source/books/old_2006-2009.html"]

-- | Pages containing a list of books
bookPages = ["source/books.markdown", "source/books/2010.markdown"]


-- *****************
-- Routes
-- *****************

-- | Custom route to drop the topmost dir from the identifier
stripTopDir = customRoute $ joinPath . tail . splitPath . toFilePath

-- | Combination of dropping the topmost dir and adding the HTML extension
defaultHtml = stripTopDir `composeRoutes` setExtension "html"


-- *****************
-- Compilers
-- *****************

-- | Default compiler for all pages
defaultCompiler template = pageCompiler >>>
                           renderLayout template

-- | Compiler for pages containing book list
bookPageCompiler json = pageCompiler >>>
                        renderBook json >>>
                        renderLayout "templates/inner.html"

-- | Render a list of books
renderBook json = setFieldPage "books" json >>>
                  arr (changeField "books" $ renderHtml . booksJsonToHtml) >>>
                  applyTemplateCompiler "templates/books.html"

-- | Render a standard layout containing some includes
renderLayout template = setFieldPage "analytics" "includes/analytics.html" >>>
                        setFieldPage "nav" "includes/nav.html" >>>
                        applyTemplateCompiler template


-- *****************
-- Configuration
-- *****************

config = defaultHakyllConfiguration {
      deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
}