{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>))
import System.FilePath (joinPath, splitPath)
import Data.List.Split (splitOn)
import Data.String()
import Text.Blaze.Html.Renderer.String (renderHtml)
import Hakyll

import Books (booksJSONToHtml)

main :: IO ()
main = hakyllWith config $ do

  -- Matches css files in static folder
  match "static/css/**" $ do
    route stripTopDir
    compile compressCssCompiler

  -- Matches all files in static folder, except CSS files
  match (predicate (\i -> matches "static/**" i && not (matches "static/css/**" i))) $ do
    route stripTopDir
    compile copyFileCompiler

  -- Includes common to different kinds of pages
  match "includes/*" $ compile readPageCompiler

  -- Templates for home and inner pages
  match "templates/*" $ compile templateCompiler

  -- Data in JSON format
  match "data/*.json" $ compile readPageCompiler

  -- Home page
  match "source/index.markdown" $ do
    route defaultHtml
    compile $ defaultCompiler "templates/home.html"

  -- Inner HTML pages
  match "source/books/old_2006-2009.html" $ do
          route defaultHtml
          compile $ defaultHtmlCompiler "templates/inner.html"

  -- Inner markdown pages
  match innerPages $ do
    route defaultHtml
    compile $ defaultCompiler "templates/inner.html"

  -- Books
  forM_ bookPages $ \b ->
    match (parseGlob b) $ do
      route defaultHtml
      compile $ bookPageCompiler (parseIdentifier $ jsonFile b)
        where jsonFile page = "data/" ++
                              (takeWhile (/= '.') . last . splitOn "/") page ++
                              ".json"


-- *****************
-- Files
-- *****************

-- Inner pages
innerPages :: forall a. Pattern a
innerPages = list ["source/code.markdown"]

-- Pages containing list of books
bookPages :: [String]
bookPages = ["source/books.markdown","source/books/2011.markdown", "source/books/2010.markdown"]


-- *****************
-- Routes
-- *****************

-- Custom route to drop the topmost dir from the identifier
stripTopDir :: Routes
stripTopDir = customRoute $ joinPath . tail . splitPath . toFilePath

-- Combination of dropping the topmost dir and adding the HTML extension
defaultHtml :: Routes
defaultHtml = stripTopDir `composeRoutes` setExtension "html"


-- *****************
-- Compilers
-- *****************

-- Default compiler for HTML pages
defaultHtmlCompiler :: Identifier Template
                    -> Compiler Resource (Page String)
defaultHtmlCompiler template = readPageCompiler >>>
                               renderLayout template

-- Default compiler for markdown pages
defaultCompiler :: Identifier Template
                -> Compiler Resource (Page String)
defaultCompiler template = pageCompiler >>>
                           renderLayout template

-- Compiler for pages containing book list
bookPageCompiler :: Identifier (Page String)
                 -> Compiler Resource (Page String)
bookPageCompiler json = pageCompiler >>>
                        renderBookPage json >>>
                        renderLayout "templates/inner.html"

-- Render a list of books
renderBookPage :: Identifier (Page String)
               -> Compiler (Page String) (Page String)
renderBookPage json = setFieldPage "books" json >>>
                      arr (changeField "books" $ renderHtml . booksJSONToHtml) >>>
                      applyTemplateCompiler "templates/books.html"

-- Render a standard layout containing some includes
renderLayout :: Identifier Template
             -> Compiler (Page String) (Page String)
renderLayout template = setFieldPage "analytics" "includes/analytics.html" >>>
                        setFieldPage "nav" "includes/nav.html" >>>
                        applyTemplateCompiler template


-- *****************
-- Configuration
-- *****************

config :: HakyllConfiguration
config = defaultHakyllConfiguration {
           deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
         }
