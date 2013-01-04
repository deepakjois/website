{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import System.FilePath (joinPath, splitPath)
import Data.List.Split (splitOn)
import Data.Monoid (mappend)
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
  match ("static/**" .&&. complement "static/css/**") $ do
    route stripTopDir
    compile copyFileCompiler

  -- Includes common to different kinds of pages
  match "includes/*" $ compile getResourceBody

  -- Templates for home and inner pages
  match "templates/*" $ compile templateCompiler

  -- Data in JSON format
  match "data/*.json" $ compile getResourceBody

  -- Home page
  match "source/index.markdown" $ do
    route defaultHtml
    compile $ pandocCompiler >>= loadAndApplyTemplate "templates/home.html" pageCtx

  -- Inner HTML pages
  match "source/books/old_2006-2009.html" $ do
    route defaultHtml
    compile $ getResourceBody >>= loadAndApplyTemplate "templates/inner.html" pageCtx

  -- Inner markdown pages
  match innerPages $ do
    route defaultHtml
    compile $ pandocCompiler >>= loadAndApplyTemplate "templates/inner.html" pageCtx

  -- Books
  match bookPages $ do
    route defaultHtml
    compile $
      pandocCompiler >>=
      loadAndApplyTemplate "templates/books.html" booksPageCtx >>=
      loadAndApplyTemplate "templates/inner.html" pageCtx


-- *****************
-- Files
-- *****************

-- Inner pages
innerPages :: Pattern
innerPages = fromList ["source/code.markdown"]

-- Pages containing list of books
bookPages :: Pattern
bookPages = fromList ["source/books.markdown","source/books/2012.markdown","source/books/2011.markdown", "source/books/2010.markdown"]


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
-- Contexts
-- *****************

-- Standard page context which pulls in some common layout elements
pageCtx :: Context String
pageCtx = field "nav"       (\_ -> loadBody "includes/nav.html")       `mappend`
          field "analytics" (\_ -> loadBody "includes/analytics.html") `mappend`
          defaultContext

booksPageCtx :: Context String
booksPageCtx = field "books" getBooks `mappend` defaultContext
 where
  jsonFile :: String -> String
  jsonFile pageFilePath = "data/" ++ (takeWhile (/= '.') . last . splitOn "/") pageFilePath ++ ".json"
  getBooks item = do
    let jsonId  = fromFilePath . jsonFile . toFilePath . itemIdentifier $ item
    jsonBody <- loadBody jsonId
    return . renderHtml . booksJSONToHtml $ jsonBody

-- *****************
-- Configuration
-- *****************

config :: Configuration
config = defaultConfiguration {
           deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
         }
