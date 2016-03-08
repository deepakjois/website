{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import System.FilePath (joinPath, splitPath)
import Data.Monoid ((<>))
import Data.String()
import Data.List (sortBy, isInfixOf)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.FilePath ((<.>), (</>), takeFileName)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Hakyll
import Text.Pandoc.Options
import Books (booksJSONToHtml)


main :: IO ()
main = hakyllWith config $ do

  -- Matches all files in static folder, except CSS files
  match ("static/**") $ do
    route stripTopDir
    compile copyFileCompiler

  -- Templates for home and inner pages
  match "templates/*" $ compile templateCompiler

  -- Includes for home and inner pages
  match "includes/*" $ compile templateCompiler

  -- Data in JSON format
  match "data/*.json" $ compile getResourceBody

  -- Home page
  match "source/index.html" $ do
    route defaultHtml
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/home.html" (constField "title" "Essays, Notes, Links and Code" <> defaultContext)
        >>= loadAndApplyTemplate "templates/main.html" (constField "title" "Essays, Notes, Links and Code" <> defaultContext)

  -- Inner pages
  match "source/**.md" $ do
    route $ stripTopDir `composeRoutes` setExtension ""
    compile $ pandocMathCompiler
        >>= loadAndApplyTemplate "templates/inner.html" postCtx
        >>= loadAndApplyTemplate "templates/main.html" postCtx

  -- Books
  create bookPages $ do
    route $ stripTopDir `composeRoutes` setExtension ""
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/books.html" booksPageCtx
        >>= loadAndApplyTemplate "templates/main.html" booksPageCtx

  -- List of all posts

  create ["All Posts"] $ do
    route $ idRoute
    compile $ do
      posts <- alphaFirst =<< filterDrafts =<< loadAll "source/**"
      let archiveCtx = listField "posts" postCtx (return posts) <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/all-posts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/main.html" archiveCtx


-- *****************
-- Books
-- *****************

-- Pages containing list of books
bookPages :: [Identifier]
bookPages = map fromFilePath $ zipWith (++) (repeat "source/Book Lists/") (map show bookPageYears)

bookPageYears :: [Int]
bookPageYears = [2010..2016]

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

-- Context for posts
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> unEscapedUrlField "uurl" <> defaultContext

-- Sort items alphabetically
alphaFirst :: [Item a] -> Compiler [Item a]
alphaFirst items = return $
  sortBy (comparing (toFilePath . itemIdentifier)) items

-- Filter any draft items
filterDrafts :: [Item a] -> Compiler [Item a]
filterDrafts items = return $
  filter (not . (isInfixOf "Drafts/") . toFilePath . itemIdentifier) items

-- Context field to get unescaped URL
unEscapedUrlField :: String -> Context a
unEscapedUrlField key = field key $
    fmap (maybe "" id) . getRoute . itemIdentifier

-- Context for a page listing books
booksPageCtx :: Context String
booksPageCtx = field "books" getBooks <>
               field "year"  getYear  <>
               field "title" getTitle <>
               defaultContext
 where
  jsonFile pageFilePath = "data" </> (year pageFilePath <.> "json")
  getBooks item = do
    let jsonId  = fromFilePath . jsonFile . toFilePath . itemIdentifier $ item
    jsonBody <- loadBody jsonId
    return . renderHtml . booksJSONToHtml $ jsonBody

-- Generate a year for an inner book list page
getYear :: forall a. Item a -> Compiler String
getYear = return . year . toFilePath . itemIdentifier

-- Generate a title for an inner book list page
getTitle :: forall a. Item a -> Compiler String
getTitle = return . title . toFilePath . itemIdentifier

-- Get the year from a path to a inner book page
year :: FilePath -> String
year = takeFileName

title :: FilePath -> String
title = ((++) "Book List ") . year

-- *****************
-- Compilers
-- *****************

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- *****************
-- Configuration
-- *****************

config :: Configuration
config = defaultConfiguration {
           deployCommand = "s3cmd sync --guess-mime-type --no-mime-magic --delete-removed -r _site/  s3://www.deepak.jois.name/"
         }
