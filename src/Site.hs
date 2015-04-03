{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import System.FilePath (joinPath, splitPath)
import Data.Monoid (mappend)
import Data.String()
import System.FilePath ((<.>), (</>), dropExtension)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Hakyll
import Books (booksJSONToHtml)


main :: IO ()
main = hakyllWith config $ do

  -- Matches all files in static folder, except CSS files
  match ("static/**") $ do
    route stripTopDir
    compile copyFileCompiler

  -- Templates for home and inner pages
  match "templates/*" $ compile templateCompiler

  -- Data in JSON format
  match "data/*.json" $ compile getResourceBody

  -- Home page
  match "source/index.html" $ do
    route defaultHtml
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/home.html" (field "title" (\_ -> return "Essays, Notes, Links and Code") `mappend` defaultContext)
        >>= loadAndApplyTemplate "templates/main.html" defaultContext

  -- Inner pages
  match "source/*.md" $ do
    route $ stripTopDir `composeRoutes` setExtension ""
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/inner.html" defaultContext
        >>= loadAndApplyTemplate "templates/main.html" defaultContext

  -- Books
  create bookPages $ do
    route idRoute
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/books.html" booksPageCtx
        >>= loadAndApplyTemplate "templates/main.html" defaultContext


-- *****************
-- Books
-- *****************

-- Pages containing list of books
bookPages :: [Identifier]
bookPages = map fromFilePath $ zipWith (++) (repeat "Book List ") (map show [2010..2015])


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

-- Context for a page listing books	
booksPageCtx :: Context String
booksPageCtx = field "books" getBooks `mappend`
               field "year"  getYear  `mappend`
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

-- Get the year from a path to a inner book page
year :: FilePath -> String
year = last . words

-- *****************
-- Configuration
-- *****************

config :: Configuration
config = defaultConfiguration {
           deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
         }
