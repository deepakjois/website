{-# LANGUAGE OverloadedStrings, RankNTypes, CPP #-}
import System.FilePath (joinPath, splitPath)
import Data.Monoid ((<>))
import Data.String()
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.FilePath ((<.>), (</>), takeFileName, splitDirectories)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Hakyll
import Text.Pandoc.Options
import Books (booksJSONToHtml)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Control.Monad (msum)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (formatTime)
import qualified Data.Time.Format as TF

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
  match "data/**.json" $ compile getResourceBody

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
      posts <- alphaFirst =<< loadAll "source/**"
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
postCtx = dateField "date" "%B %e, %Y" <> unEscapedUrlField "uurl" <>  updatedDateField "updated" "%B %e, %Y"  <> defaultContext

-- Sort items alphabetically
alphaFirst :: [Item a] -> Compiler [Item a]
alphaFirst items = return $
  sortBy (comparing (toFilePath . itemIdentifier)) items

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
  jsonFile pageFilePath = "data" </> "books" </> (year pageFilePath <.> "json")
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

-- *****************
-- Extracting updated date from metadata.
-- *****************

--------------------------------------------------------------------------------
-- | When the metadata has a field called @published@ in one of the
-- following formats then this function can render the date.
--
--   * @Mon, 06 Sep 2010 00:01:00 +0000@
--
--   * @Mon, 06 Sep 2010 00:01:00 UTC@
--
--   * @Mon, 06 Sep 2010 00:01:00@
--
--   * @2010-09-06T00:01:00+0000@
--
--   * @2010-09-06T00:01:00Z@
--
--   * @2010-09-06T00:01:00@
--
--   * @2010-09-06 00:01:00+0000@
--
--   * @2010-09-06 00:01:00@
--
--   * @September 06, 2010 00:01 AM@
--
-- Following date-only formats are supported too (@00:00:00@ for time is
-- assumed)
--
--   * @2010-09-06@
--
--   * @September 06, 2010@
--
-- Alternatively, when the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages)
-- and no @published@ metadata field set, this function can render
-- the date. This pattern matches the file name or directory names
-- that begins with @yyyy-mm-dd@ . For example:
-- @folder//yyyy-mm-dd-title//dist//main.extension@ .
-- In case of multiple matches, the rightmost one is used.

updatedDateField :: String     -- ^ Key in which the rendered date should be placed
          -> String     -- ^ Format to use on the date
          -> Context a  -- ^ Resulting context
updatedDateField = updatedDateFieldWith defaultTimeLocale


--------------------------------------------------------------------------------
-- | This is an extended version of 'dateField' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'dateField'.
updatedDateFieldWith :: TimeLocale  -- ^ Output time locale
              -> String      -- ^ Destination key
              -> String      -- ^ Format to use on the date
              -> Context a   -- ^ Resulting context
updatedDateFieldWith locale key format = field key $ \i -> do
    time <- getItemUpdatedUTC locale $ itemIdentifier i
    return $ formatTime locale format time


--------------------------------------------------------------------------------
-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'dateField' for more information.
-- Exported for user convenience.
getItemUpdatedUTC :: MonadMetadata m
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUpdatedUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
        paths          = splitDirectories $ toFilePath id'

    maybe empty' return $ msum $
        [tryField "updated" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fnCand | fnCand <- reverse paths]
  where
    empty'     = fail $ "Main.getItemUpdatedUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
parseTimeM = TF.parseTimeM
#else
parseTimeM _ = TF.parseTime
#endif
