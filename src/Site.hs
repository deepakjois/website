{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import System.FilePath (joinPath, splitPath)
import Data.Monoid (mappend)
import Data.String()
import Text.Blaze.Html.Renderer.String (renderHtml)
import System.FilePath ((<.>), (</>), takeFileName, dropExtension)
import Hakyll


main :: IO ()
main = hakyllWith config $ do

  -- Matches all files in static folder, except CSS files
  match ("static/**") $ do
    route stripTopDir
    compile copyFileCompiler

  -- Templates for home and inner pages
  match "templates/*" $ compile templateCompiler

  -- Home page
  match "source/index.html" $ do
    route defaultHtml
    compile $ getResourceBody >>= loadAndApplyTemplate "templates/home.html" defaultContext

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
-- Configuration
-- *****************

config :: Configuration
config = defaultConfiguration {
           deployCommand = "s3cmd sync  -r _site/*  s3://www.deepak.jois.name"
         }
