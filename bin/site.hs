{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler


    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "analytics.html" $ compile pageCompiler
    match "nav.html" $ compile pageCompiler

    match "templates/*" $ compile templateCompiler

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
             >>> requireA "analytics.html" (setFieldA "analytics" $ arr pageBody)
             >>> requireA "nav.html" (setFieldA "nav" $ arr pageBody)
             >>> applyTemplateCompiler "templates/home.html"

    match "code.markdown" $ do
        route $ setExtension "html"
        compile $ pageCompiler
             >>> requireA "analytics.html" (setFieldA "analytics" $ arr pageBody)
             >>> requireA "nav.html" (setFieldA "nav" $ arr pageBody)
             >>> applyTemplateCompiler "templates/inner.html"
