{-# LANGUAGE OverloadedStrings #-}
module Books where

import Data.Monoid (Monoid, mempty, mappend)
import Data.Either (rights)
import Data.String (fromString)
import Data.List (groupBy)
import Data.Char (toLower)

import Text.JSON (JSON, readJSON, showJSON, makeObj, resultToEither)
import Text.JSON.Types (JSValue(..), fromJSObject)
import Text.JSON.String (runGetJSON, readJSArray)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

-- 'readable' property in JSON is optional,
-- so Bool needs to be a monoid to allow
-- for convenient conversion
instance Monoid Bool where
  mempty   = False
  mappend  = (&&)

-- Book type
data Book = Book {
  title    :: String,  -- Title
  link     :: String,  -- Link to book (Usually Amazon)
  author   :: String,  -- Author(s)
  category :: String,  -- Category (e.g. non-fiction-other, fiction-indian etc.)
  readable :: Bool,    -- Is the book any good?
  date     :: String   -- Date read
} deriving Show

instance JSON Book where
  showJSON (Book t l a c r d) =
    makeObj [ ("title",    showJSON t)
            , ("link",     showJSON l)
            , ("author",   showJSON a)
            , ("category", showJSON c)
            , ("readable", showJSON r)
            , ("date",     showJSON d)
            ]

  readJSON (JSObject obj) =
    let jsonObjAssoc = fromJSObject obj
        lookupP key = case lookup key jsonObjAssoc of
                        Just o  -> readJSON o
                        Nothing -> return mempty
    in do
      t <- lookupP "title"
      l <- lookupP "link"
      a <- lookupP "author"
      c <- lookupP "category"
      r <- lookupP "readable"
      d <- lookupP "date"
      return $ Book t l a c r d

  readJSON _ = undefined
-- Convert a string representation of a JSON array to an list of 'Book's
books :: String -> [Book]
books json = rights
           $ map (resultToEither . readJSON) objArray where
               objArray = case runGetJSON readJSArray json of
                            Right (JSArray xs) -> xs
                            _                  -> []

-- Month extracted from date
month :: Book -> String
month  = (take 2 . drop 5) . date

-- Convert a month number to its name
-- FIXME is there a better way?
monthName :: Int -> String
monthName n = [ "January"
              , "February"
              , "March"
              , "April"
              , "May"
              , "June"
              , "July"
              , "August"
              , "September"
              , "October"
              , "November"
              , "December"
              ] !! (n-1)

-- Convert a single book to an <li> element
bookLiElem :: Book -> H.Html
bookLiElem book = H.li ! H.dataAttribute "category" (fromString c) $ do
                    booklink
                    fromString (" by " ++ a)
                    starIfReadable
                  where
                    Book t l a c r _ = book
                    booklink         = H.a ! A.href (fromString l) $ H.toHtml t
                    starIfReadable   = if r then H.em ! A.class_ "impt" $ "*" else ""

-- Convert a list of books to a @ul@ element
booksHtmlList :: [Book] -> H.Html
booksHtmlList b = H.ul $ mapM_ bookLiElem b

-- Convert a list of books to HTML with a month header and list
booksMonthlyHtml :: [Book] -> H.Html
booksMonthlyHtml b = do H.h2 ! A.id (fromString $ map toLower m) $ H.toHtml m
                        booksHtmlList b
                     where m = monthName $ (read . month) $ head b

-- Convert a list of books grouped by month to HTML
booksYearlyHtml :: [[Book]] -> H.Html
booksYearlyHtml = mapM_ booksMonthlyHtml

-- Group a list of books by the month they were read in
booksGroupedByMonth :: [Book] -> [[Book]]
booksGroupedByMonth = groupBy sameMonth
                        where sameMonth a b = month a == month b

-- Render a JSON string representing a list of books to HTML
booksJSONToHtml :: String -> H.Html
booksJSONToHtml = (booksYearlyHtml . booksGroupedByMonth) . books

-- Test method
printBooks :: IO ()
printBooks = do json <- readFile "data/books.json"
                putStr $ renderHtml $ booksJSONToHtml json
