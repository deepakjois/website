{-# LANGUAGE OverloadedStrings #-}
module Books where

import Data.Monoid (Monoid, mempty, mappend)
import Data.Either (rights)
import Data.String (fromString)
import Data.List (groupBy)
import Data.Char (toLower)

import Text.JSON (JSON, readJSON, showJSON, makeObj, resultToEither)
import Text.JSON.Types (JSValue(..), JSObject, fromJSObject)
import Text.JSON.String (runGetJSON, readJSArray)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Pretty (renderHtml)

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
  showJSON (Book title link author category readable date) =
    makeObj [ ("title",    showJSON title)
            , ("link",     showJSON link)
            , ("author",   showJSON author)
            , ("category", showJSON category)
            , ("readable", showJSON readable)
            , ("date",     showJSON date)
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

-- Convert a string representation of a JSON array to an list of 'Book's
books :: String -> [Book]
books json = rights
           $ map (resultToEither . readJSON)
           $ objArray json where
               objArray json = case runGetJSON readJSArray json of
                                 Right (JSArray xs) -> xs
                                 _                  -> []

-- Month extracted from date
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
bookLiElem book = H.li ! H.dataAttribute "category" (fromString c) $ do
                    booklink
                    starIfReadable
                  where
                    Book t l a c r _ = book
                    booklink         = H.a ! A.href (fromString l) $ H.toHtml (t ++ " by " ++ a)
                    starIfReadable   = if r then H.em ! A.class_ "impt" $ "*" else ""

-- Convert a list of books to a @ul@ element
booksHtmlList books = H.ul $ mapM_ bookLiElem books

-- Convert a list of books to HTML with a month header and list
booksMonthlyHtml books = do H.h2 ! A.id (fromString $ map toLower m) $ H.toHtml m
                            booksHtmlList books
                         where m = monthName $ (read . month) $ head books

-- Convert a list of books grouped by month to HTML
booksYearlyHtml = mapM_ booksMonthlyHtml

-- Group a list of books by the month they were read in
booksGroupedByMonth = groupBy sameMonth
                        where sameMonth a b = month a == month b

-- Render a JSON string representing a list of books to HTML
booksJSONToHtml = (booksYearlyHtml . booksGroupedByMonth) . books

printBooks = do json <- readFile "data/books.json"
                putStr $ renderHtml $ booksJSONToHtml json
