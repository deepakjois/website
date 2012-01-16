{-# LANGUAGE OverloadedStrings #-}
module Books where

import Data.Monoid
import Data.Either
import Data.String
import Data.List
import Data.Char

import Text.JSON
import Text.JSON.Types
import Text.JSON.String
import Text.JSON.Pretty

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Pretty

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.List as L

instance Monoid Bool where
    mempty   = False
    mappend  = (&&)

-- | Book type
--
data Book = Book
    { title    :: String  -- ^ Title
    , link     :: String  -- ^ Link to book (Usually Amazon)
    , author   :: String  -- ^ Author(s)
    , category :: String  -- ^ Category (e.g. non-fiction-other, fiction-indian etc.)
    , readable :: Bool    -- ^ Is the book any good?
    , date     :: String  -- ^ Date read
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
        in do t <- lookupP "title"
              l <- lookupP "link"
              a <- lookupP "author"
              c <- lookupP "category"
              r <- lookupP "readable"
              d <- lookupP "date"
              return $ Book t l a c r d

-- | Convert a string representation of a JSON array to an list of 'Book's
books :: String -> [Book]
books json = rights
           $ Prelude.map (resultToEither . readJSON)
           $ objArray json where
                 objArray json = case runGetJSON readJSArray json of
                     Right (JSArray xs) -> xs
                     _                  -> []

-- | Month extracted from date
month  = (take 2 . drop 5) . date

-- | Convert a month number to its name
--   FIXME is there a better way?
monthName = ([""
             ,"January"
             ,"February"
             ,"March"
             ,"April"
             ,"May"
             ,"June"
             ,"July"
             ,"August"
             ,"September"
             ,"October"
             ,"November"
             ,"December"] !!)

-- | Convert a single book to an @li@ element
bookLiElem book = li ! dataAttribute "category" (fromString c) $ do
                     booklink
                     starIfReadable
                where
                    Book t l a c r _ = book
                    booklink         = H.a ! href (fromString l) $ toHtml (t ++ " by " ++ a)
                    starIfReadable   = if r then em "*" else ""

-- | Convert a list of books to a @ul@ element
booksHtmlList books = ul $ mapM_ bookLiElem books

-- | Convert a list of books to HTML with a month header and list
booksMonthlyHtml books = do h2 ! A.id (fromString $ L.map toLower m) $ toHtml m
                            booksHtmlList books
                        where m = monthName $ (read . month) $ L.head books

-- | Convert a list of books grouped by month to HTML
booksYearlyHtml = mapM_ booksMonthlyHtml

-- | Group a list of books by the month they were read in
booksGroupedByMonth = groupBy sameMonth where
                         sameMonth a b = month a == month b

-- | Render a JSON string representing a list of books to HTML
booksJsonToHtml = (booksYearlyHtml . booksGroupedByMonth) . books

main = do
     json <- readFile "data/books.json"
     putStr $ renderHtml $ booksJsonToHtml json