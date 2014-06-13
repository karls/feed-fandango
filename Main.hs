module Main where

import System.Environment (getArgs)
import System.IO
import Network.HTTP
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Data.Maybe (fromMaybe, fromJust)
import Data.String.Unicode
import qualified Data.Map.Lazy as M

data CustomFeedItem = CustomFeedItem { itemId         :: String
                                     , itemTitle      :: String
                                     , itemLink       :: String
                                     , itemAuthor     :: String
                                     , itemCategories :: [String]
                                     }

instance Show CustomFeedItem where
  show (CustomFeedItem ii it il ia ic) =
    unlines ["The item is",
             "\tID: " ++ ii,
             "\tTitle: " ++ it,
             "\tLink: " ++ il,
             "\tAuthor: " ++ ia,
             "\tCategories: " ++ (show ic)]

feedUrl :: String
feedUrl = "http://feeds.theguardian.com/theguardian/uk/rss"

fetchFeed :: String -> IO String
fetchFeed url = do
  resp <- simpleHTTP (getRequest url) >>= getResponseBody
  return $ unicodeRemoveNoneAscii resp

titleOrEmpty :: Item -> String
titleOrEmpty i = fromMaybe "" (getItemTitle i)

convertItem :: Item -> CustomFeedItem
convertItem i = CustomFeedItem { itemId = snd $ fromJust (getItemId i)
                               , itemTitle = fromMaybe "" (getItemTitle i)
                               , itemLink = fromMaybe "" (getItemLink i)
                               , itemAuthor = fromMaybe "" (getItemAuthor i)
                               , itemCategories = getItemCategories i
                               }

processFeed :: Feed -> M.Map Int CustomFeedItem
processFeed feed = M.fromList $ zip [1..numItems] convertedItems
  where
    items = getFeedItems feed
    numItems = length items
    convertedItems = map convertItem items

formatItem :: (Int, CustomFeedItem) -> String
formatItem (idx, item) = "ID: " ++ (show idx) ++ ", title: " ++ (itemTitle item)

displayItems :: [(Int, CustomFeedItem)] -> IO ()
displayItems []     = return ()
displayItems (m:ms) = do
  putStrLn formatted
  displayItems ms
  where
    formatted = formatItem m

play :: M.Map Int CustomFeedItem -> IO ()
play fm = do
  putStr "ID: "
  hFlush stdout
  suppliedId <- getLine
  putStrLn $ show (fm M.! (read suppliedId :: Int))
  putStrLn "Another one?"
  play fm

main :: IO ()
main = do
  feed <- parseFeedFromFile "./feed-guardian.txt"
  let feedMap = processFeed feed
  putStrLn ("Got " ++ (show $ M.size feedMap) ++ " items.")
  putStrLn "Which one do you want to see?"
  displayItems $ M.toList feedMap
  play feedMap
