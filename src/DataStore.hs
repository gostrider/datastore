{-# LANGUAGE OverloadedStrings #-}

module DataStore where

import           Data.Char (isDigit)
import           Data.List (findIndex, isInfixOf)
import           Data.Text (Text, pack, strip, toLower, unpack)


data DataStore = MkStore { size :: Int, items :: [String] } deriving Show


data Command = Add String
             | Get Int
             | Size
             | Search String
             | Quit
             deriving Show


storeSize :: DataStore -> Int
storeSize store = size store


storeItems :: DataStore -> [String]
storeItems store = items store


addItems :: DataStore -> String -> DataStore
addItems store item = MkStore {size = size', items = items'}
  where
    size' = storeSize store + 1
    items' = item : storeItems store


initStore :: DataStore
initStore = MkStore { size = 0, items = [] }


getItem :: Int -> DataStore -> Maybe (String, DataStore)
getItem pos store =
  let
    lastIndex = storeSize store
  in
    -- This should only limit to available # of item
    if pos `elem` [0..lastIndex]
    then Just ((storeItems store) !! pos, store)
    else Nothing


searchItem :: DataStore -> String -> Maybe (String, DataStore)
searchItem store target =
  let
    items = storeItems store
    idx = findIndex (isInfixOf target) items
  in
    case idx of
      Just i  -> getItem i store
      Nothing -> Nothing


parseCommand :: String -> String -> Maybe Command
parseCommand "add" item   = Just $ Add item
parseCommand "get" key    = if all isDigit key then Just (Get (read key :: Int)) else Nothing
parseCommand "size" ""    = Just Size
parseCommand "search" str = Just $ Search str
parseCommand "quit" ""    = Just Quit
parseCommand _ _          = Nothing


parse :: String -> Maybe Command
parse input =
  let
    formatStr :: (Text -> Text) -> String -> String
    formatStr f s = unpack . f . pack $ s
  in
    case span (/= ' ') input of
      (cmd, args) -> parseCommand (formatStr toLower cmd) (formatStr strip args)


processInput :: DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse input of
    Just (Add item)   -> Just ("ID: " ++ (show $ storeSize store), addItems store item)
    Just (Get key)    -> case (getItem key store) of
                           Just (i, store') -> Just (show i, store')
                           Nothing -> Just ("Failed to get item", store)
    Just Size         -> Just (show $ storeSize store, store)
    Just (Search str) -> searchItem store str
    Just Quit         -> Nothing
    Nothing           -> Just ("Invalid command", store)

