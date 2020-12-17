{-# LANGUAGE OverloadedStrings #-}

module DataStore where

import           Data.Char (isDigit, isLetter, isSpace)
import           Data.List (findIndex, isInfixOf, map)
import           Data.Text (Text, pack, split, strip, toLower, unpack)


data DataStore = MkStore { size :: Int, items :: [Schema] } deriving Show


data Command
  = Add Schema
  | Get Int
  | Size
  | Search Schema
  | Quit
  deriving Show


data Schema
  = SInt Int
  | SString String
  | Pair Schema Schema
  | Unit -- should we have this case?
  deriving (Show, Eq)


storeSize :: DataStore -> Int
storeSize store = size store


storeItems :: DataStore -> [Schema]
storeItems store = items store


addItems :: DataStore -> Schema -> DataStore
addItems store item = MkStore {size = size', items = items'}
  where
    size' = storeSize store + 1
    items' = item : storeItems store


initStore :: DataStore
initStore = MkStore { size = 0, items = [] }


getItem :: Int -> DataStore -> Maybe (Schema, DataStore)
getItem pos store =
  let
    lastIndex = storeSize store
  in
    -- This should only limit to available # of item
    if pos `elem` [0..lastIndex]
    then Just ((storeItems store) !! pos, store)
    else Nothing


searchItem :: DataStore -> Schema -> Maybe (Schema, DataStore)
searchItem store target =
  case findIndex (== target) (storeItems store) of
    Just i  -> getItem i store
    Nothing -> Nothing


parseSchema :: [String] -> Schema
parseSchema [] = Unit
parseSchema [x] | x == ""           = Unit
                | isLetter (head x) = SString x
                | isDigit (head x)  = SInt (read x :: Int)
parseSchema (x : xs) = Pair (parseSchema [x]) (parseSchema xs)


splitArgs :: String -> [String]
splitArgs args = map unpack $ split isSpace $ pack args


parseCommand :: String -> String -> Maybe Command
parseCommand "add" item    = Just . Add . parseSchema . splitArgs $ item
parseCommand "get" key     = if all isDigit key then Just (Get (read key :: Int)) else Nothing
parseCommand "size" ""     = Just Size
parseCommand "search" item = Just . Search . parseSchema . splitArgs $ item
parseCommand "quit" ""     = Just Quit
parseCommand _ _           = Nothing


parse :: String -> Maybe Command
parse input =
  let
    formatStr :: (Text -> Text) -> String -> String
    formatStr f s = unpack . f . pack $ s
  in
    case span (/= ' ') input of
      (cmd, args) -> parseCommand (formatStr toLower cmd) (formatStr strip args)


display :: Schema -> String
display Unit         = ""
display (SInt x)     = show x
display (SString x)  = x
display (Pair s1 s2) = display s1 ++ " " ++ display s2


processInput :: DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse input of
    Just (Add item)   -> Just ("ID: " ++ (show . storeSize $ store), addItems store item)

    Just (Get key)    -> case (getItem key store) of
                           Just (i, store') -> Just (display i, store')
                           Nothing -> Just ("Failed to get item", store)

    Just Size         -> Just (show $ storeSize store, store)

    Just (Search schema) -> case (searchItem store schema) of
                              Just (i, store') -> Just (display i, store')
                              Nothing -> Just ("Item not found", store)

    Just Quit         -> Nothing
    Nothing           -> Just ("Invalid command", store)

