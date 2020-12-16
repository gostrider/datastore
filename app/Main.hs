module Main where

import           DataStore (initStore)

main :: IO ()
main = do
  print initStore
