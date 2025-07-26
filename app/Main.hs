module Main where

import ParseNews 

main :: IO ()
main = do
  news <- ParseNews.parseNews "Haskell"
  putStrLn news
