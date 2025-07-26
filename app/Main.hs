module Main where

import ParseNews (parseNews)
import Options.Applicative

main :: IO ()
main = do
  news <- parseNews "Haskell"
  putStrLn news
