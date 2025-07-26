module Main where

import ParseNews (parseNews)

import Options.Applicative

data MyOptions = MyOptions
  { keywords :: String
  , website  :: String
  } deriving (Show)

main :: IO ()
main = do
  opts <- execParser parserInfo
  output <- parseNews (keywords opts) (website opts)
  putStrLn output

myOptionsParser :: Parser MyOptions
myOptionsParser = MyOptions
  <$> strOption
      ( long "keywords"
     <> short 'k'
     <> metavar "keywords"
     <> help "Keywords for the search (required)"
      )
  <*> strOption
      ( long "website"
     <> short 'w'
     <> metavar "website"
     <> help "Website address (required)"
      )

parserInfo :: ParserInfo MyOptions
parserInfo = info (myOptionsParser <**> helper)
  ( fullDesc
 <> progDesc "A simple argument parser for keywords and a website."
 <> header "optparse-applicative example"
  )

