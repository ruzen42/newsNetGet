module Main where

import ParseNews
import Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (filterM)
import Data.List (isInfixOf) as TL

main :: IO ()
main = do
  putStrLn "Starting news parsing tests...\n"

  let
    tests :: [(String, String, String)]
    tests =
      [ ("habr.com", "Haskell", "News found for your query:")
      , ("habr.com", "Rust", "News found for your query:")
      , ("habr.com", "AI", "News found for your query:")
      , ("habr.com", "Quantum Computing", "News found for your query:")
      , ("google.com", "Haskell", "No news found from google.com")
      , ("wikipedia.org", "Haskell", "No news found from wikipedia.org")
      , ("medium.com", "AI", "No news found from medium.com")
      , ("stackoverflow.com", "Haskell", "No news found from stackoverflow.com")
      ]

  testResults <- mapM runTest tests
  let
    succeeded = Prelude.length $ Prelude.filter id testResults
    failed = Prelude.length tests - succeeded

  putStrLn $ "\n--- Test Summary ---"
  putStrLn $ "Total tests: " ++ show (Prelude.length tests)
  putStrLn $ "Succeeded: " ++ show succeeded
  putStrLn $ "Failed: " ++ show failed

  if failed > 0
    then putStrLn "Some tests failed."
    else putStrLn "All tests passed!"

  where
    runTest :: (String, String, String) -> IO Bool
    runTest (site, keyword, expected) = do
      putStrLn $ "--- Running test for '" ++ site ++ "' with keyword '" ++ keyword ++ "' ---"
      output <- parseNews keyword site
      let outputText = T.pack output

      putStrLn $ "Output received (first 10 chars): " ++ Prelude.take 10 output ++ "..."

      let testPassed = if "No news found" `TL.isInfixOf` expected
                         then T.pack "No news found" `TL.isInfixOf` outputText
                         else not (T.pack "No news found" `T.isInfixOf` outputText)

      if testPassed
        then do
          putStrLn "Test PASSED: Output matched expected result.\n"
          return True
        else do
          putStrLn "Test FAILED: Output didn't match expected result.\n"
          return False
