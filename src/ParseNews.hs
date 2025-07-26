{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module ParseNews (parseNews) where 

import Network.HTTP.Req
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List.Split (splitOn)

parseNews :: String -> String -> IO String
parseNews query website = do
  putStrLn "Starting..."
  response <- runReq defaultHttpConfig $ do
    let (domain, paths) = parseWebsiteName website

    let url = constructUrlPath (https domain) paths 
        params = (T.pack query) =: T.pack query
    req GET url NoReqBody bsResponse params
  
  let htmlContent = responseBody response
  newsItems <- parseHtmlContent htmlContent

  return $ formatNews newsItems

  where
    parseHtmlContent :: BS.ByteString -> IO [(String, String)]
    parseHtmlContent html = runX $
      readString [withParseHTML yes, withWarnings no] (T.unpack $ TE.decodeUtf8 html)
      >>> css ("h2.tm-title > a" :: String)
      >>> (getAttrValue0 "href" &&& deep getText)
      >>> arr (\(href, title) -> (getHost (parseWebsiteName website) ++ href, title))

    formatNews :: [(String, String)] -> String
    formatNews [] = "No news found for your query."
    formatNews items =
      "News found for your query:\n\n" ++
      unlines (map formatItem items)
      where
        formatItem (link, title) = "**Title** " ++ title ++ ".\n " ++ link ++ "\n"
    parseWebsiteName :: String -> (Text, [Text])
    parseWebsiteName urlString = (T.pack domain, map T.pack pathSegments) 
      where 
        parts = splitOn "/" urlString
        domain = head parts
        pathSegments = tail parts 
    getHost :: (Text, [Text]) -> String 
    getHost (name, [_]) = T.unpack name 

    constructUrlPath :: Url 'Https -> [Text] -> Url 'Https
    constructUrlPath initialUrl [] = initialUrl
    constructUrlPath initialUrl (p:ps) = foldl (/:) (initialUrl /: p) ps

        
