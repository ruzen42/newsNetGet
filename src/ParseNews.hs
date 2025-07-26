{-# LANGUAGE OverloadedStrings #-}

module ParseNews (parseNews) where 

import Network.HTTP.Req
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

parseNews :: String -> IO String
parseNews query = do
  response <- runReq defaultHttpConfig $ do
    let url = https "habr.com" /: ("ru" :: Text) /: ("search" :: Text)
        params = ("q" :: Text) =: T.pack query
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
      >>> arr (\(href, title) -> ("https://habr.com" ++ href, title))

    formatNews :: [(String, String)] -> String
    formatNews [] = "No news found for your query."
    formatNews items =
      "News found for your query:\n\n" ++
      unlines (map formatItem items)
      where
        formatItem (link, title) = "* " ++ title ++ "\n  " ++ link

