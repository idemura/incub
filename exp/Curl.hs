import Network.Curl
import Data.Char (isSpace)
import Data.List
import Control.Monad (mapM_)

isRedirectCode :: String -> Bool
isRedirectCode s = or . map (`isPrefixOf` s) $ ["HTTP/1.1 301", "HTTP/1.1 302"]

getRedirect :: String -> IO String
getRedirect url = do
    h <- curlHead url []
    --print h
    return (parse url h)
  where
    parse :: String -> (String, [(String, String)]) -> String
    parse url (code, kvs)
      | isRedirectCode code = let
          v = [dropWhile isSpace v | (k, v) <- kvs, k == "Location"]
        in
          if null v then url else head v
      | otherwise = url

test :: String -> IO ()
test shortUrl = do
    url <- getRedirect shortUrl
    putStrLn $ concat $ [shortUrl, " -> `", url, "`"]

main = withCurlDo $ do
  mapM_ test ["bit.ly/4nHKYP", "ow.ly/8Ohrt", "vk.cc/yE0wH", "j.mp/zDGh5T"]
