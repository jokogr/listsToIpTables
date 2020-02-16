{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP
import Network.URI
--import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Codec.Compression.GZip (decompress)


aUrlStr :: String
aUrlStr = "http://list.iblocklist.com/?list=ydxerpxkpcfqjaybcssw&fileformat=p2p&archiveformat=gz"

downLoadFile :: String -> IO String  --It would be better if this returned Lazy ByteString
downLoadFile urlStr = 
  do response <- simpleHTTP request
     case response of 
       Left err -> error $ "Wtf Error :" ++ show err
       Right resp -> handleResponse resp
  where 
     handleResponse resp =
        case rspCode resp of
          (2,_,_) -> return $ rspBody resp
          (3,_,_) -> handleRedirection resp
          _ -> error "what is happening"
     handleRedirection resp = 
       case lookupHeader HdrLocation $ rspHeaders resp of 
          Nothing -> error "Redirection with no location header? Wtf??"
          Just redirUrlStr -> downLoadFile redirUrlStr 

     request = Request { rqURI = uriGagarin
                       , rqMethod = GET
                       , rqHeaders = []
                       , rqBody = []
                       }
     uriGagarin = 
        case parseURI urlStr of
           Nothing -> error $ "What is this url? '" ++ urlStr ++ "' ?? This can never become a Gagarin!"
           Just s -> s

decompressString :: String -> String
decompressString = LC.unpack . decompress . LC.pack

main :: IO ()
main = 
   do gagarinString <- downLoadFile aUrlStr
      putStrLn $ decompressString gagarinString
      putStrLn "finito la musica, pasato la fiesta"

