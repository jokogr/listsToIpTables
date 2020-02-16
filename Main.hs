module Main where

import Network.HTTP
import Network.URI


aUrlStr :: String
aUrlStr = "http://list.iblocklist.com/?list=ydxerpxkpcfqjaybcssw&fileformat=p2p&archiveformat=gz"

downLoadFile :: String -> IO String
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

main :: IO ()
main = 
   do gagarinStrings <- downLoadFile aUrlStr
      putStrLn gagarinStrings
      res <- simpleHTTP (getRequest aUrlStr) >>= fmap (take 100) . getResponseBody
      putStrLn res
      putStrLn "finito la musica, pasato la fiesta"

