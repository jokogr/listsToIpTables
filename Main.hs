{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IP
import Network.HTTP
import Network.URI
import System.Process
--import System.Process.Typed
import System.IO

aUrlStr :: String
aUrlStr = "http://list.iblocklist.com/?list=ydxerpxkpcfqjaybcssw&fileformat=p2p&archiveformat=gz"

downLoadFile :: String -> IO String --It would be better if this returned Lazy ByteString
downLoadFile urlStr =
  do
    response <- simpleHTTP request
    case response of
      Left err -> error $ "Wtf Error :" ++ show err
      Right resp -> handleResponse resp
  where
    handleResponse resp =
      case rspCode resp of
        (2, _, _) -> return $ rspBody resp
        (3, _, _) -> handleRedirection resp
        _ -> error "what is happening"
    handleRedirection resp =
      case lookupHeader HdrLocation $ rspHeaders resp of
        Nothing -> error "Redirection with no location header? Wtf??"
        Just redirUrlStr -> downLoadFile redirUrlStr
    request =
      Request
        { rqURI = uriGagarin,
          rqMethod = GET,
          rqHeaders = [],
          rqBody = []
        }
    uriGagarin =
      case parseURI urlStr of
        Nothing -> error $ "What is this url? '" ++ urlStr ++ "' ?? This can never become a Gagarin!"
        Just s -> s

decompressString :: String -> String
decompressString = LC.unpack . decompress . LC.pack

data BlockRecord
  = BlockRecord
      { description :: String,
        startIP :: IPv4,
        endIP :: IPv4
      }
  deriving (Show)

breakOnLast :: (Char -> Bool) -> String -> (String, String)
breakOnLast f s =
  let p = break f (reverse s)
   in (reverse . snd $ p, reverse . fst $ p)

parseBlockString :: String -> [BlockRecord]
parseBlockString blockStr =
  map toBlockRecord
    $ filter (\ln -> not . null $ ln)
    $ tail . lines
    $ blockStr
  where
    toBlockRecord line =
      case breakOnLast (\c -> c == ':') line of
        ([], _) -> error $ "No description in entry '" ++ line ++ "'."
        (descr, ipRangeStr) ->
          BlockRecord
            { description = descr,
              startIP = fst ips,
              endIP = snd ips
            }
          where
            ips = ipRangeToIPs ipRangeStr

ipRangeToIPs :: String -> (IPv4, IPv4)
ipRangeToIPs r =
  case break (\c -> c == '-') r of
    ([], _) -> error "expecting -"
    (ip1, ip2WithMinus) -> (strToIP ip1, strToIP $ drop 1 ip2WithMinus)
      where
        strToIP s = (read s :: IPv4)

main :: IO ()
main =
  do
    gagarinString <- downLoadFile aUrlStr
    --runProcess_ "ipset create iblocklist-level1 hash:ip"
    let blockList = parseBlockString $ decompressString gagarinString
    --mapM_ (\record -> runProcess_ $ shell $ "ipset add iblocklist-level1 " ++ (show $ startIP record) ++ " -exist") blockList

    --mapM_ (putStrLn . show . (\(i,rec) -> (i, startIP rec))) (zip [1..] blockList)
    {-withFile "trololo.lst" WriteMode $
      ( \h ->
          do
            hPutStrLn
              h
              "create iblocklist-level1 hash:net family inet hashsize 262144 maxelem 524287"
            mapM_
              ( \rec ->
                  hPutStrLn h $
                    "add iblocklist-level1 "
                      ++ (show $ startIP rec)
                      ++ "-"
                      ++ (show $ endIP rec)
              )
              blockList
      )-}
    --ipset restore -! < trololo.lst
    (Just hin, _, _, procHandle) <-
       createProcess (proc "ipset restore -!" []){ std_in = CreatePipe }
    hPutStrLn hin
      "create iblocklist-level1 hash:net family inet hashsize 262144 maxelem 524287"
    mapM_
      ( \rec ->
          hPutStrLn hin $
            "add iblocklist-level1 "
              ++ (show $ startIP rec)
              ++ "-"
              ++ (show $ endIP rec)
      )
      blockList
    
    putStrLn "finito la musica, pasato la fiesta"
