{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Attoparsec.ByteString
import Data.ByteString
import Network.Simple.TCP

main :: IO ()
main = connect "localhost" "32801" $ \(connectionSocket, remoteAddr) -> do
      send connectionSocket "CONNECT\n\n\NUL"
      send connectionSocket "SUBSCRIBE\nid:0\ndestination:/queue/main\n\n\NUL"
      maybeString <- recv connectionSocket 1024
      print maybeString
      let maybeFrame = parseMaybe maybeString
      print maybeFrame

parseMaybe :: Maybe ByteString -> Maybe StompFrame
parseMaybe (Just s) = case parseFrame s of
                        Done _ res   -> Just res
                        Fail i _ v   -> Just StompFrame{command="FAIL", headers=[], body=show i ++ v}
                        Partial _    -> Just StompFrame{command="PARTIAL", headers=[], body=""}
parseMaybe Nothing  = Nothing
{-
main = do
  inpStr <- getLine
  let result = do parseFrame $ toByteString inpStr
  let outStr = case result of
                Done _ f  -> show f
                Partial _ -> "Parse did not finish" 
                Fail {}   -> "Parse failed: " ++ show result
  putStrLn outStr
-}
