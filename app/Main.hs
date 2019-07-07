{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.ByteString
import Lib

main :: IO ()
main = do
  let input = "Snapchat/10.30.1.1 (iPhone9,3; iOS 10.2; gzip)"
  case (parseOnly (userAgentParser <* endOfInput) input) of
    Left e -> putStrLn e
    Right r -> putStrLn (show r)
