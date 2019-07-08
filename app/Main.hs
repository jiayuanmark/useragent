{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Lib
import System.Environment
import qualified Data.ByteString.Char8 as B

csvWrapper :: Parser UserAgent
csvWrapper = userAgentParser <|> (char '"' *> userAgentParser <* char '"')

main :: IO ()
main = do
  [fn] <- getArgs
  content <- B.split '\n' <$> B.readFile fn
  flip mapM_ content $ \ln ->
    case parseOnly (csvWrapper <* endOfInput) ln of
      Left e -> B.putStrLn $ B.intercalate " "
        ["cannot parse:", ln, "because of", (B.pack e)]
      Right _ -> return ()
