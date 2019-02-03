module Main where

import Malodivo
import qualified Data.ByteString.Lazy as B

main :: IO ()
main =
  do
    file <- B.getContents
    let input = decodeInput file
        out = encodeResult input
    B.putStr out
