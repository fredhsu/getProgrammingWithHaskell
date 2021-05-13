{-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.Text as T

import Data.Either
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Read
import Data.Text.Read

x :: TL.Text
x = "1234"

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

main :: IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn ((TL.pack . show . sum) numbers)

-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as TIO

-- toInts :: T.Text -> [Int]
-- toInts = map (read . T.unpack) . T.lines

-- main :: IO ()
-- main = do
--   userInput <- TIO.getContents
--   let numbers = toInts userInput
--   TIO.putStrLn ((T.pack . show . sum) numbers)