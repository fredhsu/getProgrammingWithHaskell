{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   let fileName = head args
   input <- TI.readFile fileName
   let caps = T.toUpper input
   TI.writeFile "capped.txt" caps
