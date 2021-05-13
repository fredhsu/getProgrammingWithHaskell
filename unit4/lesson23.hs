{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

somewords :: T.Text
somewords = "this\nis\nit"

main :: IO()
main = do
    print aWord

-- 23.3

myLines txt = T.splitOn "\n" txt
myUnlines txt = T.intercalate "\n" txt
