import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Random

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseSelection start sectionSize bytes)

reverseSelection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSelection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

randomChar :: IO Char
randomChar = do
  charVal <- randomRIO (0, 255)
  return (intToChar charVal)

sortSelection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSelection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSelection :: BC.ByteString -> IO BC.ByteString
randomSortSelection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSelection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSelection,
    randomReplaceByte,
    randomSortSelection,
    randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      glitchActions
  --   let glitched = imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"