import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  byteFile <- B.readFile fileName
  let byteCount = B.length byteFile
  putStrLn ("bytes:" ++ show byteCount)
  print "Chars:"
  print ((T.length . E.decodeUtf8) byteFile)
