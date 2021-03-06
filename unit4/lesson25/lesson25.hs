import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = B.unpack sampleBytes

bcToInt :: BC.ByteString -> Int
bcToInt = read . B.unpack 