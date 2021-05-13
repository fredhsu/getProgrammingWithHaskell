import qualified Data.Map as Map

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

minPart :: Maybe RobotPart -> Maybe RobotPart -> Maybe Double
minPart part1 part2 = min <$> (cost <$> part1) <*> (cost <$> part2)

main :: IO ()
main = do
  let db =
        Map.fromList
          [ (1, RobotPart "a" "a" 10.0 3),
            (2, RobotPart "b" "b" 11.0 3),
            (3, RobotPart "c" "c" 12.0 3),
            (4, RobotPart "d" "d" 13.0 3),
            (5, RobotPart "e" "e" 14.0 3)
          ]
  pid1 <- getLine
  pid2 <- getLine
  let part1 = Map.lookup (read pid1) db
  let part2 = Map.lookup (read pid2) db
  let part = minPart part1 part2
  print part
