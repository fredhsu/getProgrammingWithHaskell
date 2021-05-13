import qualified Data.Map as Map

-- Q21.1
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

inputData :: Map.Map Int String
inputData = Map.fromList [(1, "Fred")]

maybeMain :: Maybe String
maybeMain = do
  --   putStrLn "Hello! What's your name?"
  name <- Map.lookup 1 inputData
  let statement = helloPerson name
  return statement

-- Q21.2

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  putStrLn "Input number:"
  x <- getLine
  let result = fib (read x)
  putStrLn (show result)