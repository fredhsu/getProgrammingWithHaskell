import System.Environment

myReplicateM n f = mapM (\_ -> f) [1 .. n]

main :: IO ()
main = do
  xs <- mapM (\_ -> getLine) [1, 2, 3]
  mapM_ putStrLn xs
  -- 22.4

--

