toInts :: String -> [Int]
toInts = map read . lines

main :: IO()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)
    let squares = map (^2) numbers
    print (sum squares)
