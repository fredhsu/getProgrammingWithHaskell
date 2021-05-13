import Data.List.Split

operator "+" = (+)
operator "-" = (-)
operator "*" = (*)
operator "/" = (/)

calc (x : "+" : y : _) = read x + read y
calc (x : "*" : y : _) = read x * read y

main :: IO ()
main =
  do
    equation <- getContents
    let values = lines equation
    print (calc values)