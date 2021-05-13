import Data.Char

-- Q4.1
compareLastNames name1 name2 = compare lastName1 lastName2
    where lastName1 = snd name1
          lastName2 = snd name2

-- Q4.2
sfOffice name = if lastName < "L"
                then nameText
                    ++ " - PO Box 1234 - SF, CA 94111"
                else nameText
                    ++ " - PO Box 1010 - SF, CA 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - NY, NY"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456, Reno, NV"
    where nameText = snd name

dcOffice name = nameText ++ " - PO Box 1100, Washington, DC"
    where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq."

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))


-- 5.1
ifEven f x = if even x 
             then f x 
             else x

genIfEven f = (\x -> ifEven f x)

inc x = x + 1

-- ifEvenInc = genIfEven inc

-- QuickCheck 5.1

genIfXEven x = (\f -> ifEven f x)

-- 5.2
--

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey
                                        
genHostRequestBuilder host = (\apiKey resource id ->
                                getRequestURL host apiKey resource id)

genApiRequestBuilder host resource = (\apiKey id ->
                                        getRequestURL host apiKey resource id)
                                        
exampleUrlBuilder = getRequestURL "http://example.com"
myExampleUrlBuilder = exampleUrlBuilder "1337Haskell" 
myExampleRequestBuilder = myExampleUrlBuilder "book"

subtract2 :: Integer -> Integer
subtract2 = flip (-) 2 

-- Q5.1

ifEvenInc = ifEven inc

ifEvenDouble = ifEven (2 *)

ifEvenSquare = ifEven (^2)

-- Q5.2

binaryPartialApplication f x = (\y -> f x y)

-- Q6.1
myRepeat x = cycle [x]

-- Q6.2
subseq start end xs = (drop start (take end xs) )

-- Q6.3
inFirstHalf x xs = if elem x firstHalf
                    then True 
                    else False 
                    where firstHalf = take half xs
                          half = length xs `div` 2


-- Listing 7.4
myHead (x:xs) = x
myHead [] = error "No head for empty list"

-- QC 7.3
myTail (_:xs) = xs

-- Q7.1
myTail [] = []

-- Q7.2
myGCD a 0 = a
myGCD a b = myGCD b remainder
    where remainder = a `mod` b

-- QC 8.1
myLength [] = 0
myLength (x:xs) = 1 + myLength (xs)
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m -1) (ackermann m (n - 1)) 

collatz 1 = 1
collatz n = if even n then 1 + collatz (n `div` 2) else 1 + collatz ((n * 3) + 1)

-- Q8.1
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q8.2
fastFib n1 n2 0 = n2
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

-- QC9.1
remove _ [] = []
remove f (x:xs) = if f x
                    then remove f xs
                    else x : remove f xs

-- QC9.2
myProduct xs = foldl (*) 1 xs

myFoldl _ init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                            where newInit = f init x

myFoldr _ init [] = init
myFoldr f init (x:xs) = f x rightResult
                            where rightResult = myFoldr f init xs

-- Q9.1
myElem x xs = length(filtered) == 0
                where filtered = filter x xs

-- Q9.2
isPalindrome2 xs = normalized xs == reverse (normalized xs)
                    where normalized y = map toLower (noSpaces y)
                          noSpaces z = filter (\x -> isSpace x == False) z

-- Q9.3
harmonic n = foldr (+) 0 series
                where series = map (\x -> 1/x) [1..n]

cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
    where ozDiff = flOz - ozDrank
          flOz = getOz aCup

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1,1,1,1,1]