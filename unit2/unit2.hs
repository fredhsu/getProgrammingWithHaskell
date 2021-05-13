{-# LANGUAGE DeriveAnyClass #-}

-- QC 11.1
halve :: Int -> Int
halve x = div x 2

-- QC 11.2
printDouble :: Int -> String
printDouble x = show (x * 2)

-- QC 11.3
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- Q11.2
myTail :: [a] -> [a]
myTail xs =
  if length xs > 0
    then tail xs
    else []

-- myHead :: [a] -> [a] Not possible because it cannot return both a list or a single element

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x

-- Lesson 12
type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

-- QC 12.1
patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType -- data and type constructor are the same

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType B _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeElizabethSmith :: Patient
janeElizabethSmith =
  Patient
    (NameWithMiddle "Jane" "Elizabeth" "Smith")
    Female
    34
    60
    120
    (BloodType A Neg)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

jackieSmithUpdated = jackieSmith {age = 44}

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

canDonateToTwo :: Patient -> Patient -> Bool
canDonateToTwo (Patient _ _ _ _ _ (BloodType O _)) _ = True
canDonateToTwo (Patient _ _ _ _ _ (BloodType AB _)) _ = True
canDonateToTwo
  (Patient _ _ _ _ _ (BloodType A _))
  (Patient _ _ _ _ _ (BloodType B _)) = True
canDonateToTwo
  (Patient _ _ _ _ _ (BloodType B _))
  (Patient _ _ _ _ _ (BloodType B _)) = True
canDonateToTwo _ _ = False

printName :: Patient -> String
printName (Patient (Name f l) _ _ _ _ _) = "Patient Name: " ++ l ++ ", " ++ f

printSex :: Patient -> String
printSex p = "Sex: " ++ sexString (sex p)
  where
    sexString Male = "Male"
    sexString _ = "Female"

patientSummary :: Patient -> String
patientSummary p =
  hr ++ printName p ++ printSex p ++ printAge p ++ printHeight p
    ++ printWeight p
    ++ printBT p
    ++ hr
  where
    hr = "**************\n"
    printAge p = "Age: " ++ show (age p) ++ "\n"
    printHeight p = "Height: " ++ show (height p) ++ " in.\n"
    printWeight p = "Weight: " ++ show (weight p) ++ " lbs.\n"
    printBT p = "Blood Type: " ++ showBloodType (bloodType p) ++ "\n"

-- Lesson 13
--
class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
    else succ n

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

instance Eq SixSidedDie where
  (==) x y = fromEnum x == fromEnum y

instance Ord SixSidedDie where
  (<=) x y = fromEnum x <= fromEnum y

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

data NameTuple = NameTuple (String, String) deriving (Eq, Show)

instance Ord NameTuple where
  compare (NameTuple (f1, l1)) (NameTuple (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [NameTuple]
names =
  [ NameTuple ("Emil", "Cioran"),
    NameTuple ("Eugene", "Thacker"),
    NameTuple ("Friedrich", "Nietzsche")
  ]

class Heads a where
  heads :: a -> Bool

class Die a where
  roll :: a -> a

data FiveSidedDie = Si1 | Si2 | Si3 | Si4 | Si5 deriving (Enum, Show)

instance Die FiveSidedDie where
  roll x = succ x

instance Heads FiveSidedDie where
  heads Si1 = True
  heads Si3 = True
  heads Si5 = True
  heads _ = False

-- Capstone

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where
    sizeOfAlphabet = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

threeLetterDencoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDencoder vals = map rot3ldecoder vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotCharDecoder = rotNdecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size = length bits
    indicies = [size -1, size -2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indicies)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =
  map
    ( \pair ->
        (fst pair) `xor` (snd pair)
    )
    (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

applyOTP' :: String -> String -> [Bits]

applyStream' seed plaintext =
  map
    ( \pair ->
        (fst pair) `xor` (snd pair)
    )
    (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext
