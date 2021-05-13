data AuthorName = AuthorName
  { first :: String,
    last :: String
  }

data Car = Car
  { make :: String,
    model :: String
  }

data Spoiler = Spoiler Int

data SportsCar = SportsCar Car Spoiler

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char
  deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft =
  AuthorCreator
    ( Author
        (TwoInitialsWithLast 'H' 'P' "Lovecraft")
    )

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectableToy = CollectableToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectableToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

-- Q16.1
data Pamphlet = Pamphlet
  { title :: String,
    pamphlet_description :: String,
    contact :: Name,
    pamphletPrice :: Double
  }

-- Q16.2
data Shape = Circle Float | Square Float | Rectangle Float Float

-- data Circle = Circle
--   {radius :: Int}

perimeter :: Shape -> Float
perimeter (Circle r) = pi * r * 2
perimeter (Square side) = side * 4
perimeter (Rectangle l w) = l * 2 + w * 2

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Square side) = side ^ 2
area (Rectangle l w) = l * w