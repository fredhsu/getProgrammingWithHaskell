import Data.Char
import Data.Map as Map
import Data.List

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String
aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x


third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a->a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)


itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)
itemCount2 :: (String, Int)
itemCount2 = ("Pencils",25)
itemCount3 :: (String, Int)
itemCount3 = ("Pens",13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Q18.1

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- Q18.2
organInventory :: Map.Map Organ Int
organInventory = Map.fromList organCountPairs
     where organCountPairs = zip organs counts
           organs = Data.List.map head (Data.List.group $ Map.elems organCatalog)
           counts = Data.List.map length (Data.List.group $ Map.elems organCatalog )
