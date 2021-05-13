import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkham", (42.6054, -70.7829)),
      ("Innsmouth", (42.8250, -70.8150)),
      ("Carcosa", (29.9714, -90.7694)),
      ("New York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just (x + y)

-- Q28.1
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO c1 c2 = do
  x1 <- c1
  x2 <- c2
  return $ haversine x1 x2

-- Q28.2
haversineIO2 :: IO LatLong -> IO LatLong -> IO Double
haversineIO2 c1 c2 = haversine <$> c1 <*> c2
