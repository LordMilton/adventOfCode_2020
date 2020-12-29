import Prelude
import Data.List
import Data.Maybe
import Data.List.Split
import Debug.Trace

firstBusNum = 17
--suggestedStart = 1000000000
--suggestedStart = 100000000000000
suggestedStart = 100

myTrace :: String -> a -> a
myTrace str val = trace str val
--myTrace str val = val

myRead :: IO [String]
myRead = do
   time <- getLine
   buses <- getLine
   let parsedBuses = splitOn "," buses in
      return (parsedBuses)

main :: IO Integer
main = do
   input <- myRead
   return (solve (transformInput input))

transformInput :: [String] -> [Maybe Integer]
transformInput (x:xs)   | x == "x"  = Nothing:(transformInput xs)
                        | otherwise = (Just ((read x) :: Integer)):(transformInput xs)
transformInput _        = []

maybeMax :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMax (Just x) (Just y) | x > y     = Just x
                           | otherwise = Just y
maybeMax Nothing  (Just y) = Just y
maybeMax (Just x) Nothing  = Just x
maybeMax Nothing  Nothing  = Nothing

maybeListMax :: Ord a => [Maybe a] -> Maybe a
maybeListMax (x:xs) = maybeMax x (maybeListMax xs)
maybeListMax _      = Nothing

makeBusPairs :: [Maybe Integer] -> Integer -> [(Integer,Integer)]
makeBusPairs (x:xs) i = case x of
                           Nothing -> makeBusPairs xs (i+1)
                           Just v  -> (v,i):(makeBusPairs xs (i+1))
makeBusPairs _      _ = []

getBusProd :: [(Integer,Integer)] -> Integer
getBusProd ((id,_):buses) = id * (getBusProd buses)
getBusProd _              = 1

solve :: [Maybe Integer] -> Integer
solve buses =  let maxBus = fromJust (maybeListMax buses) in
               let maxIndex = toInteger (fromJust (elemIndex (Just maxBus) buses)) in
               let busOrder = makeBusPairs buses 0 in
               let fullProd = getBusProd busOrder in
               let start = suggestedStart - (mod suggestedStart maxBus) - maxIndex in
                  --start
                  run start fullProd busOrder
                  
run :: Integer -> Integer -> [(Integer,Integer)] -> Integer
run start fullProd buses = let prodOfValid = checkTimestamp (myTrace ("checking " ++ (show start)) start) buses in
                           if (prodOfValid == fullProd)
                              then start
                           else 
                              run (start + prodOfValid) fullProd buses
                              
checkTimestamp :: Integer -> [(Integer,Integer)] -> Integer
checkTimestamp time ((id,index):buses) | ((mod (time + index) id) == 0) = id * (checkTimestamp time buses)
                                       | otherwise                      = checkTimestamp time buses
checkTimestamp _    _                  = 1