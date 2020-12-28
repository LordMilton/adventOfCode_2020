import Prelude
import Data.List
import Data.List.Split
import Debug.Trace

firstBusNum = 17

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

myRead :: IO (Integer,[String])
myRead = do
   time <- getLine
   buses <- getLine
   let parsedBuses = splitOn "," buses in
      return ((read time) :: Integer, parsedBuses)

main :: IO Integer
main = do
   input <- myRead
   return (solve (fst input) (snd input))
   
solve :: Integer -> [String] -> Integer
solve earliest buses = let soonestInfo = getSoonest earliest (earliest + firstBusNum - 1, firstBusNum) buses in
                           (((fst soonestInfo) - earliest) * (snd soonestInfo))

getSoonest :: Integer -> (Integer,Integer) -> [String] -> (Integer,Integer)
getSoonest earliest (soonest,busId) (bus:buses) | bus == "x" =
   getSoonest earliest (soonest,busId) (buses)
                                                | otherwise  =
   let busInt = (read bus) :: Integer in
      if ((mod earliest busInt) == 0)
         then (earliest,busInt)
      else
         let curBusSoonest = earliest + (busInt - (mod earliest busInt)) in
            if (myTrace ("bus " ++ (show busInt) ++ " will arrive next at " ++ (show curBusSoonest)) (curBusSoonest < soonest)) 
               then getSoonest earliest (curBusSoonest,busInt) buses
            else
               getSoonest earliest (soonest,busId) buses
getSoonest _        (soonest,busId) _           = (soonest, busId)