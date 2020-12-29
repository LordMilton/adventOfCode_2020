import Prelude
import Data.List
import Data.Maybe
import Data.List.Split
import Debug.Trace

firstBusNum = 17
--suggestedStart = 1000000000
suggestedStart = 100000000000000

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

makeBusPairs :: [Maybe Integer] -> Integer -> ([Integer],[Integer])
makeBusPairs (x:xs) i = let rest = makeBusPairs xs (i+1) in
                           case x of
                              Nothing -> rest
                              Just v  -> (v:(fst rest), i:(snd rest))
makeBusPairs _      _ = ([],[])

powerProducts :: [Integer] -> [Integer] -> [Integer]
powerProducts nums (x:xs) = (powerProduct nums x):(powerProducts nums xs)
powerProducts _    _      = []

powerProduct :: [Integer] -> Integer -> Integer
powerProduct (x:xs) num |  x /= num = x * (powerProduct xs num)
                        |  otherwise= powerProduct xs num
powerProduct _      _   =  1

modInverses :: [Integer] -> [Integer] -> [Integer]
modInverses (num:nums) (pProd:pProds) = (modInverse num pProd 1):(modInverses nums pProds)
modInverses _          _              = []

modInverse :: Integer -> Integer -> Integer -> Integer
modInverse num pProd potInv | (mod (pProd * potInv) num) == 1  = potInv
                            | otherwise                        = modInverse num pProd (potInv + 1)

solve :: [Maybe Integer] -> Integer
solve buses =  let busOrder = myTrace (show (makeBusPairs buses 0)) (makeBusPairs buses 0) in
               let nums = fst busOrder in
               let rems = snd busOrder in
               let prod = foldl (*) 1 nums in
               let powerProds = powerProducts nums nums in
               let modInvs = modInverses nums powerProds in
                  mod (combine rems powerProds modInvs) prod

combine :: [Integer] -> [Integer] -> [Integer] -> Integer
combine (rem:rems) (pProd:pProds) (inv:modInvs) = (rem * pProd * inv) + (combine rems pProds modInvs)
combine _          _              _             = 0