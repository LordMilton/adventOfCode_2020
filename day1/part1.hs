import Prelude

myRead :: IO [Int]
myRead = do
   line <- getLine
   if (line == "q")
      then return []
   else
      do
         rest <- myRead
         return ((read line :: Int):rest)

main :: IO Int
main = do
   input <- myRead
   return (solve input)
   
solve :: [Int] -> Int
solve (x:xs)   = max (solveForHead x xs) (solve xs)
solve _        = (negate 1)

solveForHead :: Int -> [Int] -> Int
solveForHead y (x:xs)   | y + x == 2020   = y*x
                        | otherwise       = solveForHead y xs
solveForHead y _      = (negate 1)