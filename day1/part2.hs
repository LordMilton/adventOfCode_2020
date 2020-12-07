import Prelude
import Control.Monad.List

myRead :: IO [Int]
myRead = do
   line <- getLine
   if (line == "q")
      then return []
   else
      do
         rest <- myRead
         return ((read line :: Int):rest)

main :: IO [Int]
main = do
   input <- myRead
   return (solve input)
   
solve :: [Int] -> [Int]
solve vals = let len = length vals in
   do
      x <- [0..(len-1)]
      y <- [x..(len-1)]
      z <- [y..(len-1)]
      guard $ (((vals !! x) + (vals !! y) + (vals !! z)) == 2020)
      return ((vals !! x) * (vals !! y) * (vals !! z))