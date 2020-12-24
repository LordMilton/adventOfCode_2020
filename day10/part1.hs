import Prelude
import Data.List
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

myRead :: IO [Integer]
myRead = do
   line <- getLine
   if(line == "q")
      then return []
   else
      do
         rest <- myRead
         return (((read line) :: Integer):rest)

main :: IO Integer
main = do
   input <- myRead
   let altIn = 0:((maximum input) + 3):input in
      return (solve (sort altIn))

solve :: [Integer] -> Integer
solve xs = let diffs = getDiffs xs in
               (fst (fst diffs)) * (snd diffs)
               
getDiffs :: [Integer] -> ((Integer, Integer), Integer)
getDiffs (x:y:xs) = let rest = getDiffs (y:xs) in
                        if (y - x == 1)
                           then (((1 + (fst (fst rest))), (snd (fst rest))), (snd rest))
                        else if (y - x == 2)
                           then (((fst (fst rest)), (1 + (snd (fst rest)))), (snd rest))
                        else if (y - x == 3)
                           then (((fst (fst rest)), (snd (fst rest))), (1 + (snd rest)))
                        else 
                           undefined
getDiffs _        = ((0,0),0)