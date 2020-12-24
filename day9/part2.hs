import Prelude
import Data.List
import Debug.Trace

weakness = 14360655
--weakness = 127

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
   return (solve input)

solve :: [Integer] -> Integer
solve list = let cont = getCombo list in
               (minimum cont) + (maximum cont)

getCombo :: [Integer] -> [Integer]
getCombo (x:xs) = let combo = checkComboWithX (x:[]) (xs) in
                     if ((length combo) >= 2)
                        then combo
                     else
                        getCombo xs
getCombo _      = undefined

checkComboWithX :: [Integer] -> [Integer] -> [Integer]
checkComboWithX xStart (y:ys) = let combined = foldl (+) 0 xStart in
                                    if (combined == weakness)
                                       then xStart
                                    else if (combined > weakness)
                                       then []
                                    else
                                       checkComboWithX (y:xStart) ys