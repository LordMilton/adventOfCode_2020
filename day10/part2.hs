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
      return (fst (solve (sort altIn) (repeat (-1))))

-- The ans parameter is for memoization so that our solution takes .5 seconds instead of more than 5 minutes
-- It maintains our solutions starting from each adapter so that we don't have to keep redoing all the combinations afterwards
-- Time efficiencty goes from O(n!) -> ~O(n) but is significantly less space efficient
solve :: [Integer] -> [Integer] -> (Integer,[Integer])
solve (x:a:b:c:xs) ans = let solA = getNumCombos ans x (a:b:c:xs) in
                         let solB = getNumCombos (snd solA) x (b:c:xs) in
                         let solC = getNumCombos (snd solB) x (c:xs) in
                         (((fst solA) + (fst solB) + (fst solC)),(snd solC))
solve (x:b:c:xs)   ans = let solB = getNumCombos ans x (b:c:xs) in
                         let solC = getNumCombos (snd solB) x (c:xs) in
                         (((fst solB) + (fst solC)),(snd solC))
solve (x:c:xs)     ans = let solC = getNumCombos ans x (c:xs) in
                         ((fst solC),(snd solC))
solve _            ans = (1,ans)

getNumCombos :: [Integer] -> Integer -> [Integer] -> (Integer,[Integer])
getNumCombos ans last (x:xs) | (myTrace ("testing " ++ ((show x) ++ " when last is " ++ (show last))) x) - last <= 3 =
                                    if ((ans !! (fromInteger x)) == -1) 
                                       then let solved = (solve (x:xs) ans) in
                                            let sol = fst solved in
                                            let newAns = snd solved in
                                                (sol, (setAt newAns (fromInteger x) sol))
                                    else
                                       ((ans !! (fromInteger x)),ans)
                             | otherwise     = (0,ans)
                             
--pulled from Data.List.Tools                             
setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs