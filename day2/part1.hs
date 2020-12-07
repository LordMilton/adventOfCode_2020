import Prelude
import Data.List.Split

tupleMin :: (Int,Int,Char,String) -> Int
tupleMin (min,_,_,_) = min

tupleMax :: (Int,Int,Char,String) -> Int
tupleMax (_,max,_,_) = max

tupleChar :: (Int,Int,Char,String) -> Char
tupleChar (_,_,char,_) = char

tuplePass :: (Int,Int,Char,String) -> String
tuplePass (_,_,_,pass) = pass

myRead :: IO [(Int,Int,Char,String)]
myRead = do
   line <- getLine
   if (line == "q")
      then return []
   else
         let allByDash = splitOn "-" line in
         let rightDashBySpace = splitOn " " (allByDash !! 1) in
         do
            rest <- myRead
            return (((read (allByDash !! 0) :: Int),
                      (read (rightDashBySpace !! 0) :: Int),
                      ((rightDashBySpace !! 1) !! 0),
                      (rightDashBySpace !! 2)):rest)

main :: IO Int
main = do
   input <- myRead
   return (solve input)
   
charCount :: String -> Char -> Int
charCount (x:xs) c   | c == x    = 1 + (charCount xs c)
                     | otherwise = charCount xs c
charCount _      c   = 0
                     
solve :: [(Int,Int,Char,String)] -> Int
solve (x:xs)   =  let count = charCount (tuplePass x) (tupleChar x) in
                     if(count >= (tupleMin x) && count <= (tupleMax x))
                        then 1 + (solve xs)
                     else
                        solve xs
solve _        =  0