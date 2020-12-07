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
   
passValid :: String -> Char -> Int -> Int -> Bool
passValid xs c lowP highP  = ((c /= (xs !! (highP-1))) && (c == (xs !! (lowP-1)))) || ((c == (xs !! (highP-1))) && (c /= (xs !! (lowP-1))))
                     
solve :: [(Int,Int,Char,String)] -> Int
solve (x:xs)   =  let valid = passValid (tuplePass x) (tupleChar x) (tupleMin x) (tupleMax x) in
                     if(valid)
                        then 1 + (solve xs)
                     else
                        solve xs
solve _        =  0