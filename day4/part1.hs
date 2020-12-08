import Prelude
import Data.List.Split
import Data.List

fields = ["ecl","pid","eyr","hcl","byr","iyr","hgt"]

breakLine :: [String] -> [(String,String)]
breakLine (x:xs)  =  let split = splitOn ":" x in
                     (split !! 0, split !! 1):(breakLine xs)
breakLine _       =  []

readPassport :: IO [(String,String)]
readPassport = do
   line <- getLine
   if (line == "")
      then return []
   else
      let splitOnSpace = splitOn " " line in
      let broken = breakLine splitOnSpace in
      do
         rest <- readPassport
         return (broken ++ rest)

myRead :: IO [[(String,String)]]
myRead = do
   passport <- readPassport
   if((length passport) == 0)
      then return (passport:[])
   else
      do
         rest <- myRead
         return (passport:(rest))

main :: IO Int
main = do
   input <- myRead
   return (solve fields input)
   
checkValid :: [String] -> [(String,String)] -> Bool
checkValid remFields (x:xs)   = checkValid (delete (fst x) remFields) xs
checkValid []        _        = True
checkValid remFields []       = False

solve :: [String] -> [[(String,String)]] -> Int
solve fields (x:xs)  | checkValid fields x   = 1 + (solve fields xs)
                     | otherwise             = solve fields xs
solve fields _       = 0