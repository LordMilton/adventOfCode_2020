import Prelude
import Data.List.Split
import Data.List
import Debug.Trace

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

checkECL :: String -> Bool
checkECL val = elem val ["amb","blu","brn","gry","grn","hzl","oth"]

checkPID :: String -> Bool
checkPID val = (length val) == 9

checkEYR :: String -> Bool
checkEYR val = let year = ((read val) :: Int) in
               year <= 2030 && year >= 2020

myElem :: String -> Char -> Bool
myElem str c = elem c str

checkHex :: String -> Bool
checkHex str = let hexOnly = filter (myElem "0123456789abcdef") str in
               (length hexOnly) == 6
               
checkHCL :: String -> Bool
checkHCL ('#':hex)   | (length hex) == 6  = checkHex hex
                     | otherwise          = False
checkHCL _           = False

checkBYR :: String -> Bool
checkBYR val = let year = ((read val) :: Int) in
               year <= 2002 && year >= 1920

checkIYR :: String -> Bool
checkIYR val = let year = ((read val) :: Int) in
               year <= 2020 && year >= 2010

checkHGT :: String -> Bool
checkHGT val = let split = splitAt ((length val) - 2) val in
               if((snd split) == "in")
                  then  let height = read (fst split) in
                        (height >= 59 && height <= 76)
               else if((snd split) == "cm")
                  then  let height = read (fst split) in
                        (height >= 150 && height <= 193)
               else
                  False

checkFieldValid :: (String,String) -> Bool
checkFieldValid ("ecl",val)   = checkECL val
checkFieldValid ("pid",val)   = checkPID val
checkFieldValid ("eyr",val)   = checkEYR val
checkFieldValid ("hcl",val)   = checkHCL val
checkFieldValid ("byr",val)   = checkBYR val
checkFieldValid ("iyr",val)   = checkIYR val
checkFieldValid ("hgt",val)   = checkHGT val
checkFieldValid (_,val)       = True
   
checkValid :: [String] -> [(String,String)] -> Bool
checkValid remFields (x:xs)   = (checkFieldValid x) && (checkValid (delete (fst x) remFields) xs)
checkValid []        _        = True
checkValid remFields []       = False

solve :: [String] -> [[(String,String)]] -> Int
solve fields (x:xs)  | checkValid fields x   = 1 + (solve fields xs)
                     | otherwise             = solve fields xs
solve fields _       = 0