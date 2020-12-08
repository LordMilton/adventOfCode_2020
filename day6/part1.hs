import Prelude
import Data.List
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

readGroup :: IO String
readGroup = do
   line <- getLine
   if (line == "")
      then return []
   else
      do
         rest <- readGroup
         return (line ++ rest)

myRead :: IO [String]
myRead = do
   group <- readGroup
   if((length group) == 0)
      then return (group:[])
   else
      do
         rest <- myRead
         return (group:(rest))

main :: IO Int
main = do
   input <- myRead
   return (solve input)
   
solve :: [String] -> Int
solve (x:xs) = (numAnswersForGroup x) + (solve xs)
solve _      = 0

numAnswersForGroup :: String -> Int
numAnswersForGroup ans = length (nub ans)