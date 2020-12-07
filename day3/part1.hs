import Prelude
import Control.Monad.State

myRead :: IO [String]
myRead = do
   line <- getLine
   if (line == "q")
      then return []
   else
      do
         rest <- myRead
         return (line:rest)

main :: IO Int
main = do
   input <- myRead
   return (solve input)

solve :: [String] -> Int
solve map = evalState (solveHelp map) (0,0)

solveHelp :: [String] -> State (Int,Int) Int
solveHelp map = do
   (x,y) <- get
   if(y >= (length map))
      then return 0
   else
      if((map !! y) !! x == '#')
         then  do
            put ((x + 3) `mod` (length (head map)), y + 1)
            rest <- solveHelp map
            return (1 + rest)
      else
         do
         put ((x + 3) `mod` (length (head map)), y + 1)
         rest <- solveHelp map
         return (rest)