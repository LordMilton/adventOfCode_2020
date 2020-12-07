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
solve map = (evalState (solveHelp map 1 1) (0,0)) *
            (evalState (solveHelp map 3 1) (0,0)) *
            (evalState (solveHelp map 5 1) (0,0)) *
            (evalState (solveHelp map 7 1) (0,0)) *
            (evalState (solveHelp map 1 2) (0,0))

solveHelp :: [String] -> Int -> Int -> State (Int,Int) Int
solveHelp map dx dy = do
   (x,y) <- get
   if(y >= (length map))
      then return 0
   else
      if((map !! y) !! x == '#')
         then  do
            put ((x + dx) `mod` (length (head map)), y + dy)
            rest <- solveHelp map dx dy
            return (1 + rest)
      else
         do
         put ((x + dx) `mod` (length (head map)), y + dy)
         rest <- solveHelp map dx dy
         return (rest)