import Prelude
import Data.List
import Control.Monad.State
import Debug.Trace

myTrace :: String -> a -> a
myTrace str val = trace str val
--myTrace str val = val

myRead :: IO [String]
myRead = do
   line <- getLine
   if(line == "q")
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
solve map = let runMap = evalState (runTillStatic) map in
               countChars2D '#' runMap
               
countChars2D :: Char -> [String] -> Int
countChars2D c (row:map) = (countChars1D c row) + (countChars2D c map)
countChars2D _ _         = 0

countChars1D :: Char -> String -> Int
countChars1D c (x:xs)    | x == c      = 1 + (countChars1D c xs)
                         | otherwise   = countChars1D c xs
countChars1D _ _         = 0

runTillStatic :: State [String] [String]
runTillStatic = do
   latest <- get
   let nextRound = runRound (latest) (latest) 0 in
      if (nextRound == (latest))
         then return nextRound
      else
         do
            put nextRound
            myTrace (mapToString nextRound) runTillStatic

mapToString :: [String] -> String
mapToString (x:xs) = x ++ "\n" ++ (mapToString xs)
mapToString _      = ""

runRound :: [String] -> [String] -> Int -> [String]
runRound fullMap (row:map) rowNum  = (runRoundRow fullMap row rowNum 0):(runRound fullMap map (rowNum + 1))
runRound _       _         _       = []

runRoundRow :: [String] -> String -> Int -> Int -> String
runRoundRow fullMap (spot:row) rowNum colNum =  let rest = runRoundRow fullMap row rowNum (colNum + 1) in
                                                if (spot /= '.')
                                                   then
                                                   let filledAdj = countFilledAdj fullMap rowNum colNum in
                                                      if (filledAdj == 0)
                                                         then '#':rest
                                                      else if (filledAdj >= 5)
                                                         then 'L':rest
                                                      else
                                                         spot:rest
                                                else
                                                   spot:rest
runRoundRow _       _          _      _      = ""

countFilledAdj :: [String] -> Int -> Int -> Int
countFilledAdj map rowNum colNum = let adjs =  (findSeatInView map (rowNum-1) (colNum-1) (-1) (-1)):
                                              ((findSeatInView map (rowNum-1) (colNum  ) (-1)   0):
                                              ((findSeatInView map (rowNum-1) (colNum+1) (-1)   1):
                                              ((findSeatInView map (rowNum  ) (colNum-1)   0  (-1)):
                                              ((findSeatInView map (rowNum  ) (colNum+1)   0    1):
                                              ((findSeatInView map (rowNum+1) (colNum-1)   1  (-1)):
                                              ((findSeatInView map (rowNum+1) (colNum  )   1    0):
                                              ((findSeatInView map (rowNum+1) (colNum+1)   1    1):""))))))) in
                                       countChars1D '#' adjs

findSeatInView :: [String] -> Int -> Int -> Int -> Int -> Char
findSeatInView map row col rowD colD | row >= 0 && row < (length map) &&
                                       col >= 0 && col < (length (map !! 0)) =
                                          let seen = ((map !! row) !! col) in
                                             if (seen == '.')
                                                then findSeatInView map (row + rowD) (col + colD) rowD colD
                                             else
                                                seen
                                     | otherwise = '.'