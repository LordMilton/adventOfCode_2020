import Prelude
import Control.Monad.State
import Data.List
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

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
solve (x:xs)   = max (determineID (determineSeat x)) (solve xs)
solve _        = -1

determineCoord :: String -> Char -> Char -> State (Int,Int) Int
determineCoord (x:xs) highChar lowChar = 
   do
   (min,max) <- get
   if(min == max)
      then return (myTrace (show min) min)
   else
      if(x == highChar)
         then do
            put((div (max + min) 2) + 1, max)
            rest <- determineCoord xs highChar lowChar
            return(rest)
      else
         do
         put(min, (div (min + max) 2))
         rest <- determineCoord xs highChar lowChar
         return(rest)
determineCoord _       _       _       = do
   (min,max) <- get
   return (myTrace (show min) min)

determineRow :: String -> Int
determineRow str = myTrace ("row: ") (evalState (determineCoord str 'B' 'F') (0,127))

determineCol :: String -> Int
determineCol str = myTrace ("col: ") (evalState (determineCoord str 'R' 'L') (0,7))
   

determineSeat :: String -> (Int,Int)
determineSeat str =  let split = splitAt ((length str) - 3) str in
                     ((determineRow (fst split)),
                       determineCol (snd split))

determineID :: (Int,Int) -> Int
determineID (row,col) = (row * 8) + col