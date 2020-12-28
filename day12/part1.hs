import Prelude
import Data.List
import Control.Monad.State
import Debug.Trace

myTrace :: String -> a -> a
myTrace str val = trace str val
--myTrace str val = val

myRead :: IO [(Char,Int)]
myRead = do
   line <- getLine
   if(line == "q")
      then return []
   else
      do
         rest <- myRead
         return ((head line, read (tail line) :: Int):rest)

main :: IO Int
main = do
   input <- myRead
   return (solve input)
   
data Direction = N | E | S | W deriving Show

turnRight :: Direction -> Int -> Direction
turnRight dir 0 = dir
turnRight N deg = turnRight E (deg - 90)
turnRight E deg = turnRight S (deg - 90)
turnRight S deg = turnRight W (deg - 90)
turnRight W deg = turnRight N (deg - 90)

turnLeft :: Direction -> Int -> Direction
turnLeft dir 0 = dir
turnLeft N deg = turnLeft W (deg - 90)
turnLeft E deg = turnLeft N (deg - 90)
turnLeft S deg = turnLeft E (deg - 90)
turnLeft W deg = turnLeft S (deg - 90)

solve :: [(Char,Int)] -> Int
solve dirs =   let loc = evalState (getLoc dirs) (E,0,0) in
                  (abs (fst loc)) + (abs (snd loc))

getLoc :: [(Char,Int)] -> State (Direction,Int,Int) (Int,Int)
getLoc ((dir,dist):xs) = do
   (heading,ePos,nPos) <- get
   if (dir == 'N')
      then  let newPos = move N dist (ePos,nPos) in
               do put (heading,(fst newPos),(snd newPos))
                  getLoc xs
   else if (dir == 'E')
      then  let newPos = move E dist (ePos,nPos) in
               do put (heading,(fst newPos),(snd newPos))
                  getLoc xs
   else if (dir == 'S')
      then  let newPos = move S dist (ePos,nPos) in
               do put (heading,(fst newPos),(snd newPos))
                  getLoc xs
   else if (dir == 'W')
      then  let newPos = move W dist (ePos,nPos) in
               do put (heading,(fst newPos),(snd newPos))
                  getLoc xs
   else if (dir == 'R')
      then  let newHeading = turnRight heading dist in
               do put (newHeading,ePos,nPos)
                  getLoc xs
   else if (dir == 'L')
      then  let newHeading = turnLeft heading dist in
               do put (newHeading,ePos,nPos)
                  getLoc xs
   else if (dir == 'F')
      then  let newPos = move heading dist (ePos,nPos) in
               do put (heading,(fst newPos),(snd newPos))
                  getLoc xs
   else
      undefined
getLoc _               = do
   (heading,ePos,nPos) <- get
   return (ePos,nPos)
   
move :: Direction -> Int -> (Int,Int) -> (Int,Int)
move dir dist (e,n) =   case dir of
                           N -> (e,n+dist)
                           E -> (e+dist,n)
                           S -> (e,n-dist)
                           W -> (e-dist,n)
   