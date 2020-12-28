import Prelude
import Data.List
import Control.Monad.State
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

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
   
{-    Clockwise is <- thata way for some reason
      1,2
      -+---
      ----+ -2,1
      --S--
 2,-1 +----
      ---+-
        -1,-2
-}
   
turnRight :: (Int,Int) -> Int -> (Int,Int)
turnRight wayPoint          0   = myTrace ("Waypoint rotated to: " ++ (show (fst wayPoint)) ++ "," ++ (show (snd wayPoint))) wayPoint
turnRight (wayPosE,wayPosN) deg =
   turnRight (wayPosN, negate wayPosE) (deg - 90)

turnLeft :: (Int,Int) -> Int -> (Int,Int)
turnLeft wayPoint          0   = myTrace ("Waypoint rotated to: " ++ (show (fst wayPoint)) ++ "," ++ (show (snd wayPoint))) wayPoint
turnLeft (wayPosE,wayPosN) deg =
   turnLeft (negate wayPosN, wayPosE) (deg - 90)

solve :: [(Char,Int)] -> Int
solve dirs =   let info = execState (getLoc dirs) ((10,1),(0,0)) in
               let loc = snd info in
                  (abs (fst loc)) + (abs (snd loc))

getLoc :: [(Char,Int)] -> State ((Int,Int),(Int,Int)) ()
getLoc ((dir,dist):xs) = do
   ((wayPosE,wayPosN),(shipPosE,shipPosN)) <- get
   if (dir == 'N')
      then  do put ((wayPosE, wayPosN + dist),(shipPosE,shipPosN))
               getLoc xs
   else if (dir == 'E')
      then  do put ((wayPosE + dist, wayPosN),(shipPosE,shipPosN))
               getLoc xs
   else if (dir == 'S')
      then  do put ((wayPosE, wayPosN - dist),(shipPosE,shipPosN))
               getLoc xs
   else if (dir == 'W')
      then  do put ((wayPosE - dist, wayPosN),(shipPosE,shipPosN))
               getLoc xs
   else if (dir == 'R')
      then  let newWayPos = turnRight (wayPosE,wayPosN) dist in
               do put ((fst newWayPos, snd newWayPos),(shipPosE,shipPosN))
                  getLoc xs
   else if (dir == 'L')
      then  let newWayPos = turnLeft (wayPosE,wayPosN) dist in
               do put ((fst newWayPos, snd newWayPos),(shipPosE,shipPosN))
                  getLoc xs
   else if (dir == 'F')
      then  let newShipPos = move dist (wayPosE,wayPosN) (shipPosE,shipPosN)  in
               do put ((wayPosE,wayPosN),(fst newShipPos, snd newShipPos))
                  getLoc xs
   else
      undefined
getLoc _               = return ()
   
move :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
move dist (wayPosE,wayPosN) (shipPosE,shipPosN) =  myTrace ("Ship moved to: " ++ (show (shipPosE + (wayPosE * dist))) ++ "," ++ (show (shipPosN + (wayPosN * dist)))) (shipPosE + (wayPosE * dist), shipPosN + (wayPosN * dist))
   