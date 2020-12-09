import Prelude
-- Can't seem to import this at same time as Data.List.Split
--import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

data Instruction = Inst Int (String,Int) deriving Show

data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree ins L = (V ins)
insertTree ins (V v) |  ins <= v    = (Br (V ins) v L)
                     |  otherwise   = (Br L v (V ins)) 
insertTree ins (Br l v r)  |  ins <= v    =  Br (insertTree ins l) v r
                           |  otherwise   =  Br l v (insertTree ins r)
                           
searchSortedTree :: (Ord a) => a -> Tree a -> Maybe a
searchSortedTree find L = Nothing
searchSortedTree find (V v) |  find == v = Just v
                            |  otherwise = Nothing
searchSortedTree find (Br l v r) |  find == v   = Just v
                                 |  find <= v   = searchSortedTree find l
                                 |  otherwise   = searchSortedTree find r

readWithSign :: String -> Int
readWithSign str | str !! 0 == '+'  = (read (tail str)) :: Int
                 | otherwise        = (read str) :: Int

myRead :: Int -> (Map.Map Int Instruction) -> IO (Map.Map Int Instruction)
myRead numInst map = do
   line <- getLine
   if (line == "q")
      then return map
   else
      let split = splitOn " " line in
      let inst = (Inst numInst (split !! 0, (readWithSign (split !! 1)))) in
      let newMap = Map.insert numInst inst map in
         do
            rest <- myRead (numInst + 1) newMap
            return rest

main :: IO Int
main = do
   input <- myRead 0 (Map.empty)
   return (solve (Map.toList input) input)

solve :: [(Int,Instruction)] -> (Map.Map Int Instruction) -> Int
solve ((key,inst):xs) map = 
            let updMap = Map.adjust changeInst key map in
            let ans = checkTermination (L) (updMap) 0 0 in
               case ans of
                  Nothing -> solve xs map
                  Just v  -> v

changeInst :: Instruction -> Instruction
changeInst (Inst instNum (name,num))   | name == "nop" = Inst instNum ("jmp",num)
                                       | name == "jmp" = Inst instNum ("nop",num)
                                       | otherwise     = Inst instNum (name,num)

checkTermination :: Tree Int -> (Map.Map Int Instruction) -> Int -> Int -> Maybe Int
checkTermination pastInstTree map instNum acc =
   let pastInst = searchSortedTree instNum pastInstTree in
   let curInst = (Map.lookup instNum map) in
      case pastInst of
         Just v  -> Nothing
         Nothing -> 
            (let updTree = (insertTree instNum pastInstTree) in
            case curInst of
               Nothing ->  let maxInst = (fst (Map.findMax map)) in
                              if(instNum == (maxInst + 1))
                                 then Just acc
                              else
                                 Nothing
               Just v  ->  let updatedVals = runInst v acc in
                              checkTermination updTree map (fst updatedVals) (snd updatedVals) )
                  
runInst :: Instruction -> Int -> (Int,Int)
runInst (Inst instNum (name, num)) acc  | name == "nop"  = myTrace ("Running instruction: " ++ (show instNum)) (instNum + 1, acc)
                                        | name == "acc"  = myTrace ("Running instruction: " ++ (show instNum)) (instNum + 1, acc + num)
                                        | name == "jmp"  = myTrace ("Running instruction: " ++ (show instNum)) (instNum + num, acc)
                                        | otherwise      = undefined