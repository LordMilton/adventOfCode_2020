import Prelude
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

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
                                 
inOrder :: Tree a -> [a]
inOrder L = []
inOrder (V v1) = v1:[]
inOrder (Br l v r) = (inOrder l) ++ (v:[]) ++ (inOrder r)

data Bag = FullBag String [(Int,String)] deriving Show

instance Eq Bag where
   (==) (FullBag name1 _) (FullBag name2 _) = name1 == name2
   
instance Ord Bag where
   (<=) (FullBag name1 _) (FullBag name2 _) = name1 <= name2

parseContents :: [String] -> [(Int,String)]
parseContents []     = []
parseContents cont   |  (cont !! 0) == "no" = []
                     |  otherwise =
                           let num = ((read (cont !! 0)) :: Int) in
                           let name = (cont !! 1) ++ " " ++ (cont !! 2) in
                           (num,name):(parseContents (snd (splitAt 4 cont)))

parseLine :: String -> Bag
parseLine line =  let split = splitOn " " line in
                  let name = (split !! 0) ++ " " ++ (split !! 1) in
                  FullBag name (parseContents (snd (splitAt 4 split)))

myRead :: Tree Bag -> IO (Tree Bag)
myRead tree = do
   line <- getLine
   if (line == "q")
      then return tree
   else
      do
         rest <- (myRead (insertTree (parseLine line) tree))
         return rest

main :: IO Int
main = do
   input <- myRead L
   return (solve (inOrder input) input)

solve :: [Bag] -> Tree Bag -> Int
solve (x:xs) tree | containsShinyGold x tree = 1 + (solve xs tree)
                  | otherwise                = solve xs tree
solve _      tree = -1 --We know there will only be one shiny gold bag, but we don't want to count that one

containsShinyGold :: Bag -> Tree Bag -> Bool
containsShinyGold (FullBag name cont) tree   | name == "shiny gold" = True
                                             | otherwise            = checkContents cont tree
                                             
checkContents :: [(Int,String)] -> Tree Bag -> Bool
checkContents ((_,x):xs) tree  =  let bag = searchSortedTree (FullBag x []) tree in
                                  case bag of
                                    Nothing -> undefined
                                    Just v  -> ((containsShinyGold v tree) ||
                                                (checkContents xs tree))
checkContents _          tree  =  False