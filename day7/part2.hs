import Prelude
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace

myTrace :: String -> a -> a
myTrace str val = trace str val
--myTrace str val = val

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
   input <- (myRead L)
   let shinyGold = (searchSortedTree (FullBag "shiny gold" []) input) in
      case shinyGold of
         Nothing  -> undefined
         Just v   -> return ((numContained v input) - 1)

numContained :: Bag -> Tree Bag -> Int
numContained (FullBag name cont) tree = 1 + (checkContents cont tree)

                                             
checkContents :: [(Int,String)] -> Tree Bag -> Int
checkContents ((num,name):xs) tree  =  let bag = searchSortedTree (FullBag name []) tree in
                                       case bag of
                                          Nothing -> undefined
                                          Just v  -> myTrace   ("checking contents of " ++ name ++ " bag") 
                                                               ((num * (numContained v tree)) +
                                                               (checkContents xs tree))
checkContents _               tree  =  0