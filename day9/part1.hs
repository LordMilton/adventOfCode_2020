import Prelude
import Data.List
import Debug.Trace

preambleLength = 25

myTrace :: String -> a -> a
--myTrace str val = trace str val
myTrace str val = val

myRead :: IO [Integer]
myRead = do
   line <- getLine
   if(line == "q")
      then return []
   else
      do
         rest <- myRead
         return (((read line) :: Integer):rest)

main :: IO Integer
main = do
   input <- myRead
   let broken = breakPreamble preambleLength input in 
      return (solve (fst broken) (snd broken))

breakPreamble :: Int -> [Integer] -> ([Integer],[Integer])
breakPreamble 0      list     =  ([],list)
breakPreamble preLen (x:list) =  let rest = breakPreamble (preLen - 1) (list) in
                                    (x:(fst rest), (snd rest))
breakPreamble _      _        =  undefined

combinePreamble :: [Integer] -> [Integer]
combinePreamble (x:xs)  = (map (+ x) xs) ++ (combinePreamble xs)
combinePreamble _       = []

adjustPreamble :: [Integer] -> Integer -> [Integer]
adjustPreamble (x:xs) newVal = xs ++ (newVal:[])

solve :: [Integer] -> [Integer] -> Integer
solve preamble (x:list) = let combos = combinePreamble preamble in
                              if (elem x combos)
                                 then solve (adjustPreamble preamble x) list
                              else
                                 x