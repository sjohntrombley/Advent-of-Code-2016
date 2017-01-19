module Main where
import Data.List (sort)

processInput :: String->[[Int]]
processInput = (map ((map read).words)).lines

flattenInput il = (reverse a)++(reverse b)++(reverse c) where
  rfi f [] = f
  rfi (f1,f2,f3) ((x1:x2:x3:[]):xs) = rfi (x1:f1,x2:f2,x3:f3) xs
  (a,b,c) = rfi ([],[],[]) il

groupIn3s [] = []
groupIn3s (x:y:z:xs) = [x,y,z] : groupIn3s xs
groupIn3s _ = error "Length of list is not congruent to 0 (mod 3)."

isPossible t = x+y>z where
  (x:y:z:[]) = sort t

main = do
  i <- getContents
  print $ length $ filter isPossible $ groupIn3s $ flattenInput $ 
    processInput i
