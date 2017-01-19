module Main where
import Data.List (sort)

processInput :: String->[[Int]]
processInput = (map ((map read).words)).lines

isPossible t = x+y>z where
  (x:y:z:[]) = sort t

main = do
  i <- getContents
  print $ length $ filter isPossible $ processInput i
