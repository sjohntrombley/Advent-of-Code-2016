module Main where

import  System.Environment  (getArgs,getProgName)
import  Data.List           (sort)

main = do
  args    <- getArgs
  pn      <- getProgName
  let   (rm,ifn) = 
          if length args >= 2 then (take 3 $ args!!0,args!!1) 
          else error $ pn ++ " takes two arguments.\n"
  ifs     <- readFile ifn
  let     pif = processInput ifs
  if rm !!1 == '1' 
    then putStrLn $ ("Part 1:\t"++) $ show $ length $ filter isPossible pif
    else return ()
  if last rm == '2' 
    then putStrLn $ ("Part 2:\t"++) $ show $ length $ filter isPossible 
      $ groupIn3s $ flattenInput pif
    else return ()
  
processInput :: String->[[Int]]
processInput = (map ((map read).words)).lines

isPossible :: [Int]->Bool
isPossible t = if length t /= 3 then error "A triangle has 3 sides.\n"
  else let (x:y:z:[]) = sort t in x+y>z

flattenInput il = let (a,b,c) = rfi ([],[],[]) il 
  in (reverse a)++(reverse b)++(reverse c)

rfi f [] = f
rfi (f1,f2,f3) ((x1:x2:x3:[]):xs) = rfi (x1:f1,x2:f2,x3:f3) xsgroupIn3s [] = []

groupIn3s (x:y:z:xs) = [x,y,z] : groupIn3s xs
groupIn3s _ = error "Length of list is not congruent to 0 (mod 3)."

