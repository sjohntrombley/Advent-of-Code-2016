module Main where

import  System.Environment          (getArgs, getProgName)
import  Prelude             hiding  (Left, Right)
import  Data.Maybe                  (isJust, fromJust)

data Direction = Left Int | Right Int           deriving Show
data Orientation = North | East | South | West  deriving Show
data Position = Position Orientation (Int,Int)  deriving Show

-- The program takes two arguments. The first is the run mode, which is either
-- -1, -2, or -12, signifying which part of the problem is being solved. The
-- second argument is the name of the file with the directions in it.
main :: IO ()
main = do
  args  <- getArgs
  pn    <- getProgName
  let (rm,fn) = if length args /= 2
        then error $ pn ++ " takes exactly 2 arguments.\n"
        else (args !! 0, args !! 1)
  fc  <- readFile fn
  let directions = parse fc
  if rm=="-1"
    then putStrLn $ (++) "Part 1:\t" $ show $ solve1 directions
    else if rm=="-2"
    then putStrLn $ (++) "Part 2:\t" $ show $ solve2 directions
    else if rm=="-12"
    then putStrLn $ concat ["Part 1:\t", show $ solve1 directions, 
      "\nPart 2:\t", show $ solve2 directions]
    else error "Invalid run mode."

-- parse turns a string containing formatted directions into a list of
-- Directions. It wraps parse', which is a tail recursive helper.
parse :: String -> [Direction]
parse = parse' "" []

-- parse' takes a partial reversd token, a partial reversed direction list, and
-- an input string, and returns the direction list. It does this recursively.
parse' :: String -> [Direction] -> String -> [Direction]
-- The base case of the recursion checks to see if the partial token is empty,
-- which will happen if the string has trailing non-token characters. If the
-- partial token is nonempty, it should contain a complete token, which is
-- prepended to the partial list. The partial list is now reversed in either
-- case to give the parsed direction list.
parse' pt pdl [] = if pt=="" then reverse pdl
  else reverse (mkDirection (reverse pt):pdl)
-- Checks the character at the head of the string is part of a token. If so, 
-- the character is added to the partial token, and the function is called
-- recursively on the remaining list. If not, the token is complete. It is
-- reversed, parsed into a Direction, then prepended to the partial direction
-- list. parse' is then called recursively.
parse' pt pdl (c:cs) = if isTokenC c
  then parse' (c:pt) pdl cs
  else let 
      cl = dropWhile (not.isTokenC) cs
    in parse' "" (mkDirection (reverse pt):pdl) cl

isTokenC = (`elem` "1234567890LR")

-- Turns a single token into a direction object
mkDirection :: String -> Direction
mkDirection (d:l) = case d of
  'L'         -> Left  $ read l
  'R'         -> Right $ read l
  otherwise   -> error "Error in input file.\n"

-- Solver for the first part of the problem. Simply folds the direction list
-- using move from the initial position and orientation, then calculates the
-- distance to the result.
solve1 :: [Direction] -> Int
solve1 dirs = let
    Position _ (x,y) = foldl move (Position North (0,0)) dirs
  in (abs x)+(abs y)

-- solve2 is the solver function for the second part of the problem. It wraps
-- the recursive function solve2' which does the work.
solve2 :: [Direction] -> Int
solve2 = solve2' (Position North (0,0)) [(0,0)]

solve2' :: Position -> [(Int,Int)] -> [Direction] -> Int
solve2' _ _ [] = error "No location visited twiced."
solve2' op@(Position _ oloc) h (d:ds) = let
    np@(Position _ loc@(x,y)) = move op d
    il = interpolate oloc loc
    msol = getSolFromIL il h
  in if isJust msol then fromJust msol else solve2' np (il++h) ds

move :: Position -> Direction -> Position
move (Position co (cx,cy)) d = case d of
    (Left ml)   ->  case co of
                      North   -> Position West  (cx-ml,cy)
                      East    -> Position North (cx,cy+ml)
                      South   -> Position East  (cx+ml,cy)
                      West    -> Position South (cx,cy-ml)
    (Right ml)  ->  case co of
                      North   -> Position East  (cx+ml,cy)
                      East    -> Position South (cx,cy-ml)
                      South   -> Position West  (cx-ml,cy)
                      West    -> Position North (cx,cy+ml)

interpolate :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
interpolate (x,y) (x',y') = 
  if x==x' then
    if y < y' then [ (x,y'') | y'' <- [y+1..y'] ]
    else [ (x,y'') | y'' <- reverse [y'..y-1] ]
  else if y==y' then
    if x < x' then [ (x'',y) | x'' <- [x+1..x'] ]
    else [ (x'',y) | x'' <- reverse [x'..x-1] ]
  else error "Not a straight line move."

getSolFromIL :: [(Int,Int)] -> [(Int,Int)] -> Maybe Int
getSolFromIL [] _ = Nothing
getSolFromIL (l@(x,y):ls) h = if l `elem` h then Just $ abs x + abs y
  else getSolFromIL ls h
