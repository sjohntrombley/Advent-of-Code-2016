module Main where

import  System.Environment          (getArgs, getProgName)
import  Prelude             hiding  (Left, Right)
import  Data.Maybe                  (isJust, fromJust)

data Direction = Left Int | Right Int           deriving Show
data Orientation = North | East | South | West  deriving Show
data Position = Position Orientation (Int,Int)  deriving Show

-- Binding:       main
-- Description:   Gets program arguemnts and program name. The first argument
--    is run mode (either '-1', '-2', or '-12'. The second is the name of the
--    file containing directions. The String read from the input file is parsed
--    by the parse function, then either solve1, solve2, or both is called on
--    the parsed input depending on run mode.
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

-- Function:      parse
-- Arguemnt:      Input string of directions
-- Description:   Takes an input string as an argument, and returns a list of
--    Directions. It simply wraps parse' and passes it initial parameters of an
--    empty partial token and an empty partial direction list.
parse :: String -> [Direction]
parse = parse' "" []

-- Function:      parse'
-- Arguments:
--    * (pt::String) is a partially read token
--    * (pdl::[Direction]) is a list of directions already parsed in reversed
--        order
--    * (cs::String) is the remaining input String
-- Description:   parse' is a recursive helper function for parsing the cs. If
--    cs is nonempty, the head character is read. If it is part of a token, it
--    is added to the head of pt. If it is not, pt is considered to have a
--    complete token. The String is reversed, and turned into a Direction with
--    mkDirection. This Direction is added to the head of pdl. All non-token
--    characters are stripped from the head of cs, and parse' is called
--    recursively. If cs is empty and pt is empty, then cs had trailing
--    non-tken characters, so pdl is reversed and returned. If pt is nonempty,
--    cs had no trailing non-token characters, and the last token is parsed and
--    added to the head of pdl, which is then reversed and returned.
parse' :: String -> [Direction] -> String -> [Direction]
parse' pt pdl [] = if pt=="" then reverse pdl
  else reverse (mkDirection (reverse pt):pdl)
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

-- solve2' takes a position, a history of past locations, and a list of
-- directions, and returns the distance to the first location visited twice. It
-- does this by calculating all of the points between its initial position and
-- its final position after the next move in the direction list. q
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
