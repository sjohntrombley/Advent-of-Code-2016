module Main where

import  System.Environment          (getArgs, getProgName)
import  Prelude             hiding  (Left, Right)
import  Data.Maybe                  (isJust, fromJust)

data Direction = Left Int | Right Int           deriving Show
data Orientation = North | East | South | West  deriving Show
data Position = Position Orientation (Int,Int)  deriving Show

-- Binding:       main
-- Description:   Gets program arguments and program name. The first argument
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
-- Argument:      Input string of directions
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
--    non-token characters, so pdl is reversed and returned. If pt is nonempty,
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

mkDirection :: String -> Direction
mkDirection (d:l) = case d of
  'L'         -> Left  $ read l
  'R'         -> Right $ read l
  otherwise   -> error "Error in input file.\n"

-- Function:      solve1
-- Arguments:
--    * (dirs::[Direction) the list of directions returned by parse.
-- Description:   The solution is calculated by simply calling foldl with move
--    as its functional argument on dirs. The initial value for the fold is the 
--    initial coordinates and orientation, namely (0,0) and North, 
--    respectively. The final location is extracted from the fold result, and
--    the absolute values of the coordinates are added.
solve1 :: [Direction] -> Int
solve1 dirs = let
    Position _ (x,y) = foldl move (Position North (0,0)) dirs
  in (abs x)+(abs y)

-- Function:      solve2
-- Arguments:
--    * (dirs::[Direction]) The list of directions returned by parse
-- Description:   Takes the list of directions returned by parse and returns
--    the distance to the first point visited twice. It wraps solve2', passing
--    initial arguments of (Position North (0,0)) for the initial position, a
--    list containing only (0,0) for the history (since the current position is
--    expected to be in the history by solve2'), and dirs for the direction
--    list.
solve2 :: [Direction] -> Int
solve2 = solve2' (Position North (0,0)) [(0,0)]

-- Function:      solve2'
-- Arguments:
--    * (op::Position) the current position along the path.
--    * (h::[(Int,Int)]) the history of locations already visited. Should
--      include the location contained in op.
--    * (ds::[Direction]) the remaining list of directions.
-- Description:   solve2' first checks if the remaining direction list is
--    empty. If it is, no location is visited twice, and so an error is
--    produced. If the direction list is non-empty, the endpoint of the next
--    direction is determined. A list of locations between op and the
--    destination location, in order of visiting and including the destination,
--    but excluding the original location, is returned by iterate. getSolFromIL
--    returns Nothing if no location is visited twice yet, or Just i, where i 
--    is the distance to the location, if one is. If it is Nothing, the
--    interpolated list is prepended to the history, and solve2' is called
--    recursively with the new Position, new history, and the tail of the
--    direction list.
solve2' :: Position -> [(Int,Int)] -> [Direction] -> Int
solve2' _ _ [] = error "No location visited twice.\n"
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
