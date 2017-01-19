module Main where

import  Data.Char           (isDigit, digitToInt, intToDigit)
import  System.Environment  (getArgs,getProgName)

main = do
  args    <- getArgs
  pn      <- getProgName
  let   (rm,ifn) = 
          if length args /= 2 then error $ pn ++ " takes exactly 2 arguments.\n"
          else (args !! 0, args !! 1)
  fc      <- readFile ifn
  let   ifls = lines fc
        sol1 = getSol [] moveIfPossible1 5 ifls
        sol2 = getSol [] moveIfPossible2 '5' ifls
  if length rm < 2 then error "Error in first argument.\n" else return ()
  if rm !! 1 == '1' then putStrLn $ "Part 1:\t" ++ concatMap show sol1
    else return ()
  if last rm == '2' then putStrLn $ "Part 2:\t" ++ sol2 else return ()

-- Fucntion:      moveIfPossible1
-- Arguments:
--    * (cb::Int) current button
--    * (md::Char) Direction for next move
-- Description:   If the button is on the edge of the keypad, and the move
--    would move off of the keypad, just returns cb. Otherwise, moves in the
--    direction specified by md, then returns the new button.
moveIfPossible1 :: Int -> Char -> Int
moveIfPossible1 cb md = case md of
  'U'         ->  if div (cb-1) 3 == 0 then cb
                  else cb-3
  'R'         ->  if mod cb 3 == 0 then cb
                  else cb+1
  'D'         ->  if div (cb-1) 3 >= 2 then cb
                  else cb+3
  'L'         ->  if mod cb 3 == 1 then cb
                  else cb-1
  otherwise   -> error "Invalid move direction.\n"

-- Function:      moveIfPossible1
-- Arguments:
--    * (cb::Char) current button
--    * (md::Char) direction to move in
-- Description:   Checks to see if a button exists in the move direction. If so
--    the result of the move is returned. If not, cb is returned.
moveIfPossible2 :: Char -> Char -> Char
moveIfPossible2 cb md = case md of
  'R'       ->  if cb`elem`"149CD" then cb 
                else if isDigit cb then intToDigit $ digitToInt cb + 1 
                else if cb=='A' then 'B' else 'C'
  'U'       ->  if cb`elem`"12459" then cb 
                else if isDigit cb 
                  then let m = digitToInt cb 
                  in if m==3 then '1' else intToDigit $ m-4
                else case cb of 
                  'A' -> '6'
                  'B' -> '7'
                  'C' -> '8'
                  'D' -> 'B'
  'L'       ->  if cb`elem`"125AD" then cb 
                else if isDigit cb then intToDigit $ digitToInt cb - 1 
                else if cb=='C' then 'B' else 'A'
  'D'       ->  if cb`elem`"59ACD" then cb 
                else case cb of 
                  '1' -> '3'
                  '2' -> '6'
                  '3' -> '7'
                  '4' -> '8'
                  '6' -> 'A'
                  '7' -> 'B'
                  '8' -> 'C'
                  'B' -> 'D'
  otherwise -> error "Invalid move direction.\n"

getSol :: [a]->(a->Char->a)->a->[String]->[a]
getSol rps _ _ [] = reverse rps
getSol rps mpf sb (dl:dls) = let eb = getRSol mpf sb dl 
  in getSol (eb:rps) mpf eb dls

getRSol :: (a->Char->a)->a->String->a
getRSol _ sb [] = sb
getRSol mpf sb (d:ds) = getRSol mpf (mpf sb d) ds
