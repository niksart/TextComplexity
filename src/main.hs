{-# LANGUAGE OverloadedStrings #-}

import Data.Set (Set, fromList, member)
import Debug.Trace
import Data.Char
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy hiding (map, interact, filter, length)

hasSpaces [] = False
hasSpaces (l:ll) | l == ' '  = True
                 | otherwise = hasSpaces ll

getPercentage:: Set String -> [String] -> Int
getPercentage dictWords inputWords = 100 - ((getPercentage' inputWords dictWords 0 * 100) `div` (length inputWords))

getPercentage' [] dictWords counter = counter
getPercentage' (w:ww) dictWords counter 
  | member w dictWords = getPercentage' ww dictWords (counter + 1)
  | otherwise          = trace w $ getPercentage' ww dictWords counter

main = do
  words3000String <- Prelude.readFile "resources/oxford-3000.txt"
  let words3000Set = fromList $ filter (not . hasSpaces) (lines words3000String)
  
  verbs3000JSON <- Data.ByteString.Lazy.readFile "resources/verbs/verbs-oxford-3000.json"
  let verbs3000Set = fromList $ fromJust (decode verbs3000JSON :: Maybe [String])
       
  interact $
    (++ " % of the words are not inside the Oxford 3000 list.\n") .
    show .
    getPercentage words3000Set .
    map (filter isAlpha) . -- delete punctuation
    map (map toLower) . -- lowercase characters
    words
       