{-# LANGUAGE OverloadedStrings #-}

import Data.Set as Set
import Debug.Trace
import Data.Char
import Data.Maybe
import Data.List.Split as Split
import Data.Map.Strict as Map

hasSpaces [] = False
hasSpaces (l:ll) | l == ' '  = True
                 | otherwise = hasSpaces ll


getComplexityPercentage:: Set String -> [String] -> Int
getComplexityPercentage dictWords inputWords = 100 - ((getComplexityPercentage' inputWords dictWords 0 * 100) `div` (length inputWords))

getComplexityPercentage' [] dictWords counter = counter
getComplexityPercentage' (w:ww) dictWords counter 
  | Set.member w dictWords = getComplexityPercentage' ww dictWords (counter + 1)
  | otherwise          = trace w $ getComplexityPercentage' ww dictWords counter

-- indexLemma Frequency derivedLemmas
data Lemma = Lemma String Int (Set String) deriving (Show)

-- getting a lemma from a line
mySplit = Split.splitOn " -> "
mySplitHead = Split.splitOn "/"
mySplitTail = Split.splitOn ","

getLemmaFromLine :: String -> Lemma
getLemmaFromLine line = Lemma 
  (head (mySplitHead (head (mySplit line)))) 
  (read (last (mySplitHead (head (mySplit line)))) :: Int) 
  (Set.fromList (mySplitTail (last (mySplit line))))


getMapFromLemma :: Lemma -> (Map String String)
getMapFromLemma (Lemma indexLemma _ derivedLemmas) = Map.fromSet (\k -> indexLemma) derivedLemmas

getMapFromLemmasList' [] dict = dict
getMapFromLemmasList' (l:ll) dict = getMapFromLemmasList' ll (Map.union dict (getMapFromLemma l))

getMapFromLemmasList lemmas = getMapFromLemmasList' lemmas Map.empty


convertLemma lemmasMap word = 
  if Map.lookup word lemmasMap == Nothing then word else (fromJust(Map.lookup word lemmasMap))


main = do
  words3000String <- readFile "resources/oxford-3000.txt"
  let words3000Set = Set.fromList $ Prelude.filter (not . hasSpaces) (lines words3000String)
  
  lemmasString <- readFile "resources/lemmas_en.txt"
  let lemmas = Prelude.map getLemmaFromLine (lines lemmasString)
  
  let lemmasMap = getMapFromLemmasList lemmas
  
  interact $
    (++ " % of the words are not inside the Oxford 3000 list.\n") .
    show .
    getComplexityPercentage words3000Set .
    Prelude.map (convertLemma lemmasMap) . -- convert coniugated words
    Prelude.map (Prelude.filter isAlpha) . -- delete punctuation
    Prelude.map (Prelude.map toLower) . -- lowercase characters
    words
       