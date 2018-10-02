import qualified Data.Set as Set
import Debug.Trace
import Data.Char
import Data.List (intersperse)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson

-- | is c a meaningful character?
isAdmissible :: Char -> Bool
isAdmissible c = isAlpha c || c == '\''

convertCRLFtoChar :: Char -> Char
convertCRLFtoChar '\n' = ' '
convertCRLFtoChar c = c

-- | convert to lowercase all the characters apart 'I'
convertToLower :: String -> String
convertToLower [] = []
convertToLower (c:cc) = if (c:cc) == "I" then "I" else (toLower c):(convertToLower cc)

spaceBeforeQuote' :: Char -> String
spaceBeforeQuote' '\'' = " '"
spaceBeforeQuote' c = [c]

spaceBeforeQuote :: String -> String
spaceBeforeQuote [] = []
spaceBeforeQuote (s:ss) = (spaceBeforeQuote' s) ++ (spaceBeforeQuote ss)

hasSpaces :: String -> Bool
hasSpaces [] = False
hasSpaces (l:ll) | l == ' '  = True
                 | otherwise = hasSpaces ll

-- | I've choose a list because of repetitions of some words in the text
getComplexWords :: Set.Set String -> [String] -> ([String], [String])
getComplexWords dictWords inputWords = (inputWords, getComplexWords' dictWords inputWords [])

getComplexWords' dictWords [] complexWords = complexWords
getComplexWords' dictWords (w:ww) complexWords 
  | Set.member w dictWords = getComplexWords' dictWords ww complexWords
  | otherwise              = getComplexWords' dictWords ww (w:complexWords)

convertLemma lemmasMap word = 
  if Map.lookup word lemmasMap == Nothing then word else (Maybe.fromJust(Map.lookup word lemmasMap))

stringReport :: ([String], [String]) -> String
stringReport (inputWords, complexWords) =
  (show ((length complexWords)*100 `div` (length inputWords))) ++ " % of the words are not in the Oxford 3000 list.\n\nHere are the words:\n" ++ (concat (intersperse ", " (Set.toList(Set.fromList complexWords)))) ++ "\n"

main = do
  words3000String <- readFile "resources/oxford-3000.txt"
  let words3000Set = Set.fromList $ Prelude.filter (not . hasSpaces) (lines words3000String)
  
  lemmasMapString <- BS.readFile "resources/lemmasMap.json"
  let lemmasMap = Maybe.fromJust (Aeson.decode lemmasMapString :: Maybe (Map.Map String String))
  
  interact $
    stringReport .
    getComplexWords words3000Set .
    filter (/= "") .
    map (convertLemma lemmasMap) . -- convert coniugated words
    map convertToLower . -- lowercase characters
    words .
    map (\c -> if isAdmissible c == False then ' ' else c) .
    spaceBeforeQuote
       