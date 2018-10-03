import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson

-- | is c a meaningful character?
isAdmissible :: Char -> Bool
isAdmissible c = isAlpha c || c == '\''

-- | convert to lowercase all the characters apart 'I'
convertToLower :: String -> String
convertToLower [] = []
convertToLower "I" = "I"
convertToLower (c:cc) = toLower c : convertToLower cc

truncateAfterQuote' :: String -> String -> String
truncateAfterQuote' [] word = word
truncateAfterQuote' ('\'': ss) _ = ss
truncateAfterQuote' (_: ss) word = truncateAfterQuote' ss word

-- the contractions have already been relaxed. Now I can truncate quotes because it's only used in genitive
truncateAfterQuote :: String -> String
truncateAfterQuote s = reverse $ truncateAfterQuote' (reverse s) (reverse s)

hasSpaces :: String -> Bool
hasSpaces [] = False
hasSpaces (l:ll) | l == ' '  = True
                 | otherwise = hasSpaces ll

-- | I've choose a list because of repetitions of some words in the text
getComplexWords :: Set String -> [String] -> ([String], [String])
getComplexWords dictWords inputWords = (inputWords, getComplexWords' dictWords inputWords [])

getComplexWords' :: Ord a => Set a -> [a] -> [a] -> [a]
getComplexWords' _ [] complexWords = complexWords
getComplexWords' dictWords (w:ww) complexWords 
  | Set.member w dictWords = getComplexWords' dictWords ww complexWords
  | otherwise              = getComplexWords' dictWords ww (w:complexWords)

convertLemma :: Ord k => Map k k -> k -> k
convertLemma lemmasMap word = 
  if Maybe.isNothing $ Map.lookup word lemmasMap then word else Maybe.fromJust(Map.lookup word lemmasMap)

stringReport :: ([String], [String]) -> String
stringReport (inputWords, complexWords) =
  show ((length complexWords * 100) `div` length inputWords)
  ++ " % of the words are not in the Oxford 3000 list ("
  ++ show (length complexWords)
  ++ " over "
  ++ show (length inputWords)
  ++ ").\n\nHere are the words:\n" 
  ++ intercalate ", " (Set.toList $ Set.fromList complexWords)
  ++ "\n"

main :: IO ()
main = do
  words3000String <- readFile "resources/oxford-3000.txt"
  let words3000Set = Set.fromList $ Prelude.filter (not . hasSpaces) (lines words3000String)
  
  lemmasMapString <- BS.readFile "resources/lemmasMap.json"
  let lemmasMap = Maybe.fromJust (Aeson.decode lemmasMapString :: Maybe (Map String String))
  
  contractionsString <- BS.readFile "resources/contractions.json"
  let contractionsMap = Maybe.fromJust (Aeson.decode contractionsString :: Maybe (Map String String))
  
  interact $
    stringReport .
    getComplexWords words3000Set .
    filter (/= "") .
    map (truncateAfterQuote . convertLemma lemmasMap . convertLemma contractionsMap  . convertToLower) . -- convert coniugated words and to lower
    words .
    map (\c -> if not $ isAdmissible c then ' ' else c)
