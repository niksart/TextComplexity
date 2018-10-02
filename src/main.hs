import qualified Data.Set as Set
import Debug.Trace
import Data.Char
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

getComplexityPercentage :: Set.Set String -> [String] -> Int
getComplexityPercentage dictWords inputWords = 100 - ((getComplexityPercentage' inputWords dictWords 0 * 100) `div` (length inputWords))

getComplexityPercentage' [] dictWords counter = counter
getComplexityPercentage' (w:ww) dictWords counter 
  | Set.member w dictWords = getComplexityPercentage' ww dictWords (counter + 1)
  | otherwise              = trace w $ getComplexityPercentage' ww dictWords counter

convertLemma lemmasMap word = 
  if Map.lookup word lemmasMap == Nothing then word else (Maybe.fromJust(Map.lookup word lemmasMap))


main = do
  words3000String <- readFile "resources/oxford-3000.txt"
  let words3000Set = Set.fromList $ Prelude.filter (not . hasSpaces) (lines words3000String)
  
  lemmasMapString <- BS.readFile "resources/lemmasMap.json"
  let lemmasMap = Maybe.fromJust (Aeson.decode lemmasMapString :: Maybe (Map.Map String String))
  
  interact $
    (++ " % of the words are not inside the Oxford 3000 list.\n") .
    show .
    getComplexityPercentage words3000Set .
    filter (/= "") .
    map (convertLemma lemmasMap) . -- convert coniugated words
    map (filter isAdmissible) . -- delete punctuation
    map convertToLower . -- lowercase characters
    words .
    spaceBeforeQuote
       