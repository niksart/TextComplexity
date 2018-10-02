import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B

-- indexLemma derivedLemmas
data Lemma = Lemma String (Set String) deriving (Show)

-- getting a lemma from a line
mySplit = Split.splitOn " -> "
mySplitHead = Split.splitOn "/"
mySplitTail = Split.splitOn ","

getLemmaFromLine :: String -> Lemma
getLemmaFromLine line = Lemma 
  (head (mySplitHead (head (mySplit line)))) 
  (Set.fromList (mySplitTail (last (mySplit line))))

getMapFromLemma :: Lemma -> Map String String
getMapFromLemma (Lemma indexLemma derivedLemmas) = Map.fromSet (const indexLemma) derivedLemmas

getMapFromLemmasList' [] dict = dict
getMapFromLemmasList' (l:ll) dict = getMapFromLemmasList' ll (Map.union dict (getMapFromLemma l))

getMapFromLemmasList lemmas = getMapFromLemmasList' lemmas Map.empty

main :: IO ()
main = do
  lemmasString <- readFile "resources/lemmas_en.txt"
  let lemmas = Prelude.map getLemmaFromLine (lines lemmasString)
  
  let lemmasMap = getMapFromLemmasList lemmas
  
  B.writeFile "resources/lemmasMap.json" (Aeson.encode lemmasMap)
  