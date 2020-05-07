module Clean.CleanVotes where
import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

--converts a string to an int
changeToInt :: String -> Int
changeToInt string = read string::Int

--gets list of candidates
getCandidates:: String-> [String]
getCandidates csvVotes = splitOn"," $drop 2 (head (drop 1 $splitOn"\n" csvVotes))

--gets the list of votes dropping the appropriate lines from the csv
getBallots:: String -> [[String]]
getBallots csvVotes = map (drop 2 . splitOn ",")  $ tail $ drop 1 $ splitOn "\n" csvVotes

--Zips the candidates with their respective preference
zipCandiates :: [String] -> [String] -> [(String, String)]
zipCandiates candidates votePref = trace ("\n")zip (candidates) (votePref)

--Maps zipCandiates to the output of getCandidates and getBallots
formatVotes :: String -> [[(String, String)]]
formatVotes csvVotes = map (zipCandiates (getCandidates csvVotes)) (getBallots csvVotes)

--Sorts a ballot in order of preference
sortBallotByPref:: [(String, String)] -> [(String, String)]
sortBallotByPref ballot = sortBy (compare `on` snd) ballot

--Maps sortBallotByPref to the output of formatVotes
sortVotes :: String -> [[(String, String)]]
sortVotes csvVotes = map (sortBallotByPref) (formatVotes csvVotes)

--Removes a vote when the snd x or preference is blank or a *
removeBlankVotes :: [(String, String)] -> [(String, String)]
removeBlankVotes xs = [(fst x , snd x)| x <- xs, snd x/="*", snd x/=""]

--Maps removeBlankVotes to the output of sortVotes
cleanVotes :: String -> [[(String, String)]]
cleanVotes csvVotes =  filter(/=[]) (map (removeBlankVotes) (sortVotes csvVotes))

--Returns a list of the preferences
listOfPrefs :: [(String, String)] -> [Int]
listOfPrefs = map (\x -> changeToInt(snd x))

--Checks if 1+ a preference appears in the listOfPrefs output
--returns when a gap/hole is detected
removeHoles :: [(String, String)] -> [(String, String)]
removeHoles [] = []
removeHoles (x : xs)
  | changeToInt (snd x) + 1 `notElem` listOfPrefs xs  = [x]
  | otherwise = x : removeHoles xs

--Adds the list of preferences from the output of findDupes whenever the length is 1, thus removing dupes
removeDupes :: [(String, String)] -> [(String, String)]
removeDupes ballot = concat ([x | x <- votes, length x == 1])
            where votes = findDupes ballot

--Groups the ballots by preferences that are the same
findDupes :: [(String, String)] -> [[(String, String)]]
findDupes ballot = (groupBy (\a b -> ((changeToInt . snd) a) == ((changeToInt . snd)b)) ballot)

--Removes the dupes, and then holes in a ballot
fixDupesAndHoles :: [(String, String)] -> [(String, String)]
fixDupesAndHoles ballot = removeHoles $ removeDupes ballot

--Maps the fixDupesAndHoles function to the output of cleanVotes
validVotes :: String -> [[(String, String)]]
validVotes csvVotes = map (fixDupesAndHoles) (cleanVotes csvVotes)

--Returns a list of candidates based on the order of preference but without the preference number
orderedCandidates :: [(String, String)] -> [String]
orderedCandidates xs = [fst x | x <- xs]

--Maps the output of validVotes to the orderedCandidates function, resulting in the final version of clean votes.
finalVotes :: String -> [[String]]
finalVotes csvVotes = map (orderedCandidates) (validVotes csvVotes)
