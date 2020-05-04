module Clean.CleanVotes where

import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

changeToInt :: String -> Int
changeToInt string = read string::Int

getCandidates csvVotes = splitOn"," $drop 2 (head (drop 1 $splitOn"\n" csvVotes))

getBallots csvVotes = map (drop 2 . splitOn ",")  $ tail $ drop 1 $ splitOn "\n" csvVotes

zipCandiates candidates votePref = trace ("\n")zip (candidates) (votePref)

formatVotes csvVotes = map (zipCandiates (getCandidates csvVotes)) (getBallots csvVotes)

sortBallotByPref ballot = sortBy (compare `on` snd) ballot

sortVotes csvVotes = map (sortBallotByPref) (formatVotes csvVotes)

removeBlankVotes xs = [(fst x , snd x)| x <- xs, snd x/="*", snd x/=""]

cleanVotes csvVotes =  filter(/=[]) (map (removeBlankVotes) (sortVotes csvVotes))

listOfPrefs :: [(String, String)] -> [Int]
listOfPrefs = map (\x -> changeToInt(snd x))

-- Stops counting votes for the row if any gap more than one between vote priorities are detected
removeHoles :: [(String, String)] -> [(String, String)]
removeHoles [] = []
removeHoles (x : xs)
  | changeToInt (snd x) + 1 `notElem` listOfPrefs xs  = [x]
  | otherwise = x : removeHoles xs


removeDupes :: [(String, String)] -> [(String, String)]
removeDupes ballot = concat ([x | x <- votes, length x == 1])
            where votes = findDupes ballot


findDupes :: [(String, String)] -> [[(String, String)]]
findDupes ballot = (groupBy (\a b -> ((changeToInt . snd) a) == ((changeToInt . snd)b)) ballot)

fixDupesAndHoles :: [(String, String)] -> [(String, String)]
fixDupesAndHoles ballot = removeHoles $ removeDupes ballot

validVotes csvVotes = map (fixDupesAndHoles) (cleanVotes csvVotes)

orderedCandidates xs = [fst x | x <- xs]

finalVotes csvVotes = map (orderedCandidates) (validVotes csvVotes)
