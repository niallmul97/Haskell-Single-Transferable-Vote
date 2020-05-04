module Count.CountVotes where

import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

stringToInt :: String -> Int
stringToInt string = read string::Int

-- intToFloat :: Int -> Float
-- intToFloat x = x::Float

ballotHeadTail:: [String] -> (String, [String])
ballotHeadTail (x:xs) = (x, xs)

splitBallotHeadTail:: [[String]] -> [(String, [String])]
splitBallotHeadTail votes = map (ballotHeadTail) votes

-- candidatesFirstPrefBallots :: [String] -> [(String, [String])] -> [(String, [String])]
candidatesFirstPrefBallots ys x = (x, [snd y | y <- ys, fst y == x])
-- (Ed.Balls [ballots] )

applyCandidatesFirstPrefBallots :: [String] -> [(String, [String])] -> [(String, [[String]])]
applyCandidatesFirstPrefBallots candidates votes = map (candidatesFirstPrefBallots votes) (candidates)

countFirstPref candidate = (fst candidate, length(snd candidate))

getCountFirstPref votes candidates = map (countFirstPref) (applyCandidatesFirstPrefBallots candidates (splitBallotHeadTail votes))
--get length of second element in each tuple, ie list of first prefs
--get quota
--check who was first passed the quota
--calculate the new weight
--redistribute that second element of whoever was first passed quota with the new weight, ie second element into splitBallotHeadTail
--repeat

numberOfValidVotes :: [[String]] -> Int
numberOfValidVotes votes = length votes

quota :: Float -> [[String]] -> Float
quota seats votes = 1 + (a / (b + 1))
  where a = fromIntegral $ numberOfValidVotes votes :: Float
        b = seats
