module Count.CountVotes where
import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

weight:: Float
weight = 1.0

myFst :: (a,b,c) -> a
myFst (x,_,_) = x

mySnd :: (a,b,c) -> b
mySnd (_,x,_) = x

myTrd :: (a,b,c) -> c
myTrd (_,_,x) = x

stringToInt :: String -> Int
stringToInt string = read string::Int

ballotHeadTail:: [String] -> (String, [String], Float)
ballotHeadTail (x:xs) = (x, xs, weight)

splitBallotHeadTail:: [[String]] -> [(String, [String], Float)]
splitBallotHeadTail votes = map (ballotHeadTail) votes

candidatesFirstPrefBallots :: [(String, [String], Float)] -> String -> (String, [([String], Float)])
candidatesFirstPrefBallots ys x = (x, [(mySnd y, myTrd y) | y <- ys, myFst y == x])

applyCandidatesFirstPrefBallots :: [String] -> [(String, [String], Float)] -> [(String, [([String], Float)])]
applyCandidatesFirstPrefBallots candidates votes = map (candidatesFirstPrefBallots votes) candidates

countFirstPref :: (String, [([String], Float)]) -> (String, Float)
countFirstPref candidate = (fst candidate, sum $ map (snd) (snd candidate))

getCountFirstPref ::[[String]] -> [String] -> [(String, Float)]
getCountFirstPref votes candidates = reverse $ sortBy (compare `on` snd) $ map (countFirstPref) $ applyCandidatesFirstPrefBallots candidates (splitBallotHeadTail votes)

candidatesAboveQuota :: Float -> [[String]] -> [String] -> [(String, Float)]
candidatesAboveQuota quota votes candidates = filter((>quota) . realToFrac . snd) $ getCountFirstPref votes candidates

roundWinner quota votes candidates = head $ candidatesAboveQuota quota votes candidates
--get length of second element in each tuple, ie list of first prefs
--get quota
--check who was first passed the quota
--calculate the new weight
--redistribute that second element of whoever was first passed quota with the new weight, ie second element into splitBallotHeadTail
--repeat

numberOfValidVotes :: [[String]] -> Int
numberOfValidVotes votes = length votes

findQuota :: Float -> [[String]] -> Float
findQuota seats votes = 1 + (a / (b + 1))
  where a = fromIntegral $ numberOfValidVotes votes :: Float
        b = seats
