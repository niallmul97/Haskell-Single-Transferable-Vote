module Count.CountVotes where
import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

--Variable for vote weight, inital value is 1.0
weight:: Float
weight = 1.0

--Function that will get the first element in a tuple with three elements
myFst :: (a,b,c) -> a
myFst (x,_,_) = x

--Function that will get the second element in a tuple with three elements
mySnd :: (a,b,c) -> b
mySnd (_,x,_) = x

--Function that will get the third element in a tuple with three elements
myTrd :: (a,b,c) -> c
myTrd (_,_,x) = x

--Converts a String to an INT
stringToInt :: String -> Int
stringToInt string = read string::Int

--Splits a ballot into a tuple where the head is the first element, the tail the second,
-- and the weight of the vote is the third.
ballotSplitAddWeight:: [String] -> (String, [String], Float)
ballotSplitAddWeight (x:xs) = (x, xs, weight)

--Maps the list of votes to the ballotHeadTail Function
votesSplitAddWeight:: [[String]] -> [(String, [String], Float)]
votesSplitAddWeight votes = map (ballotSplitAddWeight) votes

--Takes in the list of votes and a candidate, outputs a tuple, where the first element is the candidate,
--the second is a list of tuples, in which the first element is the list of ballots where the candidate
--was the first preference and the the second element is the weight.
candidatesFirstPrefBallots :: [(String, [String], Float)] -> String -> (String, [([String], Float)])
candidatesFirstPrefBallots ys x = (x, [(mySnd y, myTrd y) | y <- ys, myFst y == x])

--Maps candidatesFirstPrefBallots function to each candidate, resulting in a list of 5 tuples, one for each candidate
--containing the ballots which had them at first preference along with their weight.
applyCandidatesFirstPrefBallots :: [String] -> [(String, [String], Float)] -> [(String, [([String], Float)])]
applyCandidatesFirstPrefBallots candidates votes = map (candidatesFirstPrefBallots votes) candidates

--Takes in a candidate, and returns the total weight of their votes
countFirstPref :: (String, [([String], Float)]) -> (String, Float)
countFirstPref candidate = (fst candidate, sum $ map (snd) (snd candidate))

--Maps countFirstPref to the split votes sorting the candidates in reverse order by their total vote weight
getCountFirstPref ::[(String, [String], Float)] -> [String] -> [(String, Float)]
getCountFirstPref votes candidates = reverse $ sortBy (compare `on` snd) $ map (countFirstPref) $ applyCandidatesFirstPrefBallots candidates votes

--Returns the list of candidates that were above the quota with their vote weight
candidatesAboveQuota :: Float -> [(String, [String], Float)] -> [String] -> [(String, Float)]
candidatesAboveQuota quota votes candidates = filter((>quota) . realToFrac . snd) $ getCountFirstPref votes candidates

--Returns the winner of the round with their vote weight
roundWinner:: Float -> [(String, [String], Float)] -> [String] -> [(String, Float)]
roundWinner quota votes candidates = candidatesAboveQuota quota votes candidates

--Main driver function for STV
driver :: Float -> [(String, [String], Float)] -> [String] -> [(String, Float)]
driver quota votes candidates = roundWinner quota (votes ++ newVotes) candidates
  where newVotes = updateWeights (fst . head $ roundWinner quota votes candidates) quota votes

--Splits a ballot where the head is the first element in the tuple,
--and the tail is the second, with the weight being the third
splitBallotHeadTail :: (String, [String], Float) -> (String, [String], Float)
splitBallotHeadTail ballot = (head $ mySnd ballot, tail $ mySnd ballot, myTrd ballot)

--Maps the votes to the splitBallotHeadTail function
splitVotesHeadTail :: [(String, [String], Float)] -> [(String, [String], Float)]
splitVotesHeadTail votes = map (splitBallotHeadTail) votes

--finds list of transferable votes
findTransferableVotes :: String -> [(String, [String], Float)] -> [(String, [String], Float)]
findTransferableVotes candidate votes = splitVotesHeadTail $ filter((/=[]).mySnd) $ filter((==candidate).myFst) votes

--calculats the new weight for the transferable votes
newWeight:: Float -> [(String, [String], Float)] -> String -> Float
newWeight quota votes candidate = (a - quota ) / b
  where a = sum $ map (myTrd) $ filter((==candidate).myFst) votes
        b = sum $ map (myTrd) $ findTransferableVotes candidate votes

--applies the new weight to the transferable votes
updateWeights:: String -> Float -> [(String, [String], Float)] -> [(String, [String], Float)]
updateWeights candidate quota votes = map (\x -> (myFst x, mySnd x, myTrd x * y)) (findTransferableVotes candidate votes)
  where y = newWeight quota votes candidate

-- removes candidate from the candidates list
updateCandidates :: String -> [String] -> [String]
updateCandidates _ [] = []
updateCandidates candidate (x : xs)
  | candidate == x = xs
  | otherwise = x : xs

--gets the number of votes for calculating the quota
numberOfValidVotes :: [[String]] -> Int
numberOfValidVotes votes = length votes

--calculates the quota
findQuota :: Float -> [[String]] -> Float
findQuota seats votes = 1 + (a / (b + 1))
  where a = fromIntegral $ numberOfValidVotes votes :: Float
        b = seats
