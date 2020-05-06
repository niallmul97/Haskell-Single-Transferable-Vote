module Count.CountVotes where
import Debug.Trace
import Data.List
import Text.Read
import Data.List.Split (splitOn)
import Data.Function (on)

--List of winners
winners:: [(String, Float)]
winners = []

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
ballotHeadTail:: [String] -> (String, [String], Float)
ballotHeadTail (x:xs) = (x, xs, weight)

--Maps the list of votes to the ballotHeadTail Function
splitBallotHeadTail:: [[String]] -> [(String, [String], Float)]
splitBallotHeadTail votes = map (ballotHeadTail) votes

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

getCountFirstPref ::[(String, [String], Float)] -> [String] -> [(String, Float)]
getCountFirstPref votes candidates = reverse $ sortBy (compare `on` snd) $ map (countFirstPref) $ applyCandidatesFirstPrefBallots candidates votes

candidatesAboveQuota :: Float -> [(String, [String], Float)] -> [String] -> [(String, Float)]
candidatesAboveQuota quota votes candidates = filter((>quota) . realToFrac . snd) $ getCountFirstPref votes candidates

roundWinner quota votes candidates = candidatesAboveQuota quota votes candidates

driver :: Float -> [(String, [String], Float)] -> [String] -> [(String, Float)]
driver quota votes candidates = roundWinner quota (votes ++ newVotes) candidates
  where newVotes = updateWeights (fst . head $ roundWinner quota votes candidates) quota votes


  -- print $ findTransferableVotes "D. Milliband" (splitBallotHeadTail votes)
  --
  -- let votes2 = updateWeights "D. Milliband" quota (splitBallotHeadTail votes)
  -- print $ roundWinner quota ((splitBallotHeadTail votes) ++ votes2) candidates
ballotHeadTail2 :: (String, [String], Float) -> (String, [String], Float)
ballotHeadTail2 ballot = (head $ mySnd ballot, tail $ mySnd ballot, myTrd ballot)

splitballotHeadTail2 :: [(String, [String], Float)] -> [(String, [String], Float)]
splitballotHeadTail2 votes = map (ballotHeadTail2) votes

--finds list of transferable votes
findTransferableVotes :: String -> [(String, [String], Float)] -> [(String, [String], Float)]
findTransferableVotes candidate votes = splitballotHeadTail2 $ filter((/=[]).mySnd) $ filter((==candidate).myFst) votes

newWeight:: Float -> [(String, [String], Float)] -> String -> Float
newWeight quota votes candidate = (a - quota ) / b
  where a = sum $ map (myTrd) $ filter((==candidate).myFst) votes
        b = sum $ map (myTrd) $ findTransferableVotes candidate votes


updateWeights:: String -> Float -> [(String, [String], Float)] -> [(String, [String], Float)]
updateWeights candidate quota votes = map (\x -> (myFst x, mySnd x, myTrd x * y)) (findTransferableVotes candidate votes)
  where y = newWeight quota votes candidate

-- updateVotes candidate = candidatesFirstPrefBallots (findTransferableVotes candidate votes) candidate

-- removes candidate
updateCandidates :: String -> [String] -> [String]
updateCandidates _ [] = []
updateCandidates candidate (x : xs)
  | candidate == x = xs
  | otherwise = x : xs

-- driver :: Float -> [[String]] -> [String] -> Int -> Int -> [(String, Float)] -> [(String, Float)]
-- driver quota votes candidates 0 _ _ = roudWinner quota votes candidates
-- driver quota votes candidates seatsFilled seats winners
--                  | seatsFilled == seats = winners
--                  | otherwise = roundWinner quota (updateVotes votes) (updateCandidates candidates)

numberOfValidVotes :: [[String]] -> Int
numberOfValidVotes votes = length votes

findQuota :: Float -> [[String]] -> Float
findQuota seats votes = 1 + (a / (b + 1))
  where a = fromIntegral $ numberOfValidVotes votes :: Float
        b = seats

--Find weight of transferable votes
--calculate the new weight and apply to the transferable votes
--redistribute that second element of whoever was first passed quota with the new weight, ie second element into splitBallotHeadTail
--repeat
