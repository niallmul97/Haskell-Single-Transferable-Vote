module Main where

import Lib
import Clean.CleanVotes
import Count.CountVotes
import AltVote.AlternativeVote

main :: IO ()
main = do
  csvVotes <- readFile "votes.csv"
  let votes = finalVotes csvVotes
  -- print "Alt vote: "
  -- print $ winner' votes
  print "Candidates: "
  let candidates = getCandidates csvVotes
  -- print $ candidates
  print "Enter number of seats: "
  seats <- readLn :: IO Float
  let quota = findQuota seats votes
  print "Quota: "
  print quota
  -- print "Votes: "
  -- print $ votes
  -- print "Split ballots: "
  -- print $ splitBallotHeadTail votes
  -- print "Wack shiz: "
  -- print $ applyCandidatesFirstPrefBallots candidates $ (splitBallotHeadTail votes)
  -- print "Candidates first preference count: "
  -- print $ getCountFirstPref votes candidates
  print "Candidates above quota: "
  print $ candidatesAboveQuota quota votes candidates
  print "Round Winner: "
  print $ roundWinner quota votes candidates
