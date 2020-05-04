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
  -- print "Enter number of seats: "
  -- seats <- readLn :: IO Float
  -- print $ quota seats votes
  -- print "Votes: "
  -- print $ votes
  -- print "Split ballots: "
  -- print $ splitBallotHeadTail votes
  -- print "Wack shiz: "
  -- print $ applyCandidatesFirstPrefBallots candidates $ (splitBallotHeadTail votes)
  print "Candidates first preference count: "
  print $ getCountFirstPref votes candidates
