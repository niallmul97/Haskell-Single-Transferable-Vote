module Main where

import Lib
import Clean.CleanVotes
import Count.CountVotes
import AltVote.AlternativeVote

main :: IO ()
main = do
  csvVotes <- readFile "votes.csv"

  print "Enter number of seats: "
  seats <- readLn :: IO Float

  let votes = finalVotes csvVotes
  let quota = findQuota seats votes
  let candidates = getCandidates csvVotes

  print "Alt vote: "
  print $ winner' votes

  print "Quota: "
  print quota

  -- print "Candidates above quota: "
  -- print $ candidatesAboveQuota quota votes candidates

  print "Round Winner: "
  print $ driver quota (splitBallotHeadTail votes) candidates
  -- print "Weight of transferable votes"
  -- print $ weightOfTransferableVotes $ applyCandidatesFirstPrefBallots candidates (splitBallotHeadTail votes)
