module Main where

import Lib
import Clean.CleanVotes
import Count.CountVotes
import AltVote.AlternativeVote

main :: IO ()
main = do

  --Reads in csv file
  csvVotes <- readFile "votes.csv"

  --User inputs the number of seats, read as a Float
  print "Enter number of seats: "
  seats <- readLn :: IO Float

  --Final version of clean votes
  let votes = finalVotes csvVotes
  -- print $ votes

  --The quota calculated based on number of seats
  let quota = findQuota seats votes

  --The list of candidates
  let candidates = getCandidates csvVotes

  --Shows the number of votes
  print "Number of votes: "
  print $ numberOfValidVotes votes

  --Shows the winner of Alternative Vote
  print "Alt vote: "
  print $ winner' votes

  --Displays the quota
  print "Quota: "
  print quota

  --Shows the winners of the election along with their vote weight (note only shows the first two, but with correct weights)
  print "Election Winners: "
  print $ driver quota (votesSplitAddWeight votes) candidates
