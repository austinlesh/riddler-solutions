# Link to puzzle: https://fivethirtyeight.com/features/can-the-hare-beat-the-tortoise/

set.seed(1)

season_BA = function(num_bats, num_games, true_BA){
  total_bats = num_bats*num_games
  hits = 0
  for(i in 1:total_bats){
    result = runif(1)
    if(result <= true_BA) hits = hits + 1
  }
  return(hits/total_bats)
}

results_60 = replicate(100000, season_BA(4, 60, 0.350), simplify = TRUE) #simulate 100,000 seasons

sum(results_60 >= .400)/100000 #probability of batting .400 over 60 games
# = 0.0613 with this seed


results_162 = replicate(100000, season_BA(4, 162, 0.350), simplify = TRUE)

sum(results_162 >= .400)/100000 #probability of batting .400 over 162 games
# = 0.00371


# So it's still unlikely to bat .400 in a 60-game season, but it is 16x more likely
# than in a 162-game season.

#######

# Extra credit:

# This function constructs a vector for the season where each element
# is a game. Each game gets a 0 if there are no hits or a 1 if there are
# one or more hits. Then it searches the vector for a sequence of hits
# and returns TRUE if that sequence is found, otherwise FALSE.
consec_hits <- function(num_bats, num_games, true_BA, target_games){
  season = vector()
  for(i in 1:num_games){
    bat_results <- runif(num_bats)
    if(any(bat_results <= true_BA)){ # if any hits
      hits = 1 # 1+ hits
    } else {
      hits = 0 # 0 hits
    }
    season = c(season, hits)
  }
  
  goal = rep(1, target_games) # sequence we're going to look for
  result = grepl(paste(goal, collapse = ';'), paste(season, collapse = ';'))
  if(result == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

consec_test_60 <- replicate(100000, consec_hits(4, 60, .350, 56), simplify = TRUE)
consec_test_162 <- replicate(100000, consec_hits(4, 162, .350, 56), simplify = TRUE)

sum(consec_test_60)/100000 # 0.00003 with this seed
sum(consec_test_162)/100000 # 0.00031

# 56-game hitting streak is about 10 times more likely in the longer season.

########

# Extra extra credit:

hits_and_400 <- function(num_bats, num_games, true_BA, target_games){
  season = vector()
  total_hits = 0
  for(i in 1:num_games){
    bat_results <- runif(num_bats)
    if(any(bat_results <= true_BA)){ # if any hits
      hits = 1 # 1+ hits
    } else {
      hits = 0 # 0 hits
    }
    season = c(season, hits)
    
    total_hits = total_hits + sum(bat_results <= true_BA) 
  }
  
  goal = rep(1, target_games) # sequence we're going to look for
  consec_result = grepl(paste(goal, collapse = ';'), paste(season, collapse = ';'))
  if(consec_result == TRUE && total_hits/(num_bats*num_games) >= 0.400){ # has minimum number of consecutive games and
                                                                         # target batting average
    return(TRUE)
  } else {
    return(FALSE)
  }
}

sum(replicate(1000000, hits_and_400(4, 60, .350, 56), simplify = TRUE))/1000000
# 1.5e-5 on this seed. pretty unlikely.