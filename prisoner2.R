game <- function(dec1,dec2){
  if(dec1 == 1 && dec2 == 1){
    return(c(3,3))
  }
  if(dec1 == 1 && dec2 == 0){
    return(c(5,0))
  }
  if(dec1 == 0 && dec2 == 1){
    return(c(0,5))
  }
  if(dec1 == 0 && dec2 == 0){
    return(c(1,1))
  }
}

decision.1 <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  decision <- sample(c(0,1), size=1, replace = TRUE)
  return(decision)
}
decision.2 <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  decision <- sample(c(0,1), size=1, replace = TRUE)
  return(decision)
}
decision.friendship <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  return(1)
}
decision.betrayal <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  return(0)
}
decision.tft <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  if (nrow(past.results) == 0){
    return(1)
  }
  if (past.results[nrow(past.results), 1]==3 | past.results[nrow(past.results), 1]==5){
    return(1)
  }
  if (past.results[nrow(past.results), 1]==0 | past.results[nrow(past.results), 1]==1){
    return(0)
  }
}

####
# WSLS STRATEGY (Silagi, Leers)
decision.WSLS <- function(past.moves = c(0,0),past.results = c(0,0),current.payoffs = c(0,0)){
  if (nrow(past.results) == 0){
    return(1)
  }
  if (past.results[nrow(past.results), 1]==3){
    return(1)
  }
  if (past.results[nrow(past.results), 1]==5){
    return(0)
  }
  if (past.results[nrow(past.results), 1]==0){
    return(0)
  }
  if (past.results[nrow(past.results), 1]==1){
    return(0)
  }
    
}
####


practice.seq <- function(dec1,dec2){
  moves <- data.frame(matrix(ncol = 2, nrow = 0))
  results <- data.frame(matrix(ncol = 2, nrow = 0))
  total_payoff <- c(0,0)
  
  for(i in seq(0:200)){
    decision1 <- dec1(moves, results,total_payoff)
    decision2 <- dec2(c(moves[1,],moves[0,]), c(results[1,],results[0,]),c(total_payoff[1,],total_payoff[0,]))
    result <- game(decision2, decision1)
    total_payoff = total_payoff + result
    moves <- rbind(moves, c(decision1,decision2))
    results <- rbind(results, result)
  }
  print(sum(results[,1]))
  print(sum(results[,2]))
  return(moves)
}
print(practice.seq(dec1 = decision.WSLS, dec2 = decision.2))