####
# WSLS STRATEGY
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