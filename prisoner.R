# CHOOSE YOUR STRATEGIES AND NUMBER OF GAMES
####
STRAT1 <- tit_for_tat1
STRAT2 <- random_choice
NUM_GAMES <- 200
####

# Define payoff matrices
mat1.data <- c(3,0,5,1)
mat1 <- matrix(mat1.data,nrow=2,ncol=2,byrow=TRUE)
mat1

mat2.data <- c(3,5,0,1)
mat2 <- matrix(mat2.data,nrow=2,ncol=2,byrow=TRUE)
mat2


# Define strategies
random_choice <- function(prevpay){
  
  choice <- sample(1:2, 1)
  return(choice)
}

always_cooperate <- function(prevpay){
  
  return(1)
}

always_defect <- function(prevpay){
  
  return(2)
}

tit_for_tat1 <- function(prevpay){
  
  # If opponent cooperated last time
  if (prevpay[1]==3 | prevpay[1]==5){
    return(1)
  }
  
  # If opponent defected last time
  if (prevpay[1]==0 | prevpay[1]==1){
    return(2)
  }
}

grim1 <- function(prevpay){
  
  # If we defected last time
  if (prevpay[1]==1 | prevpay[1]==5){
    return(2)
  } else{
    return(1)
  }
}


# Define game
game <- function(strategy1, strategy2, mat1, mat2, prevpay){
  
  choice1 <- strategy1(prevpay)
  choice2 <- strategy2(prevpay)
  
  payoff1 <- mat1[choice1, choice2]
  payoff2 <- mat2[choice1, choice2]
  
  payoffs <- c(payoff1, payoff2)
  
  return(payoffs)
}


# Play the game NUM_GAMES times
payoffs <- c(5,5)
sum1 <- 0
sum2 <- 0
for (i in 1:NUM_GAMES){
  payoffs <- game(STRAT1, STRAT2, mat1, mat2, payoffs)
  print(payoffs)
  sum1 <- sum1 + payoffs[1]
  sum2 <- sum2 + payoffs[2]
  
  # if (payoffs[1] == 3 & payoffs[2] == 3){
  #   print('coop, coop')
  # }
  # if (payoffs[1] == 0 & payoffs[2] == 5){
  #   print('coop, deft')
  # }
  # if (payoffs[1] == 5 & payoffs[2] == 0){
  #   print('deft, coop')
  # }
  # if (payoffs[1] == 1 & payoffs[2] == 1){
  #   print('deft, deft')
  # }
  
}

'Player1 total payoff:'
sum1
'Player2 total payoff:'
sum2
