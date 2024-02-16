birth.df <- data.frame(matrix(ncol = 2, nrow = 50))
colnames(birth.df) <- c('NumPeople', 'Prob')
birth.df$NumPeople <- rep(1:50)

#numpeople <- c()
#prob <- c()


for (people in 1:50){
  count <- 0
  for (sims in 1:10000){
    births <- sample(365, people, replace=TRUE)
    if (any(duplicated(births))){
      count <- count+1
    }
  #birth.df$NumPeople <- people
  birth.df$Prob[people] <- count / 10000
  #numpeople <- append(numpeople, people)
  #prob <- append(prob, count/1000)
  }
  
  #plot(numpeople, prob)
}
plot(birth.df$NumPeople, birth.df$Prob, type='l', 
     xlab='Number of People', ylab='Probability of Same Birthday')
