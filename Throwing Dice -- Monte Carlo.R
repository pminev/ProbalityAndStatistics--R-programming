library("ggplot2")

throwDiceNTimes<- function(n)
{
  #Make all combinations of 3 dice
  x <- expand.grid(1:6, 1:6, 1:6)
  #Sum all combinations 
  sums <- rowSums(x)
  #number of all sums are 216
  len <- length(sums)
  #Vector with probabilities for throw every sum
  probabilities <- sapply(unique(sums), function(x) sum(sums == x)) / 216
  #return sum value between 3 and 18 with every probabilies
  return (sample(3:18,size = n, prob = probabilities, replace = TRUE))
}

throwDiceMonteCarlo<-function(n)
{
  #generate 3 dice then make their sum and repeat that n-times
  return (replicate(n,sum(sample(1:6, size = 3, replace = TRUE))))
}



graphical<- function(n)
{
  sim<-throwDiceNTimes(n)
  monteCarloSim<-throwDicesMonteCarlo(n)
  
  analyticFrame <- data.frame(dice = sim)
  monteCarloFrame <- data.frame(dice = monteCarloSim)
  
  analyticFrame$method    <- 'Analytic'
  monteCarloFrame$method <- 'Monte-Carlo'

  combined = rbind(analyticFrame, monteCarloFrame)
  ggplot(combined, aes(dice, fill=method)) + geom_histogram(alpha=0.5, position='identity', binwidth=1) +
    ggtitle("Throwing 3 dice") + labs(x="Sum of dice", y="Count of each sum occurence", fill="Method used")
}

