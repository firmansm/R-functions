tastingtea <- function(ac=8, tc=4){
  apc <- choose(n=ac, k=tc) #all possible combinations for the test 
  
  #test: ask the lady to indentify cups with milk first or tea first (works either way)
  #all possible outcomes under the null hypothesis
  correct <- seq(from=0, to=tc) #number of correct cups
  incorrect <- tc-correct #number of incorrect cups
  
  #create a probability matrix then loop it
  prob <- matrix(NA, nrow=length(correct), ncol=2)
  colnames(prob) <- c("correct", "probability")
  for(i in 1:nrow(prob)) {
    prob[i,1] <- correct[i]
    prob[i,2] <- (choose(n=tc, k=correct[i]) * choose(n=tc, k=incorrect[i]) ) / apc
  }
  
  #create a barplot for the probability matrix
  viz <- barplot(prob[,2], xlab="correct guest", ylab="probabilty", main="Probability Distribution Under Ho", names.arg = correct)
  message <- "Please ignore the second list. See the barplot instead."
  
  return(list(prob, viz, message))
}

#example
par(mfrow=c(2,2))
tastingtea()
tastingtea(20,10)
tastingtea(60,30)
tastingtea(100,50)
