## Pearson's chisquare test can be calculated through the following function
## for our simulation study

## The function takes as input the model and test length
pearson.test <- function(model, k){
  
  ## For pearson's chisquare test, we need to get the expected frequencies
  ## of all possible scorepatterns (not just the observed ones that
  ## factor.scores() usually gives)
  
  ## Therefore, we first create a dataframe that contains all possible
  ## combinations of ordered 0s and 1s with expand.grid
  scorepatterns <- matrix(NA, nrow = 2, ncol = k)
  
  for(d in 1:k){
    scorepatterns[1:2, d] <- c(0, 1)
  }
  
    scorepatterns <- scorepatterns %>%
      as.data.frame(.) %>%
      expand.grid(.)
    
    ## Then, we obtain the observed and expected frequencies of all these
    ## possible scorepatterns
    obs_exp <- factor.scores(model, resp.patterns = scorepatterns)$score.dat[, (k+1):(k+2)]
  
    ## And we calculate the chisquare statistic value through its formula
    chisq_value <- sum( ( (obs_exp$Obs - obs_exp$Exp)^2 / obs_exp$Exp ) )
    
    ## And from this, we obtain the pvalue
    ## where the degrees of freedom is equal to the
    ## total number of possible scorepatterns minus one and minus
    ## the number of parameters in the model (which for the 2PL is 2 * test length)
    p_value <- 1 - pchisq(q = chisq_value, df = 2^k - 1 - 2*k)
  
  ## Finally, return the pvalue
  return(p_value)
}
