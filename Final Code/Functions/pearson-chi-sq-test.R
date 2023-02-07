
pearson.test <- function(model, k){
  
  scorepatterns <- matrix(NA, nrow = 2, ncol = k)
  
  for(d in 1:k){
    scorepatterns[1:2, d] <- c(0, 1)
  }
  
    scorepatterns <- scorepatterns %>%
    as.data.frame(.) %>%
    expand.grid(.)
    
    obs_exp <- factor.scores(model, resp.patterns = scorepatterns)$score.dat[, (k+1):(k+2)]
  
    chisq_value <- sum( ( (obs_exp$Obs - obs_exp$Exp)^2 / obs_exp$Exp ) )
    
    p_value <- 1 - pchisq(q = chisq_value, df = 2^k - 1 - 2*k)
  
  return(p_value)
}

