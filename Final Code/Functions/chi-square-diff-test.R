
diff.test <- function(l_0, dataset, k){
  
  l_a <- tpm(dataset, max.guessing = 0.25, start.val = "random",
             control = list(iter.qN = 5000, GHk = 40))$log.Lik
  
  chisqvalue <- 2 * (l_a - l_0)
    
  p.value <- 1 - pchisq(chisqvalue, df = k)
  
  return(p.value)
}

mirt.diff.test <- function(l_0, dataset, k){
  l_a <- dataset %>% 
            as.data.frame(.) %>%
            mirt(data = ., model = 1, itemtype = "3PL", optimizer = "nlminb", 
                 TOL = .0001, control = list(maxit = 1000),
                 verbose = TRUE) %>% logLik(.)
  
  chisqvalue <- 2 * (l_a - l_0)
  
  p.value <- 1 - pchisq(chisqvalue, df = k)
  
  return(p.value)
}
