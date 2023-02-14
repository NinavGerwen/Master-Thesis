
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
                 verbose = FALSE) %>% logLik(.)
  
  chisqvalue <- 2 * (l_a - l_0)
  
  p.value <- 1 - pchisq(chisqvalue, df = k)
  
  return(p.value)
}

equal.mirt.diff.test <- function(l_0, dataset, k){
  if(k == 5){
    l_a <- dataset %>% 
      as.data.frame(.) %>%
      mirt(data = ., model = 1, itemtype = "3PL", optimizer = "nlminb", 
         TOL = .0001, control = list(maxit = 1000),
         verbose = FALSE, constrain = list(c(3, 7, 11, 15, 19))) %>% logLik(.)
  }
  if(k == 10){
    l_a <- dataset %>% 
      as.data.frame(.) %>%
      mirt(data = ., model = 1, itemtype = "3PL", optimizer = "nlminb", 
           TOL = .0001, control = list(maxit = 1000),
           verbose = FALSE, constrain = list(c(3, 7, 11, 15, 19, 
                                               23, 27, 31, 35, 39))) %>% logLik(.)     
  }
  if(k == 20){
    l_a <- dataset %>% 
      as.data.frame(.) %>%
      mirt(data = ., model = 1, itemtype = "3PL", optimizer = "nlminb", 
           TOL = .0001, control = list(maxit = 1000),
           verbose = FALSE, constrain = list(c(3, 7, 11, 15, 19,
                                               23, 27, 31, 35, 39,
                                               43, 47, 51, 55, 59,
                                               63, 67, 71, 75, 79))) %>% logLik(.)
  }
  
  chisqvalue <- 2 * (l_a - l_0)
  
  p.value <- 1 - pchisq(chisqvalue, df = 1)
  
  return(p.value)
}
