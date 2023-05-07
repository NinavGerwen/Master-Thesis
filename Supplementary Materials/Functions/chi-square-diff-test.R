## The following function is to perform a chisquare difference test
## between the 2PL and 3PL through fitting the 3PL with the tpm function from
## the ltm package

## The function requires as input the loglikelihood of the null model,
## the dataset, and the test length
diff.test <- function(l_0, dataset, k){
  
  ## Fit the 3PL from the ltm package
  l_a <- tpm(dataset, max.guessing = 0.25, start.val = "random",
             control = list(iter.qN = 5000, GHk = 40))$log.Lik
  
  ## Calculate the chisquare value
  chisqvalue <- 2 * (l_a - l_0)
    
  ## Get the p-value of the chisquare value, with degrees of freedom
  ## equal to the test length
  p.value <- 1 - pchisq(chisqvalue, df = k)
  
  ## Return this p-value
  return(p.value)
}

## This following function performs a chisquare difference test between
## the 2PL and 3PL through fitting the 3PL with the mirt function from
## the mirt package

## The function requires as input the loglikelihood of the null model,
## the dataset, and the test length
mirt.diff.test <- function(l_0, dataset, k){
  
  ## Fitting the 3PL model and gaining the loglikelihood of the alternative model
  l_a <- dataset %>% 
            as.data.frame(.) %>%
            mirt(data = ., model = 1, itemtype = "3PL", optimizer = "nlminb", 
                 TOL = .0001, control = list(maxit = 1000),
                 verbose = FALSE) %>% logLik(.)
  
  ## Calculating the chisquare value
  chisqvalue <- 2 * (l_a - l_0)
  
  ## Calculating the p-value
  p.value <- 1 - pchisq(chisqvalue, df = k)
  
  ## Return this p-value
  return(p.value)
}

## Finally, this function performs a chisquare difference test between
## the full 2PL model, and the 3PL model with the constraint that
## the gamma parameter has to be equal for every item

equal.mirt.diff.test <- function(l_0, dataset, k){
  
  ## The function works in a similar way as above, however, we now
  ## also put a constraint on the list of items that states which parameters
  ## have to be equal
      ## Which parameters have to be equal depends on the test length, 
      ## but it is the third parameter in the list of parameters according
      ## to the mirt package, and every 4 parmaters after this
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
  
  ## And for the rest, the same happens as described above
  chisqvalue <- 2 * (l_a - l_0)
  
  p.value <- 1 - pchisq(chisqvalue, df = 1)
  
  return(p.value)
}
