## To calculate the fit indices the Tucker-Lewis Index and Comparative Fit Index,
## we require a baseline model and saturated model to test a model against

## The loglikelihood of the base model can be calculated with the following 
## function, which only takes the data as input
base.model <- function(data) {
  
  k <- ncol(data)
  n <- nrow(data)
  
  ## Create an empty vector of length equal to test length
  pi <- rep(NA, k)
  
  ## Then, for every column/item:
  for(i in 1:k) {
    
    ## Determine the number of people who scored the item correctly
    n_i <- sum(data[, i] == 1)
    
    ## Determine the proportion of people who scored the item correctly
    pi_i <- mean(data[, i])
    
    ## Determine the likelihood that this many items people
    ## scored the item correctly
    pi[i] <- n_i * log(pi_i) + n * log((1 - pi_i)) - n_i * log((1 - pi_i))
    
  }
  
  ## The total loglikelihood is the sum of all these logs
  loglik <- sum(pi)
  
  
  ## Return the log of the likelihood
  return(loglik)
  
}

## The loglikelihood of the saturated model can be calculated through
## the following function, which takes as input the 
## score-pattern frequency aggregrated data and the total number of observations
sat.model <- function(agg_data, n_r) {
  
  ## Determine the number of observed score patterns
  n <- nrow(agg_data)
  
  ## Create an empty vector of length n
  pi <- rep(NA, n)
  
  ## Then, for every observed scorepattern
  for(i in 1:n) {
    ## get the number of times it has been observed
    n_x <- agg_data$fr[i]
    
    ## Get the relative frequency of the score pattern
    pi[i] <- log((n_x / n_r)^n_x)
    
  }
  
  ## Then, the log likelihood is calculated by taking the log of the
  ## product of all relative frequencies
  loglik <- sum(pi)


  ## print(c("The saturated likelihood is ", loglik, ".") ) 
  ## And this value should be returned  
  return(loglik)
  
}



## Then, to calculate the TLI and CFI, we have a function which takes as input:
## the loglikelihood of the tested model, the dataset,
## the aggregated dataset, number of observations and test length
TLI <- function(testedlog, dataset, agg_data, n, k){

  ## First, calculate the chisquare statistic between
  ## the saturated and hypothesized model
  chi_tested <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))
  
  ## And the degrees of freedom of the hypothesized model
  df_tested <- 2*k
  
  ## And the chisquare statistic between the saturated and baseline model
  chi_base <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))
  
  ## And the degrees of freedom of the baseline model
  df_base <- k
  
  ## Then, calculate the TLI according to the right formula
  numerator <- chi_tested / df_tested
  
  denominator <- chi_base / df_base
  
  fit_value <- 1 - (numerator/denominator)
  
  ## And return this value
  return(fit_value)
  
  
}

## Then, to calculate the CFI, the same is done with a slightly different
## formula
CFI <- function(testedlog, dataset, agg_data, n, k){
  
  chi_tested <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))
  
  df_tested <- 2 * k
  
  chi_base <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))
  
  df_base <- k
  
  numerator <- chi_tested - df_tested
    
  denominator <- chi_base - df_base
    
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
}

TLI.Yang <- function(testedlog, dataset, agg_data, n, k){
  
  ## First, calculate the chisquare statistic between
  ## the saturated and hypothesized model
  chi_tested <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))
  
  ## And the degrees of freedom of the hypothesized model
  df_tested <- 2*k
  
  ## And the chisquare statistic between the saturated and baseline model
  chi_base <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))
  
  ## And the degrees of freedom of the baseline model
  df_base <- k
  
  ## Then, calculate the TLI according to the right formula
  numerator <- (chi_base/df_base) - (chi_tested/df_tested)
  
  denominator <- (chi_base / df_base) -  1
  
  fit_value <- (numerator/denominator)
  
  ## And return this value
  return(fit_value)
  
  
}

CFI.Yang <- function(testedlog, dataset, agg_data, n, k){
  
  chi_tested <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))
  
  df_tested <- 2 * k
  
  chi_base <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))
  
  df_base <- k
  
  numerator <- max(c((chi_tested - df_tested), 0))
  
  denominator <- max(c(chi_tested - df_tested), (chi_base - df_base), 0)
  
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
}