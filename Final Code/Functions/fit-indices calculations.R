## To calculate the fit indices the Tucker-Lewis Index and Comparative Fit Index,
## we require a baseline model and saturated model to test a model against

## The loglikelihood of the base model can be calculated with the following 
## function, which takes as input the data, sample size and test length
base.model <- function(data, n, k) {
  
  ## How the loglikelihood is calculated is as follows: 
  
  ## Create an empty vector of length equal to test length
  pi <- rep(NA, k)
  
  ## Then, for every column/item:
  for(i in 1:k) {
    
    ## Determine the number of people who scored the item correctly
    n_i <- sum(data[, i])
    
    ## Determine the proportion of people who scored the item correctly
    pi_i <- mean(data[, i])
    
    ## Determine the loglikelihood that this many items people
    ## scored the item correctly
    pi[i] <- n_i * log(pi_i) + n * log((1 - pi_i)) - n_i * log((1 - pi_i))
    
  }
  
  ## The total loglikelihood is then the sum of all these logs
  loglik <- sum(pi)
  
  
  ## Return the loglikelihood
  return(loglik)
  
}

## The loglikelihood of the saturated model can be calculated through
## the following function, which takes as input the 
## score-pattern frequency aggregrated data and the total number of observations
sat.model <- function(agg_data, n) {
  
  ## Determine the number of observed score patterns
  n_r <- nrow(agg_data)
  
  ## Create an empty vector of length equal to this number
  pi <- rep(NA, n_r)
  
  ## Then, for every observed scorepattern
  for(i in 1:n_r) {
    ## get the number of times it has been observed
    n_x <- agg_data$fr[i]
    
    ## Get the log of the relative frequency of the score pattern
    pi[i] <- log((n_x / n)^n_x)
    
  }
  
  ## Then, the log likelihood is calculated by taking the sum of all
  ## logs of relative frequencies
  loglik <- sum(pi)

  ## Return this value
  return(loglik)
  
}

## To calculate the chi-square value between the
## base model and saturated model, we use this function which takes
## as input the aggregated data, normal data, sample size and test length
chi.base <- function(agg_data, data, n, k){
  
  ## Then, the chisquare value is two times the loglikelihood 
  ## of the saturated model minus the loglikelihood of the base model
  ## which can be gained through the functions defined above
  chi_val <- (2 * ((sat.model(agg_data, n)) - base.model(data, n = n, k = k)))
    
  ## Return this value
  return(chi_val)
}

## The degrees of freedom for the base model vs. saturated model function
## which is equal to the formula shown inside
df.base <- function(k){
  return((k * (k + 1)) / 2)
}

## This is function to get the chisquare value of the tested model versus
## the saturated model, it takes as input the loglikelihood of the tested
## model, the aggregated data and sample size
chi.tested <- function(l_0, agg_data, n){
  
  ## Chisquare value is alculated in the following way
  chi_val <- (2 * (sat.model(agg_data, n) - l_0))
    
  ## Return this value
  return(chi_val)
}

## The degrees of freedom for the tested model vs. saturate model function,
## which is equal to the formula shown inside
df.tested <- function(k){
  return(((k * (k + 1))/2) - k)
}


## Then, to calculate the TLI and CFI, we have a function which takes as input:
## the two chisquare values and two degrees of freedom values
TLI <- function(chi_base, df_base, chi_tested, df_tested){
  
  ## It is done by simply calculating the TLI through its formula
  ## as defined in the manuscript
  numerator <- chi_tested / df_tested
  
  denominator <- chi_base / df_base
  
  fit_value <- 1 - (numerator/denominator)
  
  ## And the function should return this value
  return(fit_value)
  
  
}

## Then, to calculate the CFI, the same is done with a slightly different
## formula
CFI <- function(chi_base, df_base, chi_tested, df_tested){
  
  numerator <- chi_tested - df_tested
    
  denominator <- chi_base - df_base
    
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
}

## Finally, to calculate the ICFI, we created a function which takes
## as input the tested ltm model and the test length
exp.obs.FI <- function(model, k){
  
  ## First, obtain the observed and expected frequencies of all
  ## observed score patterns
  obs_exp <- factor.scores(model)$score.dat
  
  ## Subset them correspondingly
  obs <- obs_exp[, (k + 1)]
  
  exp <- obs_exp[, (k + 2)]
  
  ## And calculate the fit index value through matrix multiplication
  numerator <- 2 * t(obs) %*% exp
  
  denominator <- (t(obs) %*% obs) + (t(exp) %*% exp)
  
  value <- numerator / denominator
  
  ## Return this value
  return(value)
}