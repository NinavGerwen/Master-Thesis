## These three functions perform the necessary matrix multiplication
## to simulate data for three Item Response Theory models given
## parameter values: the one- (1PL) two- (2PL) and three- (3PL)
## parameter logistic models


## For the 1PL:
one.pl <- function(theta, beta){
  ## Probabilities are calculated through the following formula
  prob <- exp(theta - beta) / (1 + exp(theta - beta))
  return(prob)
}

## For the 2PL:
two.pl <- function(theta, alpha, beta){
  ## Probabilities are calculated through the following formula
  prob <- exp((alpha * theta) + beta) / (1 + exp((alpha * theta) + beta))
  return(prob)
}

## For the 3PL:
three.pl <- function(theta, alpha, beta, gamma){
  ## Probabilities are calculated through the following formula
  prob <- gamma + (1 - gamma) * (exp((alpha * theta) + beta) / (1 + exp((alpha + theta) + beta)))
  return(prob)
}
