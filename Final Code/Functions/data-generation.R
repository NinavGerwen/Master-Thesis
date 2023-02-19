## This function generates dichotomous IRT data of sample size n
## with test length k, according to three IRT models (1PL, 2Pl, 3PL)
data.gen <- function(n, k, model = "1PL"){
  
  ## First, create a matrix of thetas (person parameters) by sampling
  ## from a standard normal distribution
  theta <- matrix(data = rep((rnorm(n)), k), ncol = k)
  
  ## Then, create a matrix of betas (difficulty parameters) by
  ## repeating the wanted values and making a matrix out of them
  ## here, the beta values chosen are -1, -0.5, 0, etc.
  beta <- matrix(data = rep(c(-1, -0.5, 0, 0.5, 1), k), nrow = n, ncol = k, byrow = TRUE)
  
  ## If the chosen model is the 1PL: use function one.pl to
  ## properly calculate the probabilities for every person given every item
  if(model == "1PL"){
    Z <- one.pl(theta = theta, beta = beta)
    
  }
  ## If the chosen model is the 2PL: use function two.pl to
  ## properly calculate the probabilities for every person given every item
  if(model == "2PL"){
    
    ## The 2PL also has alphas in the equation (scaling parameter)
    ## Therefore, these also need to be created
    if(k == 5){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3), ncol = k,
                      nrow = n, byrow = TRUE)
    }
    if(k == 10){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1,
                               0.85, 0.7), ncol = k, nrow = n,
                      byrow = TRUE)
    }
    if(k == 20){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1,
                               0.85, 0.7, 1.15, 1.3, 0.7, 0.85, 1,
                               1, 0.7, 1.3, 0.85, 1.15), ncol = k, nrow = n,
                      byrow = TRUE)
    } else {
      alpha <- matrix(data = rep(c(0.7, 0.85, 1, 1.15, 1.3), k), ncol = k,
                      nrow = n, byrow = TRUE)
    }
    
    
    Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
    
  }
  ## If the chosen model is the 3PL: use function three.pl to
  ## properly calculate the probabilities for every person given every item
  if(model == "3PL"){
    
    if(k == 5){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3), ncol = k,
                      nrow = n, byrow = TRUE)
    }
    if(k == 10){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1,
                               0.85, 0.7), ncol = k, nrow = n,
                      byrow = TRUE)
    }
    if(k == 20){
      alpha <- matrix(data = c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1,
                               0.85, 0.7, 1.15, 1.3, 0.7, 0.85, 1,
                               1, 0.7, 1.3, 0.85, 1.15), ncol = k, nrow = n,
                      byrow = TRUE)
    } else {
      alpha <- matrix(data = rep(c(0.7, 0.85, 1, 1.15, 1.3), k), ncol = k,
                      nrow = n, byrow = TRUE)
    }
    
    ## The 3PL has gammas in the equation (pseudo-guessing parameter),
    ## therefore we create these.
    gamma <- matrix(data = 0.25, ncol = k, 
                    nrow = n, byrow = TRUE)
    
    Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
  }
  
  ## Finally, sample from a binomial distribution, given the probability
  ## matrix that we had calculated earlier
  data <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), ncol = k, nrow = n)
  
  ## And return this matrix
  return(data)
  
}

## Note that we create matrices of the parameters in order to be able to use
## the hadamard product instead of matrix multiplication through %*%