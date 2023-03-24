## In simulation study 2, we generate data with group differences
## To do that, we created the following function which takes as input:

## the sample size per group wanted, the test length, the number of groups,
## the method of differences wanted (theta or item parameters) and
## the model under which to generate
multigroup.data.gen <- function(n, k, g, method = "theta", model = "2PL"){
  
  ## We want space for the final simulated matrix, so we create this here
  ## the number of columns is k + 1, because we need an extra column for 
  ## the group indicator
  data <- matrix(data = NA, nrow = n * g, ncol = k + 1)
  
  ## If method is theta, we want differences based on their latent variable
  if(method == "theta"){
    
    ## Therefore, the itemparameters can be the sample for everyone, which
    ## we define here in matrix beta, alpha, and gamma
    beta <- matrix(data = rep(c(-1, -0.5, 0, 0.5, 1), k/5), nrow = n, ncol = k, byrow = TRUE)
    
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
    }
    
    if(model == "3PL"){
      gamma <- matrix(data = 0.25, ncol = k, nrow = n, byrow = TRUE)
    }
    
    ## Now, to generate data we will work with a for loop for every group
    
    ## Therefore, we keep a counter for indexing
    counter <- 1
    
    ## Then, for every group:
    for(i in 1:g){
      
      ## Generate latent variables theta in a matrix with differences of 0.25
      ## compared to the group before
      theta <- matrix(data = rep((rnorm(n) - ((as.numeric(i) - 1)/4)), k), ncol = k)
      
      ## And calculate the matrix of probabilities according to the 2PL and 3PL model
      if(model == "2PL"){
        
        Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
        
      }
      
      if(model == "3PL"){
        
        Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
        
      }
      
      ## Then, through rbinom and the earlier created matrix of probabilities,
      ## we sample 0s and 1s and put this in the correct place in the 
      ## earlier created dataset
      data[counter:(counter + n - 1) , 1:k] <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), 
                                                      ncol = k, nrow = n)
      
      ## And in the final column, we put a group indicator
      data[counter:(counter + n - 1), (k + 1)] <- i
      
      ## And we increase the counter by n so that we can subset properly again
      ## in the next loop
      counter <- counter + n
      
    }
    
    
  }
  
  ## If the method is param, it means we want differences in the item parameters
  ## between each group
  if(method == "param"){
    
    counter <- 1
    
    for(i in 1:g){
      ## So for each group, we can now sample thetas from the same distribution
      theta <- matrix(data = rep((rnorm(n)), k), ncol = k)
      
      ## But the betas and alphas have to be lowered by 0.25 and 0.10 respectively for
      ## each consecutive group
      beta <- matrix(data = rep( (c(-1, -0.5, 0, 0.5, 1) - ((as.numeric(i) -  1)/4)) , k/5), 
                     nrow = n, ncol = k, byrow = TRUE)
      
      if(k == 5){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3) - ((as.numeric(i) - 1)/10)), ncol = k,
                        nrow = n, byrow = TRUE)
      }
      if(k == 10){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1, 0.85, 0.7) - ((as.numeric(i) - 1)/10)), ncol = k, nrow = n,
                        byrow = TRUE)
      }
      if(k == 20){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1, 0.85, 
                                  0.7, 1.15, 1.3, 0.7, 0.85, 1, 1, 0.7, 1.3, 0.85, 1.15) - ((as.numeric(i) - i)/10)), ncol = k, nrow = n,
                        byrow = TRUE)
      }
      
      ## Calculate the correct model probabilities
      if(model == "2PL"){
        
        Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
        
      }
      
      if(model == "3PL"){
        
        gamma <- matrix(data = 0.25, ncol = k, nrow = n, byrow = TRUE)
        
        Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
      }
      
      ## And again put the sampled values in their correct subsetted place
      data[counter:(counter + n - 1) , 1:k] <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), 
                                                      ncol = k, nrow = n)
      
      data[counter:(counter + n - 1), (k + 1)] <- i
      
      counter <- counter + n
    }
    
    
  }
  
  ## Finally, the function should return this matrix
  return(data)
  
}


## For the final few conditions where we also want gamma to differ
## per group, we created another function which allows this for this.
## It works in the exact same way as the function defined above, except for
## one difference
gamma.diff.data.gen <- function(n, k, g, method = "theta", model = "3PL"){
  
  data <- matrix(data = NA, nrow = n * g, ncol = k + 1)
  
  if(method == "theta"){
    
    beta <- matrix(data = rep(c(-1, -0.5, 0, 0.5, 1), k/5), nrow = n, ncol = k, byrow = TRUE)
    
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
    }
    
    if(model == "3PL"){
      gamma <- matrix(data = 0.25, ncol = k, nrow = n, byrow = TRUE)
    }
    
    counter <- 1
    
    for(i in 1:g){
      
      theta <- matrix(data = rep((rnorm(n) - ((as.numeric(i) - 1)/4)), k), ncol = k)
      
      
      if(model == "2PL"){
        
        Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
        
      }
      
      if(model == "3PL"){
        
        Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
        
      }
      
      data[counter:(counter + n - 1) , 1:k] <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), 
                                                      ncol = k, nrow = n)
      
      data[counter:(counter + n - 1), (k + 1)] <- i
      
      counter <- counter + n
      
    }
    
    
  }
  
  ## For when method is param, and the model = 3PL, the gammas now also lower by .05 per
  ## consecutive group
  if(method == "param"){
    
    counter <- 1
    
    for(i in 1:g){
      theta <- matrix(data = rep((rnorm(n)), k), ncol = k)
      
      beta <- matrix(data = rep( (c(-1, -0.5, 0, 0.5, 1) - ((as.numeric(i) -  1)/2)) , k/5), 
                     nrow = n, ncol = k, byrow = TRUE)
      
      if(k == 5){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3) - ((as.numeric(i) - 1)/8)), ncol = k,
                        nrow = n, byrow = TRUE)
      }
      if(k == 10){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1, 0.85, 0.7) - ((as.numeric(i) - 1)/8)), ncol = k, nrow = n,
                        byrow = TRUE)
      }
      if(k == 20){
        alpha <- matrix(data = (c(0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1, 0.85, 
                                  0.7, 1.15, 1.3, 0.7, 0.85, 1, 1, 0.7, 1.3, 0.85, 1.15) - ((as.numeric(i) - i)/8)), ncol = k, nrow = n,
                        byrow = TRUE)
      }
      
      if(model == "2PL"){
        
        Z <- two.pl(theta = theta, alpha = alpha, beta = beta)
        
      }
      
      if(model == "3PL"){
        
        gamma <- matrix(data = (0.25 - ((as.numeric(i) - 1)/20)), ncol = k, nrow = n, byrow = TRUE)
        
        Z <- three.pl(theta = theta, alpha = alpha, beta = beta, gamma = gamma)
      }
      
      data[counter:(counter + n - 1) , 1:k] <- matrix(data = rbinom(n = n * k, size = 1, prob = Z), 
                                                      ncol = k, nrow = n)
      
      data[counter:(counter + n - 1), (k + 1)] <- i
      
      counter <- counter + n
    }
    
    
  }
  
  return(data)
  
}
