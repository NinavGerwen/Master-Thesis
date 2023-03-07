## method options are theta or param
## n is here group size, no longer total n
multigroup.data.gen <- function(n, k, g, method = "theta", model = "2PL"){
  
  ## We want space for the final simulated matrix
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
            
            ## Generate latent variables theta in a matrix with differences
            ## based on the group they are in (group 1 to g)
            theta <- matrix(data = rep((rnorm(n) - (as.numeric(i) - 1)), k), ncol = k)
            
            
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
        
      gamma <- matrix(data = 0.25, ncol = k, nrow = n, byrow = TRUE)
        
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
