## method options are theta or param
## n is here group size, no longer total n
multigroup.data.gen <- function(n, k, g, method = "theta", model = "2PL"){
  
  if(method == "theta"){
    
    data <- matrix(data = NA, nrow = n * g, ncol = k + 1)
    
    beta <- matrix(data = rep(c(-1, -0.5, 0, 0.5, 1), k), nrow = n, ncol = k, byrow = TRUE)
    
    counter <- 1
    
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
    
  for(i in 1:g){

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
    
    data <- matrix(data = NA, nrow = n * g, ncol = k + 1)
    
    counter <- 1
    
    for(i in 1:g){
      theta <- matrix(data = rep((rnorm(n)), k), ncol = k)
      
      beta <- matrix(data = rep( (c(-1, -0.5, 0, 0.5, 1) - ((as.numeric(i) -  1)/ 2)) , k), 
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
