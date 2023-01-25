
pearson.test <- function(agg_data, k){
  
  all_obs <- FALSE
  ## sample until nrow(unique(test)) == 2^k
  extra_data <- data.gen(100000, k = k)
  
  while(all_obs == FALSE){
    
    extra_data <- rbind(extra_data, data.gen(100000, k = k))
    
    score_patterns <- nrow(unique(extra_data))
    
    if(score_patterns == 2^k){
      all_obs <- TRUE
    } 
    next
  }
  
  
  ##obs <- ...
  
  ##exp <- ...
  
  ## sum( ((obs - exp)^2 / exp) )
  
  return(extra_data)
}

