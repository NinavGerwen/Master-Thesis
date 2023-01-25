LR.test <- function(l_0, data, model = "1PL", g = 2){
  
  temp_data <- as.data.frame(data)
  temp_data$split <- sample(1:g, nrow(temp_data), replace = TRUE)
  
  split_LR_values <- rep(NA, g)
  
  for(j in 1:g) {
    split_set <- subset(temp_data, split == as.numeric(j), select = -split)
    if(model == "1PL"){
      
      split_LR_values[j] <- rasch(split_set)$log.Lik
    }
    
    if(model == "2PL"){
      
      
      split_LR_values[j] <- ltm(split_set ~ z1, IRT.param = TRUE)$log.Lik
    }
    if(model == "3PL"){
      
      
      split_LR_values[j] <- tpm(split_set,
                                start.val = "random")$log.Lik
    }
  }
  
  LR_value <- 2 * ((sum(split_LR_values, na.rm =  TRUE)) - l_0)
  return(LR_value)
}