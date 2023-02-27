MG.LR.test <- function(l_0, data, g, k, start.val = NULL){
  
  temp_data <- data %>% as.data.frame(.)
  
  colnames(temp_data) <- c(1:k, "split")
  
  split_LR_values <- rep(NA, g)
  
  for(j in 1:g){
    
    split_set <- subset(temp_data, split == as.numeric(j), -split)
    
    split_LR_values[j] <- ltm(split_set ~ z1, IRT.param = TRUE, 
                              start.val = start.val)$log.Lik
  }
  
  LR_value <- 2 * ((sum(split_LR_values, na.rm =  TRUE)) - l_0)
  return(LR_value)
}
