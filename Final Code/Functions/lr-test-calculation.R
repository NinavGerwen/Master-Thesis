LR.test <- function(l_0, data, g = 2, start.val = NULL){ ## for multiple models: add argument model = "1PL"
  
  temp_data <- data %>% as.data.frame(.) %>%
    mutate(split = sample(1:g, nrow(data), replace = TRUE))
  
  split_LR_values <- rep(NA, g)
  
  for(j in 1:g) {
    split_set <- subset(temp_data, split == as.numeric(j), select = -split)
    ##if(model == "1PL"){
      
      split_LR_values[j] <- rasch(split_set)$log.Lik
    ##}
    
    ##if(model == "2PL"){
      
      
    ##  split_LR_values[j] <- ltm(split_set ~ z1, IRT.param = TRUE, 
    ##                            start.val = start.val)$log.Lik
    ##}
    ##if(model == "3PL"){
      
      
    ##  split_LR_values[j] <- tpm(split_set,
    ##                           start.val = "random")$log.Lik
    ##}
  }
  
  LR_value <- 2 * ((sum(split_LR_values, na.rm =  TRUE)) - l_0)
  return(LR_value)
}