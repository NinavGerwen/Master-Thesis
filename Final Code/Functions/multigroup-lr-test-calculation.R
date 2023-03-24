## To calculate the chisquare statistic of the MG LR test when
## groups are based on actual differences, we created the following function

## This function takes the same arguments as its randomised counterpart
MG.LR.test <- function(l_0, data, g, k, start.val = NULL){
  
  ## First, make the data into a dataframe, as we want to change colnames
  temp_data <- data %>% as.data.frame(.)
  
  ## Name the final column, which contains the group indicator "split"
  colnames(temp_data) <- c(1:k, "split")
  
  ## Make space for the LR values in each group
  split_LR_values <- rep(NA, g)
  
  ## Then, calculate the LR values in each group with a for loop, where
  for(j in 1:g){
    
    ## First, observation for each group is subsetted
    split_set <- subset(temp_data, split == as.numeric(j), -split)
    
    ## And then the 2PL model is fitted and its loglikelihood obtained
    split_LR_values[j] <- ltm(split_set ~ z1, IRT.param = TRUE, 
                              start.val = start.val)$log.Lik
  }
  
  ## Finally, calculate the chisquare statistic
  LR_value <- 2 * ((sum(split_LR_values, na.rm =  TRUE)) - l_0)
  ## And return this value
  return(LR_value)
}
