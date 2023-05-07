## To calculate the chisquare value of the MG LR test under
## random assignment, we created the following function, which
## takes as input the loglikelihood of the null model (i.e., the
## model tested on the whole dataset), the number of groups
## desired, and the start values to speed up convergence
LR.test <- function(l_0, data, g = 2, start.val = NULL){
  
  ## First, we randomly assign groups up to group g through use of the
  ## sample function
  temp_data <- data %>% as.data.frame(.) %>%
    mutate(split = sample(1:g, nrow(data), replace = TRUE))
  
  ## Then, we create a vector of length g that will save the loglikelihoods
  ## for the model when fitted in each randomised group
  split_LR_values <- rep(NA, g)
  
  ## We obtain these loglikelihoods through a for loop from 1 to g
  for(j in 1:g) {
    ## First, we subset the observations that have been put in one specific group
    split_set <- subset(temp_data, split == as.numeric(j), select = -split)

    ## And then we fit the 2PL model and obtain the loglikelihood
     split_LR_values[j] <- ltm(split_set ~ z1, IRT.param = TRUE, 
                               start.val = start.val)$log.Lik

  }
  
  ## Finally, the LR statistic is calculated through the following
  ## formula
  LR_value <- 2 * ((sum(split_LR_values, na.rm =  TRUE)) - l_0)
  
  ## And the function returns this value
  return(LR_value)
}