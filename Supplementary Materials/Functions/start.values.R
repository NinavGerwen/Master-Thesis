## To let our simulation study run faster, we want to ltm function to
## have start values that are equal to the true parameter values

## Therefore, we created a function that creates the correct specification
## of start values according to each test length for the ltm model

start.values <- function(k){
  if(k == 5){
    start_vals <- matrix( c(-1, -0.5, 0, -0.5, 1, 0.7, 0.85, 1, 1.15, 1.3), nrow = 5, ncol = 2)
  }
  if(k == 10){
    start_vals <- matrix( c(-1, -0.5, 0, -0.5, 1, -1, -0.5, 0, -0.5, 1, 
                            0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1,
                            0.85, 0.7), nrow = 10, ncol = 2)
  }
  if(k == 20){
    start_vals <- matrix( c(-1, -0.5, 0, -0.5, 1, -1, -0.5, 0, -0.5, 1, 
                            -1, -0.5, 0, -0.5, 1, -1, -0.5, 0, -0.5, 1,
                            0.7, 0.85, 1, 1.15, 1.3, 1.3, 1.15, 1, 0.85, 
                            0.7, 1.15, 1.3, 0.7, 0.85, 1, 1, 0.7, 1.3, 
                            0.85, 1.15), nrow = 20, ncol = 2)
  }
  
  return(start_vals)
}