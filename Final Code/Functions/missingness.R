## If you wish to get random missing data in your simulated matrix, 
## you can use the following function which takes the argument of your
## dataset and the percentage (in decimals) of missing data you would like

missify <- function(dataset, missingness){
  n <- nrow(dataset)
  p <- ncol(dataset)
  
  ## Then, for every row and column element:
  for(i in 1:n){
    for(j in 1:p){
      
      ## Sample a random value from a uniform distribution from 0 to 1
      temp_value <- runif(n = 1, min = 0, max = 1)
      
      ## And if this value is smaller than the missingness rate desired
      if(temp_value <= missingness){
        ## Replace the item with an NA
        dataset[i, j] <- NA
        ## Otherwise go to the next row-column element and repeat
      } else {
        next
      }
     }
  }
  
  return(dataset)
}
 