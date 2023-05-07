## To aggregrate data based on score patterns:

## We make a function, which takes the dataset and the total number of
## observations
score.pattern.aggr <- function(data, n){
  ## Aggregated data will be data, where:
  agg_data <- data %>% 
    ## It is turned into a data frame
    as.data.frame(.) %>%
    ## And then a vector of 1s is added to all observations
    mutate(fr = rep(1, n)) %>%
    ## And the data is aggregated on this vector of 1s
    ## through aggregate based on the sum function
    aggregate(fr ~ ., data = ., sum)
  
  ## And it should return this aggregrated data
  return(agg_data)
  
}