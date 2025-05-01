#' Random deletion
#'
#' Randomly deletes a food.
#' @param df Dataframe.
#' @param column Column from which decision about removal of values will be made.
#' @param condition Condition that, if is true, will enable radom removal.
#' @return Random meal plan
random_plan <- function(df, column, condition){
  random_parameter <- 0.4
  for(i in 1:nrow(df)){
    if(unlist(df[i, column]) %in% condition){
      selector <- runif(1)
      if(selector > random_parameter){
        df <- df[-i,]
      }
    }
  }
  return(df)
}