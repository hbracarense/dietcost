#' Linked foods check
#' 
#' Checks if lower bound linked foods serves are lower or equal to higher bound linked foods serves.
#' @param df Random meal plan.
#' @param low Vector of lower bound food IDs.
#' @param high Vector of higher bound food IDs.
#' @return Differences dataframe.
#' @export
checkLinkedFoods <- function(df, low, high){
  standard_name_check(df, 'food_id', 'serves')
  ls <- 0
  hs <- 0
  for(i in 1:length(low)){
    ls <- ifelse((low[i] %in% df$food_id),
                 {ls + df$serves[df$food_id == low[i]]},
                 ls)
  }
  for(i in 1:length(high)){
    hs <- ifelse((high[i] %in% df$food_id),
                 {hs + df$serves[df$food_id == high[i]]},
                 hs)
  }
  net <- hs - ls
  return(net)
}