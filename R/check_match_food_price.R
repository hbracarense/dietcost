#' Food/price mismatch check
#'
#' Checks if all foods have a price.
#' @param df Dataframe.
#' @export
check_match_food_price <- function(df){
  if(any(is.na(df$food_id))){
    stop('There is a food without an ID! Check your data.')
  }
  
  if(any(is.na(df$price))){
    stop('There is a food without a price! Check your data.')
  }
  
}