#' Individual/diet mismatch check
#'
#' Checks if all individuals have a matching diet.
#' @param df Dataframe.
#' @examples 
#' check_match_individual_diet(df)
#' @export
check_match_individual_diet <- function(df){
  if(any(is.na(df$individual))){
    stop('There is a diet without a matching individual! Check your data.')
  }
  
  if(any(is.na(df$diet))){
    stop('There is an individual without a matching diet! Check your data.')
  }
  
}
