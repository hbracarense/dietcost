#' Standard name check
#'
#' Checks if variable names are the standard defined into DIETCOST R standard table.
#' @param df Dataframe.
#' @param ... Any number of strings.
#' @examples 
#'standard_name_check(foods, 'food_id', 'food_name')
#' @export
standard_name_check <- function(df,...){
  variables <- list(...)
  for(variable in variables){
    if(!(variable %in% colnames(df))){
      stop("Column names don't match standard. Check your data!")
    }
  }
}