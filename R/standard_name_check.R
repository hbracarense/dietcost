#' Standard name check
#'
#' Checks if variable names are the standard defined into DIETCOST R standard table.
#' @param df Dataframe.
#' @examples 
#'DIETCOST::standard_name_check(DIETCOST::foods, 'food_id', 'food_name')
#' @export
standard_name_check <- function(df,...){
  variables <- list(...)
  for(variable in variables){
    if(!(variable %in% colnames(df))){
      stop("Column names don't match standard. Check your data!")
    }
  }
}