#' Standard name check
#'
#' Checks if variable names are the stadard defined into DIETCOST R standard table.
#' @param df Dataframe.
#' @examples 
#'standard_name_check(df, 'food_id', 'food_name')
#' @export
standard_name_check <- function(df,...){
  variables <- list(...)
  for(variable in variables){
    if(!(variable %in% colnames(df))){
      stop("Column names don't match standard. Check your data!")
    }
  }
}