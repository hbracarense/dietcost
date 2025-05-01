#' Missing value check
#'
#' Checks if there are any missing values in a given column from the dataset.
#' @param name Column in which missing values will be sought.
#' @param column Column name, in string format.
#' @examples 
#' DIETCOST::unique_values(DIETCOST::foods$food_id,"food ID");
#' @export
check_function <- function(name, column){
  if(any(is.na(name))){
    stop(paste('Missing',column,'! Check the food data!'))
  }
}