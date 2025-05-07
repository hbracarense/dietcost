#' Float range
#'
#' Checks if a numeric variable is within a continuous float range.
#' @param variable Numeric variable.
#' @param min Minimum possible value.
#' @param max Maximum possible value.
#' @return No return value,only performs a check.
#' @export
add_float_range<- function(variable, min, max){
  if(variable<min || variable > max){
    stop(paste(variable, 'argument must be be between',min,'and',max))
  }
}