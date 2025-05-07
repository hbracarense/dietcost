#' Discrete range
#'
#' Checks if a variable is within a discrete range.
#' @param variable variable.
#' @param range Allowed range.
#' @param message Message to be printed in case of failure.
#' @return No return value, only performs a check.
#' @export
add_range <- function(variable, range, message){
  if(is.null(variable) || !length(variable) || !all(variable %in% range)){
    stop(paste("'",variable,"' argument must be one of:", message))
  }
}