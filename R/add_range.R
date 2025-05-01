#' Discrete range
#'
#' Checks if a variable is within a discrete range.
#' @param variable variable.
#' @param range Allowed range.
#' @param message Message to be printed in case of failure.
#' @examples 
#' add__range(value, c('C','PF','H','PV'),'C, PF, H and PV')
#' @export
add_range <- function(variable, range, message){
  if(is.null(variable) || !length(variable) || !all(variable %in% range)){
    stop(paste("'",variable,"' argument must be one of:", message))
  }
}