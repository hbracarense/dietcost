#' Safe sampling
#'
#' Safely extracts a random unitary sample from a vector.
#' @param x Vector.
#' @return Random sample.
#' @examples 
#'DIETCOST::intake <- sample_safe(c(10,25,37,52,100));
#' @export
sample_safe <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}
