#' Safe sampling
#'
#' Safely extracts a random unitary sample from a vector.
#' @param x Vector.
#' @return Random sample.
#' @examples 
#'serve_range <- c(10,25,37,52,100);
#'foods_df$intake <- sample_safe(serve_range);
#' @export
sample_safe <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}
