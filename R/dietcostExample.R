#' Package helper function
#' 
#' Convenienc function making easy to access external data files.
#' @param path A string containing the path to extdata folder.
#' @examples
#' dietcost_example("extdata")
#' 
#' @export
dietcostExample <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "dietcost"))
  } else {
    system.file("extdata", path, package = "dietcost", mustWork = TRUE)
  }
}