#' All zero difference check
#' 
#' Checks if differences dataframe is all zeroes.
#' @param diff Differences dataframe
#' @return Boolean. TRUE if all zeroes, FALSE otherwise.
#' @export
checkZeroDiff <- function(diff){
  if(all(diff$value == 0)) return(TRUE) else return(FALSE)
}
