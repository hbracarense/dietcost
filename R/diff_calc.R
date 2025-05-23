#' Difference calculator
#' 
#' Calculates difference between values of random meal plan created and targets logged.
#' @param val Value to be evalueted.
#' @param min Minimum constraint.
#' @param max Maximum constraint.
#' @return Difference.
#' @export
diff_calc <- function(val, min, max){
  res <- ifelse((val < min),
                {val - min},
                ifelse((val > max),
                       {val - max},
                       0))
  return(res)
}
