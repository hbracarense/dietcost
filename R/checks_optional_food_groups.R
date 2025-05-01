#' Optional food groups check
#'
#' If discretionary foods, alcohol or takeaway are permitted, looks for a minimum value and sets zero if missing,
#' @param check Boolean variable to permit optional food group.
#' @param value Minimum percentage of energy intake from optional food group.
#' @return Minimum percentage of energy intake from optional food group.
#' @examples 
#' alcohol <- checks_optional_food_groups(allow_alcohol, alcohol_perc_max)
#' @export
checks_optional_food_groups <- function(check, value){
  if(isTRUE(check)){
    if(is.null(value)){
      stop(paste("Please insert into function the following variable: "), deparse(substitute(value)))
    }
    add_float_range(value, 0,100)
  } else{
    value <- 0
  }
  return(value)
}