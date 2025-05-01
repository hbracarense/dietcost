#' Redmeat flag
#'
#' Sets a boolean redmeat flag column in dataset.
#' @param id Food group id column in dataframe.
#' @param redmeat_ids Vector of unique food IDs that are redmeat.
#' @examples 
#'df$redmeat <- sapply(df$food_id, redmeat_check, c(00001, 00002))
#' @export
redmeat_check <- function(id, redmeat_ids){
  ifelse(id %in% redmeat_ids,
         TRUE,
         FALSE)
}