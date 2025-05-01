#' Applies non-nummeric value check to entire dataframe
#'
#' Checks if values supposed to be numeric are in fact numeric.
#' @param df Dataframe columns.
#' @examples 
#'DIETCOST::check_nom_num_df(DIETCOST::foods)
#' @export
check_nom_num_df <- function(df){
  if(0 %in% lapply(df, check_non_num)){
    stop("Non-nummerical entries in nutrient targets dataset aren't allowed. Check your data!]")
  }
}