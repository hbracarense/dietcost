#' Minimum intake food groups check
#'
#' Looks for a minimum value and sets zero if missing,
#' @param df Dataframe.
#' @param check Boolean variable to permit optional food group.
#' @param col Minimum percentage intake column name.
#' @return Dataframe.
#' @examples 
#'  df <- check_min_exists(df, allow_alcohol, alcohol_perc_min)
#' @export
check_min_exists <- function(df, check, col){
  if(isTRUE(check) && !(col %in% colnames(df))){
    df[col] <- 0
  }
  return(df)
}