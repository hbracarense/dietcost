#' Non-numeric check
#'
#' Checks if values supposed to be numeric are in fact numeric.
#' @param df Dataframe column.
#' @export
check_non_num <- function(df) {
  bad_num <- is.na(suppressWarnings(as.numeric(as.character(df))))
  if(length(which(bad_num & !is.na(df)))>0) return(0) else return(1)
}