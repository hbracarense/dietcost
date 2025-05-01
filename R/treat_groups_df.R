#' Treatment of food group constraints dataframe
#'
#' Converts weekly food group serves to daily and adds diet suffix to column names.
#' @param df Dataframe.
#' @param suffix Suffix to be added to column.
#' @return Treated food group dataframe.
#' @export
treat_groups_df <- function(df, suffix){
  df <- df %>% replace(is.na(.), 0)
  df[] <- lapply(df, FUN = function(x) if (is.numeric(x)) return(x/7) else return(x))
  icol <- which(names(df) %in% 'food_group')
  colnames(df)[-icol] <- paste(colnames(df)[-icol],suffix,sep = '_')
  return(df)
}