#' Unique value check
#'
#' Checks if there are non-unique values in dataset.
#' @param value Column from which an unique vector will be formed.
#' @param df Dataframe in which lies the column to be checked.
#' @param value_col Name of the column to be checked, in string format.
#' @param value_name Name of the variable tested.
#' @return No R object return, performs only a check.
#' @export
unique_values <- function(value, df, value_col, value_name){
  if(nrow(as.data.frame(unique(value)))<nrow(df)){
    duplicates = df %>% group_by(UQ(sym(value_col))) %>% summarise(n=n()) %>% filter(.data$n>1)
    colnames(duplicates) <- c("value", "n")
    stop(paste("Check the data! There are duplicates!", duplicates$value))
  } else{
    message(paste('Only unique values:', value_name))
  }
}