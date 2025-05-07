#' Join function
#'
#' Safely performs a left join between two dataframes.
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param condition Column in which the two datframes will be joined. Can be a single string or a vector.
#' @return Dataframe.
#' @export
join_function <- function(df1, df2, condition){
  df <- df1 %>%
    tryCatch(
      expr = left_join(.,df2, by = condition),
      error = function(e){
        message('Join failed!')
        stop(e)
      }
    )
  return(df)
}