#' Weekly conversion
#'
#' Converts data from daily to weekly
#' @param df Dataframe.
#' @param exclusion_cols Columns (non-numerical or percentage) that conversion won't be applied.
#' @return Weekly dataframe.
#' @export
converts_dataframe <- function(df, exclusion_cols){
  df <- tryCatch(
    expr = {
      df %>% mutate(
        across(
          .cols = -all_of(exclusion_cols),
          .fns = function(x){
            x*7
          }
        )
      )
    },
    error = function(e){
      message('Check your data! There are non-nummeric values in measures.')
      stop(e)
    }
  )
  return(df)
}
