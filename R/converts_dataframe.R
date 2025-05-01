#' Weekly conversion
#'
#' Converts data from daily to weekly
#' @param df Dataframe.
#' @param exclusion_cols Columns (non-numerical or percentage) that conversion won't be applied.
#' @return Weekly dataframe.
#' @examples
#'   individual_n <- grep('individual',colnames(df))
#'   diet_n <- grep('diet',colnames(df))
#'   percentage_n <- grep(percentage,colnames(df))
#'   exclusion_n <- c(individual_n,diet_n,percentage_n)
#'   df <- converts_dataframe(df, exclusion_n)
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
      print('Check your data! There are non-nummeric values in measures.')
      stop(e)
    }
  )
  return(df)
}
