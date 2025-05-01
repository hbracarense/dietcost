#' MJ to KJ conversion
#'
#' Converts energy values in megajoules (MJ) to kilojoules (KJ),
#' @param df Dataframe.
#' @param min Minimum energy column name. Default 'energy_mj_min'.
#' @param max Maximum energy column name. Default 'energy_mj_max'.
#' @export
energy_conversor <- function(df, min, max){
  df <- df %>% mutate(
    across(
      .cols = c(grep(min, colnames(df)), grep(max, colnames(df))),
      .fns = function(x){
        x * 1000
      }
    )
  )
  names(df)[names(df) == min] <- "energy_kj_min"
  names(df)[names(df) == max] <- "energy_kj_max"
  return(df)
}
