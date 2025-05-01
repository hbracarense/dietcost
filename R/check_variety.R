#' Variety check
#'
#' Checks if varieties are into the allowed range (1,2 or 3).
#' @param df Dataframe variety column.
#' @examples 
#'DIETCOST::check_variety(DIETCOST::foods$variety)
#' @export
check_variety <- function(df){
  check_nom_num_df(df)
  if(max(df) > 3){
    stop('Allowed varieties: 1, 2 or 3 ')
  }
}