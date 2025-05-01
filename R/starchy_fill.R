#' Starchy vegetables serves addition
#'
#' Adds minimum and maximum serves of starchy vegetables.
#' @param df Dataframe.
#' @param starchy_name Starchy vegetables food group name. Default 'Starchy vegetables'.
#' @param serve_identifier Serve column identifier. Default 'serve'.
#' @param max_identifier Max column identifier. Default 'max'.
#' @return Food group dataframe with starchy vegetable minimum and maximum serves columns added.
#' @examples 
#' df <- starchy_fill(df, 'Starchy vegetables', 'serve', 'max')
#' @export
starchy_fill <- function(df,starchy_name, serve_identifier, max_identifier){
  row = which(df['food_group'] == starchy_name)
  for(column in which(grepl(serve_identifier,colnames(df))&grepl(max_identifier,colnames(df)))){
    if((is.numeric(df[row,column]) && df[row,column] == 0)||(is.na(df[row,column]))){
      df[row,column] <- 100
    }
  }
  return(df)
}