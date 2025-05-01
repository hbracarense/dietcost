#' Price data addition
#'
#' Adds price data to foods dataframe.
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param df Foods dataframe.
#' @return Foods dataframe with added price data.
#' @export
addPriceData <- function(filepath, df){
  standard_name_check(df, 'food_id', 'food_name')
  df1 <- upload_data(filepath, 'prices')
  standard_name_check(df1, 'food_id', 'food_name', 'price')
  check_match_food_price(df1)
  check_id_defined(df1, df, 'price')
  check_nom_num_df(df1[,!(names(df1) %in% c('food_id', 'food_name'))])
  check_spelling(df[,c('food_id', 'food_name')], df1[,c('food_id', 'food_name')],'food_id')
  df <- join_function(df,df1[,c('food_id', 'price')],'food_id')
  print('Price data added with success.')
  return(df)
}