#' Nutrients data addition
#'
#' Adds nutrients data to foods dataframe.
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param df Foods dataframe.
#' @return Foods dataframe with nutrient columns.
#' @examples 
#' foods_df <- addNutrientData(filepath = 'C:/Users/username/Downloads/dataset.xlsx', df = foods_df);
#' @export
addNutrientData <- function(filepath, df){
  standard_name_check(df, 'food_id', 'food_name')
  df1 <- upload_data(filepath, 'nutrients')
  standard_name_check(df1,'food_id', 'food_name')
  check_nom_num_df(df1[,!(names(df1) %in% c('food_id', 'food_name'))])
  check_id_defined(df1, df, 'nutrition')
  check_spelling(df[,c('food_id', 'food_name')], df1[,c('food_id', 'food_name')],'food_id')
  df1 <- df1[,!(names(df1) %in% 'food_name')]
  df <- join_function(df, df1, 'food_id')
  print('Nutrients data added with success.')
  return(df)
}