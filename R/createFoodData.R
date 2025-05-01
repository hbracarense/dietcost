#' Food data creation
#'
#' Creates a food data dataframe
#' @param filepath Path in which the dataset, in .xlsx format, is stored.
#' @param redmeat_ids Vector of redmeat IDs.
#' @return Food dataframe.
#' @examples 
#' foods_df <- createFoodData(filepath = 'C:/Users/username/Downloads/dataset.xlsx', redmeat_ids = c(71003, 71008, 71041, 81005, 81021, 81022, 81026, 81027, 81029))
#' @export
createFoodData <- function(filepath, redmeat_ids){
  df <- upload_data(filepath, 'foods')
  standard_name_check(df, 'food_id', 'food_name', 'food_group', 'variety')
  unique_values(df$food_id,df,'food_id', "food ID")
  lapply(df$food_name, check_function, 'food names')
  check_variety(df$variety)
  df$food_group <- sapply(df$food_group, sauces_protein_discretionary_change)
  print("Food group names altered with success.")
  df$redmeat <- sapply(df$food_id, redmeat_check, redmeat_ids)
  print("Red meat flag added with success.")
  print('Food data dataframe created with success.')
  return(df)
}