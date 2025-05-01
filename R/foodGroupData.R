#' Single-function food group dataframe creation
#'
#' Creates food groups dataframe, with constraints data, in a single function.
#' @param filepath Path in which the dataset, in .xlsx format, is stored.
#' @param df_foods Foods dataframe.
#' @param diets Chosen diets. Constraint sheets in foods dataset must be of format 'constraints_DIETNAME_diet_food_groups', then the parameter passed will be DIETNAME. Can be a vector of diets in format c('DIETNAME1','DIETNAME2',...,'DIETNAMEN').
#' @return Food groups dataframe.
#' @export
foodGroupData <- function(filepath, df_foods, diets){
  df <- createFoodGroupData(df_foods)
  for(i in 1:length(diets)){
    df <- addFoodGroupsConstraintData(filepath = filepath, df, diets = diets[i])
  }
  return(df)
}