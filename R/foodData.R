#' Single-function food dataframe creation
#'
#' Creates foods dataframe, with emission, nutrients, constraints and price data, in a single function.
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param redmeat_ids Vector of unique food IDs that are redmeat.
#' @param diets Chosen diets. Constraint sheets in foods dataset must be of format 'constraints_DIETNAME_diet_foods', then the parameter passed will be DIETNAME. Can be a vector of diets in format c('DIETNAME1','DIETNAME2',...,'DIETNAMEN').
#' @param max_scale Maximum scale. Default is two.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @param override_min If is not null, overrides all minimum values
#' @return Foods dataframe.
#' @export
foodData <- function(filepath = filepath, redmeat_ids, diets, max_scale, emission_cols = NULL, override_min = NULL){
  df <- createFoodData(filepath = filepath, redmeat_ids = redmeat_ids)
  df <- addEmissionData(filepath = filepath, df = df, emission_cols = emission_cols)
  for(i in 1:length(diets)){
    df <- addConstraintData(filepath = filepath, df = df, diets = diets[i], max_scale = max_scale, override_min = override_min)
  }
  df <- addNutrientData(filepath = filepath, df = df)
  df <- addPriceData(filepath = filepath, df = df)
  return(df)
}