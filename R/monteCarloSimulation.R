#' Single-function Monte Carlo simulation and results export.
#' 
#' Runs Monte Carlo Simulation and prints results, in .xlsx format, in a single funtion.
#' @param iterations Number of iterations. Integer.
#' @param foods_df Foods dataframe.
#' @param nutrient_targets_df Nutrient constraints dataframe.
#' @param food_group_targets_df Food group serves dataframe.
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param allowed_varieties Permitted food varieties. Can be a vector of the following: 1,2 and/or 3.
#' @param min_serve_size_difference Multiplier to serve difference. A float between 0 and 1.
#' @param allow_alcohol Boolean variable checking if alcohol is permitted. Default TRUE.
#' @param allow_discretionary Boolean variable checking if discretionary foods are permitted. Default TRUE.
#' @param allow_takeaway Boolean variable checking if takeaway is permitted. Default TRUE.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @param nutrient_cols Optional parameter. Nutrients column names if standard dataset isn't used.
#' @param nutrient_constraints Optional parameter. Vector of nutrients column names to be used if not all nutrients are to be used as constraints.
#' @param linked_low_1 Optional parameter. Vector of lower bound food IDs.
#' @param linked_high_1 Optional parameter. Vector of higher bound food IDs.
#' @param linked_low_2 Optional parameter. Vector of lower bound food IDs.
#' @param linked_high_2 Optional parameter. Vector of higher bound food IDs.
#' @examples
#' monteCarloSimulation(iterations = 5,
#'                      foods_df = foods,
#'                      nutrient_targets_df = nutrient_targets,
#'                      food_group_targets_df = food_groups,
#'                      person = 'woman',
#'                      diet = 'PF',
#'                      allowed_varieties = c(1,2,3),
#'                      min_serve_size_difference = 0.5,
#'                      allow_discretionary = TRUE,
#'                      allow_alcohol = TRUE,
#'                      allow_takeaway = TRUE)
#' 
#' @export

monteCarloSimulation <- function(iterations, foods_df, nutrient_targets_df, food_group_targets_df, person, diet, allowed_varieties, min_serve_size_difference, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE, emission_cols = NULL, nutrient_cols = NULL, nutrient_constraints = NULL, linked_low_1 = NULL, linked_high_1 = NULL, linked_low_2 = NULL, linked_high_2 = NULL){
  if((!is.null(linked_low_1) && is.null(linked_high_1))||(!is.null(linked_high_1) && is.null(linked_low_1))){
    stop('Please inform the matching pair of linked foods!')
  }
  if((!is.null(linked_low_2) && is.null(linked_high_2))||(!is.null(linked_high_2) && is.null(linked_low_2))){
    stop('Please inform the matching pair of linked foods!')
  }
  if((!is.null(linked_low_2)||!is.null(linked_high_2))&&(is.null(linked_low_1)&&is.null(linked_high_1))){
    stop('Please inform the first pair of linked foods!')
  }
  
  if(is.null(nutrient_cols)){
    nutrient_cols <- c('energy_kj_g','fat_g','sat_fat_g','CHO_g','sugars_g','fibre_g','protein_g','sodium_mg')
  }
  
  if(is.null(emission_cols)){
    emission_cols <- c('CF_gCO2eq', 'WF_l', 'EF_g_m2')
  }
  
  if(!is.null(nutrient_constraints)){
    for(i in 1:length(nutrient_constraints)){
      if(!(nutrient_constraints[i]) %in% nutrient_cols){
        stop('Nutrient constraints must also be located in nutrient values. Check your data!')
      }
    }
  }
  results <- monteCarlo(iterations, foods_df, nutrient_targets_df, food_group_targets_df, person, diet, allowed_varieties, min_serve_size_difference, allow_discretionary, allow_alcohol, allow_takeaway, emission_cols, nutrient_cols, nutrient_constraints, linked_low_1, linked_high_1, linked_low_2, linked_high_2)
  printResults(results, person, diet, allowed_varieties, iterations)
}
