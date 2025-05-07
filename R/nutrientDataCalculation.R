#'Nutrient data application to random meal plan created
#'
#'Applies nutrient data calculation to random meal plan generated.
#'
#' @param df Random meal plan.
#' @param nutrient_cols Optional parameter. Nutrient column names if standard dataset isn't used.
#' @return Random meal plan with nutrients calculated.
#' 
#' @export
nutrientDataCalculation <- function(df, nutrient_cols = NULL){
  standard_name_check(df, 'price', 'intake')
  if(is.null(nutrient_cols)){
    nutrient_cols <- c('energy_kj_g', 'fat_g', 'sat_fat_g', 'CHO_g', 'sugars_g', 'fibre_g',	'protein_g',	'sodium_mg')
  }
  for(i in 1:length(nutrient_cols)){
    standard_name_check(df, nutrient_cols[i])
    df[nutrient_cols[i]] <- (df[nutrient_cols[i]]/100)*df$intake
  }
  return(df)
}