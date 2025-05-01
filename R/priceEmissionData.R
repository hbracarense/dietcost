#' Price/emission data application to random meal plan created
#'
#' Applies price and emission data calculation to random meal plan generated.
#'
#' @param df Random meal plan.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @return Random meal plan with price and emissions calculated.
#' @examples
#' df <-priceEmissionData(meal_plan);
#' df <-priceEmissionData(meal_plan, c('CO2','WF_l'));
#' @export
#Apply price and emission data to food intake
priceEmissionData <- function(df, emission_cols = NULL){
  standard_name_check(df, 'price', 'intake')
  if(is.null(emission_cols)){
    emission_cols <- c('CF_gCO2eq', 'WF_l', 'EF_g_m2')
  }
  for(i in 1:length(emission_cols)){
    standard_name_check(df, emission_cols[i])
    df[emission_cols[i]] <- (df[emission_cols[i]]/1000)*df$intake
  }
  df$price <- (df$price/100)*df$intake
  return(df)
}