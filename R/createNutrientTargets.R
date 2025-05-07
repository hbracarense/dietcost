#' Nutrients data addition
#'
#' Adds nutrients data to foods dataframe.
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param allow_alcohol Boolean variable checking if alcohol is permitted. Default TRUE.
#' @param allow_discretionary Boolean variable checking if discretionary foods are permitted. Default TRUE.
#' @param allow_takeaway Boolean variable checking if takeaway is permitted. Default TRUE.
#' @param alcohol_perc_max Optional parameter. Defines maximum energy intake derived from alcohol.
#' @param discretionary_perc_max Optional parameter. Defines maximum energy intake derived from discretionary foods.
#' @param takeaway_perc_max Optional parameter. Defines maximum energy intake derived from takeaway.
#' @return Nutrient targets dataframe.
#' @export
createNutrientTargets <- function(filepath, allow_alcohol = TRUE, allow_discretionary = TRUE, allow_takeaway = TRUE, alcohol_perc_max = NULL, discretionary_perc_max = NULL, takeaway_perc_max = NULL){
  alcohol <- checks_optional_food_groups(allow_alcohol, alcohol_perc_max)
  discretionary <- checks_optional_food_groups(allow_discretionary, discretionary_perc_max)
  takeaway <- checks_optional_food_groups(allow_takeaway, takeaway_perc_max)
  df <- upload_data(filepath, 'nutrient_targets')
  standard_name_check(df,'individual', 'diet', 'energy_mj_min', 'energy_mj_max', 'alcohol_perc_max', 'discretionary_perc_max', 'takeaway_perc_max')
  possible_missing_cols <- c('alcohol_perc_min', 'discretionary_perc_min', 'takeaway_perc_min')
  checks <- c(allow_alcohol, allow_discretionary, allow_takeaway)
  for(i in 1:length(possible_missing_cols)){
    df <- check_min_exists(df, checks[i], possible_missing_cols[i])
  }
  df <- energy_conversor(df, 'energy_mj_min', 'energy_mj_max')
  df$alcohol_perc_max <- alcohol
  df$discretionary_perc_max <- discretionary
  df$takeaway_perc_max <- takeaway
  check_match_individual_diet(df)
  check_nom_num_df(df[,!(names(df) %in% c('individual', 'diet'))])
  df[,!(names(df) %in% c('individual', 'diet'))] <- df[,!(names(df) %in% c('individual', 'diet'))] %>% replace(is.na(.), 0)
  message('Nutrient targets data added with success.')
  return(df)
}