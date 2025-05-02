#' Food group serves conversion
#' 
#' Converts food group serves dataframe to weekly values.
#' @param df Food group serves dataframe.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_food_groups' sheet in dataset.
#' @param individual Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @return Converted food group serves dataframe.
#' @examples
#' food_groups_wk <- convertWeeklyFoodGroups(DIETCOST::food_groups, 'C', 'man');
#' @export
convertWeeklyFoodGroups <- function(df, diet, individual){
  min <- paste0(individual,'_min_serve_',diet)
  max <- paste0(individual,'_max_serve_',diet)
  standard_name_check(df, 'food_group', 'food_group_id', min, max)
  df <- df %>% select(all_of('food_group_id'),
                      all_of('food_group'),
                      all_of(min),
                      all_of(max))
  id_n <- grep('food_group_id', colnames(df))
  group_n <- grep('food_group', colnames(df))
  exclusion_n <- c(id_n, group_n)
  df <- converts_dataframe(df, exclusion_n)
  names(df)[names(df) == min] <- 'min'
  names(df)[names(df) == max] <- 'max'
  return(df)
}