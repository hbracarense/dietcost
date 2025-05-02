#' Food group serves calculator
#' 
#' Calculates total food group serves of random meal plan.
#' @param df Random meal plan.
#' @return Food group serves dataframe.
#' 
#' @export
getFoodGroupServes <- function(df){
  standard_name_check(df, 'food_group', 'food_group_id', 'serves')
  
  df_serves <- df %>% summarise(value = sum(.data$serves),
                                .by = c(.data$food_group, .data$food_group_id))
  return(df_serves)
}