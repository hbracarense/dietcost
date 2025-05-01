#' Food group serves calculator
#' 
#' Calculates total food group serves of random meal plan.
#' @param df Random meal plan.
#' @return Food group serves dataframe.
#' @examples
#' serves <- getFoodGroupServes(random_meal);
#' 
#' @export
getFoodGroupServes <- function(df){
  standard_name_check(df, 'food_group', 'food_group_id', 'serves')
  
  df_serves <- df %>% summarise(value = sum(serves),
                                .by = c(food_group, food_group_id))
  return(df_serves)
}