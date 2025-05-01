#' General difference calculation
#' 
#' Applies difference calculation to entire dataset.
#' @param df_target Constraints dataframe.
#' @param df_nutrients Nutrients/serves from random meal plan dataframe.
#' @param merge_col Column to join both dataframes.
#' @return Differences dataframe.
#' @examples
#' nutrients_diff <- getDifference(df_target = nutrient_targets_wk, df_nutrients = nutrients_plan, merge_col = 'nutrient');
#' 
#' @export
getDifference <- function(df_target, df_nutrients, merge_col){
  for(i in 1:length(merge_col)){
    standard_name_check(df_target, merge_col[i])
    standard_name_check(df_nutrients, merge_col[i])
  }
  df <- left_join(df_target, df_nutrients, merge_col)
  df <- na.omit(df)
  for(i in 1:nrow(df)){
    df$diff[i] <- diff_calc(as.numeric(df$value[i]), as.numeric(df$min[i]), as.numeric(df$max[i]))
  }
  df <- df[,-which(names(df) %in% c('min', 'max', 'value'))]
  names(df)[names(df) == 'diff'] <- 'value'
  return(df)
}
