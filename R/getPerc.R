#' Percentage values calculator
#' 
#' Calculates percentage nutrient values.
#' @param df_nutri Nutrient constraints dataframe.
#' @param df_meal Random meal plan
#' @return Percentage dataframe.
#' @examples
#' df <- getPerc(getNutrients(random_meal),random_meal);
#' 
#' @export
getPerc <- function(df_nutri, df_meal){
  f1 <- 37.7
  f2 <- 16.7
  alcohol_id <- ifelse('Alcohol' %in% df_meal$food_group,
                       unique(df_meal$food_group_id[df_meal$food_group == 'Alcohol']),
                       0)
  discretionary_id <- ifelse('Discretionary foods' %in% df_meal$food_group,
                             unique(df_meal$food_group_id[df_meal$food_group == 'Discretionary foods']),
                             0)
  takeaway_id <- ifelse('Takeaway' %in% df_meal$food_group,
                        unique(df_meal$food_group_id[df_meal$food_group == 'Takeaway']),
                        0)
  if('fat' %in% df_nutri$nutrient && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('fat_perc',(as.numeric(df_nutri$value[df_nutri$nutrient == 'fat'])*f1/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if('sat_fat' %in% df_nutri$nutrient && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('sat_fat_perc',(as.numeric(df_nutri$value[df_nutri$nutrient == 'sat_fat'])*f1/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if('CHO' %in% df_nutri$nutrient && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('CHO_perc',(as.numeric(df_nutri$value[df_nutri$nutrient == 'CHO'])*f2/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if('protein' %in% df_nutri$nutrient && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('protein_perc',(as.numeric(df_nutri$value[df_nutri$nutrient == 'protein'])*f2/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if('sugars' %in% df_nutri$nutrient && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('sugars_perc',(as.numeric(df_nutri$value[df_nutri$nutrient == 'sugars'])*f2/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if(alcohol_id != 0 && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('alcohol_perc',(sum(df_meal$energy_kj_g[df_meal$food_group_id == alcohol_id])/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if(discretionary_id != 0 && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('discretionary_perc',(sum(df_meal$energy_kj_g[df_meal$food_group_id == discretionary_id])/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  
  if(takeaway_id != 0 && 'energy' %in% df_nutri$nutrient){
    df_nutri[nrow(df_nutri)+1,] <- c('takeaway_perc',(sum(df_meal$energy_kj_g[df_meal$food_group_id == takeaway_id])/as.numeric(df_nutri$value[df_nutri$nutrient == 'energy']))*100)
  }
  return(df_nutri)
}