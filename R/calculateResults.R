#'Calculates results for a Monte Carlo Simulation
#'
#'Calculates a confidence interval for several parameters obtained through a Monte Carlo Simulation. This function should be employed only if the standard table supplied with this package is utilized. Prints a .xlsx file in the home directory.
#'@param path_file A string containing the path to the folder containing the .csv files created in the monteCarlo function.
#'@param report_path A string containing the path to where the report will be saved.
#'@param confidence_interval A float. Must be either 0.01, 0.05 or 0.1.
#'@return No R object return, prints an Excel workbook.
#'@export
calculateResults <- function(path_file, report_path, confidence_interval){
  if(!(confidence_interval %in% c(0.01, 0.05, 0.1))){
    stop('Confidence interval must be either 0.01, 0.05 or 0.1. Please try again!')
  }
  
  files <- list.files(path = path_file, pattern = "meal_plan_", all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  if(is_empty(files)){
    stop("There aren't any files in said directory. Please try again.")
  } else{
    f1 <- 37.7
    f2 <- 16.7
    df <- data.frame(item = c('energy_kj_g', 'fat_g', 'sat_fat_g', 'CHO_g', 'sugars_g', 'protein_g', 'fat_perc', 'sat_fat_perc', 'CHO_perc', 'sugars_perc', 'fibre_g', 'protein_perc', 'red_meat_g', 'sodium_mg', 'fruit_serves', 'vegetable_serves', 'grains_serves', 'dairy_serves', 'protein_serves', 'fats_serves', 'sauces_serves', 'beverages_serves', 'ssb_serves', 'starchy_serves', 'red_meat_serves', 'alcohol_serves', 'discretionary_serves', 'takeaway_serves','fruit_perc', 'vegetable_perc', 'grains_perc', 'dairy_perc', 'protein_foods_perc', 'fats_perc', 'sauces_perc', 'beverages_perc', 'ssb_perc', 'starchy_perc', 'red_meat_perc', 'alcohol_perc', 'discretionary_perc', 'takeaway_perc','price', 'CF_gCO2eq', 'WF_l', 'EF_g_m2'))
    confidence_interval <- 1 - (confidence_interval/2)
    for(file in files){
      meal_df <- read.csv(file.path(path_file, file))
      col <- as.character(strsplit(file,'.csv')[1])
      df[,col] <- double(46)
      
      for(i in 1:nrow(df)){
        switch(df$item[i],
               'energy_kj_g' = {df[i, col] <- sum(meal_df$energy_kj_g, na.rm = TRUE)},
               'fat_g' = {df[i, col] <- sum(meal_df$fat_g, na.rm = TRUE)},
               'sat_fat_g' = {df[i, col] <- sum(meal_df$sat_fat_g, na.rm = TRUE)},
               'CHO_g' = {df[i, col] <- sum(meal_df$CHO_g, na.rm = TRUE)},
               'sugars_g' = {df[i, col] <- sum(meal_df$sugars_g, na.rm = TRUE)},
               'protein_g' = {df[i, col] <- sum(meal_df$protein_g, na.rm = TRUE)},
               'fat_perc' = {df[i, col] <- (sum(meal_df$fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
               'sat_fat_perc' = {df[i, col] <- (sum(meal_df$sat_fat_g, na.rm = TRUE)*f1)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
               'CHO_perc' = {df[i, col] <- (sum(meal_df$CHO_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
               'sugars_perc' = {df[i, col] <- (sum(meal_df$sugars_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
               'fibre_g' = {df[i, col] <- sum(meal_df$fibre_g, na.rm = TRUE)},
               'protein_perc' = {df[i, col] <- (sum(meal_df$protein_g, na.rm = TRUE)*f2)/sum(meal_df$energy_kj_g, na.rm = TRUE)*100},
               'red_meat_g' = {df[i, col] <- sum(meal_df$intake[meal_df$food_group == 'red meat'], na.rm = TRUE)},
               'sodium_mg' = {df[i, col] <- sum(meal_df$sodium_mg, na.rm = TRUE)},
               'fruit_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Fruit']), na.rm = TRUE)},
               'vegetable_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Vegetables']), na.rm = TRUE)},
               'grains_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Grains']), na.rm = TRUE)},
               'dairy_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Dairy/alternatives']), na.rm = TRUE)},
               'protein_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Protein foods: Meat, poultry, seafood, eggs, legumes, nuts, seeds']), na.rm = TRUE)},
               'fats_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Fats & oils']), na.rm = TRUE)},
               'sauces_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Sauces, dressings, spreads, sugars']), na.rm = TRUE)},
               'beverages_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Beverages']), na.rm = TRUE)},
               'ssb_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'ssb']), na.rm = TRUE)},
               'starchy_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Starchy vegetables']), na.rm = TRUE)},
               'red_meat_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'red meat']), na.rm = TRUE)},
               'alcohol_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Alcohol']), na.rm = TRUE)},
               'discretionary_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Discretionary foods']), na.rm = TRUE)},
               'takeaway_serves' = {df[i, col] <- sum((meal_df$serves[meal_df$food_group == 'Takeaway']), na.rm = TRUE)},
               'fruit_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Fruit'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'vegetable_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Vegetables'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'grains_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Grains'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'dairy_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Dairy/alternatives'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'protein_foods_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Protein foods: Meat, poultry, seafood, eggs, legumes, nuts, seeds'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'fats_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Fats & oils'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'sauces_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Sauces, dressings, spreads, sugars'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'beverages_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Beverages'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'ssb_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'ssb'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'starchy_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Starchy vegetables'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'red_meat_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'red meat'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'alcohol_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Alcohol'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'discretionary_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Discretionary foods'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'takeaway_perc' = {df[i, col] <- (sum(meal_df$energy_kj_g[meal_df$food_group == 'Takeaway'], na.rm = TRUE)/(sum(meal_df$energy_kj_g, na.rm = TRUE)))*100},
               'price' = {df[i, col] <- sum(meal_df$price, na.rm = TRUE)},
               'CF_gCO2eq' = {df[i, col] <- sum(meal_df$CF_gCO2eq, na.rm = TRUE)},
               'WF_l' = {df[i, col] <- sum(meal_df$WF_l, na.rm = TRUE)},
               'EF_g_m2' = {df[i, col] <- sum(meal_df$EF_g_m2, na.rm = TRUE)}
        )
        
      }
    }
    df_results <- data.frame(item = df$item)
    df_results[,c('value', 'margin')] <- double(nrow(df))
    
    for(i in 1:nrow(df)){
      n <- ncol(df) - 1
      df_results$value[i] <- as.numeric(rowMeans(df[i,2:ncol(df)]))
      s <- sd(df[i,2:ncol(df)])
      df_results$margin[i] <- ifelse(n <= 30,
                                     qt(confidence_interval,df=n-1)*s/sqrt(n),
                                     qnorm(confidence_interval)*s/sqrt(n))
      
    }
    
    df_results[nrow(df_results)+1,] <-c('n',n,NA)
    file_path <- paste0('calculated_results_', format(Sys.time(), '%Y%m%d%H%M%S'), '.xlsx')
    write.xlsx(df_results, file.path(report_path, file_name), sheetName = 'Results', row.names = FALSE)
  }
}