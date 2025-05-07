#'Calculates grouped results for a Monte Carlo Simulation
#'
#'Calculates a confidence interval for price and footprints obtained through a Monte Carlo Simulation, grouped by food groups. This function should be employed only if the standard table supplied with this package is utilized. Prints a .xlsx file in the home directory.
#'@param path_file A string containing the path to the folder containing the .csv files created in the monteCarlo function.
#'@param report_path A string containing the path to where the report will be saved.
#'@param confidence_interval A float. Must be either 0.01, 0.05 or 0.1.
#'@return No R object return, prints an Excel workbook.
#'@export
calculateGroupedResults <- function(path_file, report_path, confidence_interval){
  if(!(confidence_interval %in% c(0.01, 0.05, 0.1))){
    stop('Confidence interval must be either 0.01, 0.05 or 0.1. Please try again!')
  }
  
  files <- list.files(path = path_file, pattern = "meal_plan_", all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  if(is_empty(files)){
    stop("There aren't any files in said directory. Please try again.")
  } else{
    food_groups <- c('Fruit', 'Vegetables', 'Grains', 'Dairy/alternatives', 'Protein foods: Meat, poultry, seafood, eggs, legumes, nuts, seeds', 'Fats & oils', 'Sauces, dressings, spreads, sugars', 'Beverages', 'ssb', 'Starchy vegetables', 'red meat', 'Alcohol', 'Discretionary foods', 'Takeaway')
    df_price <- data.frame(groups = food_groups)
    df_carbon <- data.frame(groups = food_groups)
    df_water <- data.frame(groups = food_groups)
    df_ecological <- data.frame(groups = food_groups)
    confidence_interval <- 1 - (confidence_interval/2)
    for(file in files){
      meal_df <- read.csv(file.path(path_file, file))
      col <- as.character(strsplit(file,'.csv')[1])
      df_price[,col] <- double(14)
      df_carbon[,col] <- double(14)
      df_water[,col] <- double(14)
      df_ecological[,col] <- double(14)
      for(food_group in food_groups){
        if(!(food_group %in% unique(meal_df$food_group))){
          next
        } else{
          for(i in 1:nrow(df_price)){
            if(df_price$groups[i] == food_group){
              df_price[i, col] <- sum(meal_df$price[meal_df$food_group == food_group], na.rm = TRUE)
            }
          }
          for(i in 1:nrow(df_carbon)){
            if(df_carbon$groups[i] == food_group){
              df_carbon[i, col] <- sum(meal_df$CF_gCO2eq[meal_df$food_group == food_group], na.rm = TRUE)
            }
          }
          for(i in 1:nrow(df_water)){
            if(df_water$groups[i] == food_group){
              df_water[i, col] <- sum(meal_df$WF_l[meal_df$food_group == food_group], na.rm = TRUE)
            }
          }
          for(i in 1:nrow(df_ecological)){
            if(df_ecological$groups[i] == food_group){
              df_ecological[i, col] <- sum(meal_df$EF_g_m2[meal_df$food_group == food_group], na.rm = TRUE)
            }
          }
        }
      }
    }
    df_results_price <- data.frame(group = food_groups,
                             value = double(14),
                             margin = double(14))
    df_results_carbon <- data.frame(group = food_groups,
                                   value = double(14),
                                   margin = double(14))
    df_results_water <- data.frame(group = food_groups,
                                    value = double(14),
                                    margin = double(14))
    df_results_ecological <- data.frame(group = food_groups,
                                   value = double(14),
                                   margin = double(14))

    for(i in 1:nrow(df_price)){
      n <- ncol(df_price) - 1
      df_results_price$value[i] <- as.numeric(rowMeans(df_price[i,2:ncol(df_price)]))
      s <- sd(df_price[i,2:ncol(df_price)])
      df_results_price$margin[i] <- ifelse(n <= 30,
                                     qt(confidence_interval,df=n-1)*s/sqrt(n),
                                     qnorm(confidence_interval)*s/sqrt(n))
      
    }
    
    for(i in 1:nrow(df_carbon)){
      n <- ncol(df_carbon) - 1
      df_results_carbon$value[i] <- as.numeric(rowMeans(df_carbon[i,2:ncol(df_carbon)]))
      s <- sd(df_carbon[i,2:ncol(df_carbon)])
      df_results_carbon$margin[i] <- ifelse(n <= 30,
                                           qt(confidence_interval,df=n-1)*s/sqrt(n),
                                           qnorm(confidence_interval)*s/sqrt(n))
      
    }
    
    for(i in 1:nrow(df_water)){
      n <- ncol(df_water) - 1
      df_results_water$value[i] <- as.numeric(rowMeans(df_water[i,2:ncol(df_water)]))
      s <- sd(df_water[i,2:ncol(df_water)])
      df_results_water$margin[i] <- ifelse(n <= 30,
                                           qt(confidence_interval,df=n-1)*s/sqrt(n),
                                           qnorm(confidence_interval)*s/sqrt(n))
      
    }
    
    for(i in 1:nrow(df_ecological)){
      n <- ncol(df_ecological) - 1
      df_results_ecological$value[i] <- as.numeric(rowMeans(df_ecological[i,2:ncol(df_ecological)]))
      s <- sd(df_ecological[i,2:ncol(df_ecological)])
      df_results_ecological$margin[i] <- ifelse(n <= 30,
                                           qt(confidence_interval,df=n-1)*s/sqrt(n),
                                           qnorm(confidence_interval)*s/sqrt(n))
      
    }
    
    df_results_price[nrow(df_results_price)+1,] <-c('n',n,NA)
    df_results_water[nrow(df_results_water)+1,] <-c('n',n,NA)
    df_results_carbon[nrow(df_results_carbon)+1,] <-c('n',n,NA)
    df_results_ecological[nrow(df_results_ecological)+1,] <-c('n',n,NA)
    file_name <- paste0('calculated_group_results_', format(Sys.time(), '%Y%m%d%H%M%S'), '.xlsx')
    write.xlsx(df_results_price, file.path(report_path, file_name), sheetName = 'Price', row.names = FALSE)
    write.xlsx(df_results_carbon, file.path(report_path, file_name), sheetName = 'CF_gCO2eq', row.names = FALSE, append = TRUE)
    write.xlsx(df_results_water, file.path(report_path, file_name), sheetName = 'WF_l', row.names = FALSE, append = TRUE)
    write.xlsx(df_results_ecological, file.path(report_path, file_name), sheetName = 'EF_g_m2', row.names = FALSE, append = TRUE)
  }
  
}