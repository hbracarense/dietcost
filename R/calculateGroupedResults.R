#'Calculates grouped results for a Monte Carlo Simulation
#'
#'Calculates a confidence interval for price and footprints obtained through a Monte Carlo Simulation, grouped by food groups. This function should be employed only if the standard table supplied with this package is utilized. Prints a .xlsx file in the home directory.
#'@param path_file A string containing the path to the folder containing the .csv files created in the monteCarlo function.
#'@param confidence_interval A float. Must be either 0.01, 0.05 or 0.1.
#'@examples
#'calculateGroupedResults('/my/folder', 0.05)
#'@export
calculateGroupedResults <- function(path_file, confidence_interval){
  if(!(confidence_interval %in% c(0.01, 0.05, 0.1))){
    stop('Confidence interval must be either 0.01, 0.05 or 0.1. Please try again!')
  }
  
  files <- list.files(path = path_file, pattern = "meal_plan_", all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  if(lenght(files) == 0){
    stop("There aren't any files in said directory. Please try again.")
  } else{
    df <- data.frame(item = c('price', 'CF_gCO2eq', 'WF_l', 'EF_g_m2'))
    confidence_interval <- 1 - (confidence_interval/2)
    for(file in files){
      meal_df <- read.csv(file.path(path_file, file))
      col <- as.character(strsplit(file,'.csv')[1])
      df[,col] <- double(4)
      meal_df$price <- (meal_df$price/100)*meal_df$intake
      meal_df$CF_gCO2eq <- (meal_df$CF_gCO2eq/1000)*meal_df$intake
      meal_df$WF_l <- (meal_df$WF_l/1000)*meal_df$intake
      meal_df$EF_g_m2 <- (meal_df$EF_g_m2/1000)*meal_df$intake
      for(i in nrow(df)){
        switch(df$item[i],
               'price' = {df[i, col] <- meal_df %>% group_by(food_group) %>% summarise(!!col := sum(price, na.rm = TRUE)) %>% select(!!col)},
               'CF_gCO2eq' = {df[i, col] <- meal_df %>% group_by(food_group) %>% summarise(!!col := sum(CF_gCO2eq, na.rm = TRUE)) %>% select(!!col)},
               'WF_l' = {df[i, col] <- meal_df %>% group_by(food_group) %>% summarise(!!col := sum(WF_l, na.rm = TRUE)) %>% select(!!col)},
               'EF_g_m2' = {df[i, col] <- meal_df %>% group_by(food_group) %>% summarise(!!col := sum(EF_g_m2, na.rm = TRUE)) %>% select(!!col)}
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
    write_xlsx(df_results, file.path(getwd(), paste0('calculated_results_', format(Sys.time(), '%Y%m%d%H%M%S'), '.xlsx')))
  }
  
}