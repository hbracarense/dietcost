#' Exportation of Monte Carlo results
#' 
#' Exports, in .xlsx format, the results of Monte Carlo simulation.
#' @param results List of results
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param allowed_varieties Permitted food varieties. Can be a vector of the following: 1,2 and/or 3.
#' @param iterations Number of iterations. Integer.

#' @export
printResults <- function(results, person, diet, allowed_varieties,iterations){
  general <- data.frame('Variable' = c('Individual', 'Diet', 'Varieties','Iterations', 'Hit meals', 'Path to hit meals'),
                        'Value' = c(person, diet, paste0(allowed_varieties, collapse = ","), iterations, results[['meals_created']], results[['path_file']]))
  
  write.xlsx(general,
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'General info',
             row.names= FALSE)
  write.xlsx(results[['nutrient_targets_wk']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Weekly nutrients',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['food_groups_wk']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Weekly serves',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_constraints']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Iterations (nutrients)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_fg']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Iterations (food groups)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_lk']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Iterations (linked foods)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(as.data.frame(results[['last_meal']]),
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Last meal created',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['nutrients_diff']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Difference (nutrients)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['serves_diff']],
             paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.'),
             sheetName = 'Difference (serves)',
             append = TRUE,
             row.names = FALSE)
}