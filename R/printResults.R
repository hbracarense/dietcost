#' Exportation of Monte Carlo results
#' 
#' Exports, in .xlsx format, the results of Monte Carlo simulation.
#' @param file_path A string containing the path where the file will be saved.
#' @param results List of results
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param allowed_varieties Permitted food varieties. Can be a vector of the following: 1,2 and/or 3.
#' @param iterations Number of iterations. Integer.
#' @return No R object return, prints a Excel workbook.
#' @export
printResults <- function(file_path, results, person, diet, allowed_varieties,iterations){
  general <- data.frame('Variable' = c('Individual', 'Diet', 'Varieties','Iterations', 'Hit meals', 'Path to hit meals'),
                        'Value' = c(person, diet, paste0(allowed_varieties, collapse = ","), iterations, results[['meals_created']], results[['path_file']]))
  file_name <- paste(paste('results',person,diet,format(Sys.time(), "%Y-%m-%d_%H-%M"),sep ='_'),'xlsx',sep = '.')
  write.xlsx(general,
             file.path(file_path, file_name),
             sheetName = 'General info',
             row.names= FALSE)
  write.xlsx(results[['nutrient_targets_wk']],
             file.path(file_path, file_name),
             sheetName = 'Weekly nutrients',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['food_groups_wk']],
             file.path(file_path, file_name),
             sheetName = 'Weekly serves',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_constraints']],
             file.path(file_path, file_name),
             sheetName = 'Iterations (nutrients)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_fg']],
             file.path(file_path, file_name),
             sheetName = 'Iterations (food groups)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['iterations_lk']],
             file.path(file_path, file_name),
             sheetName = 'Iterations (linked foods)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(as.data.frame(results[['last_meal']]),
             file.path(file_path, file_name),
             sheetName = 'Last meal created',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['nutrients_diff']],
             file.path(file_path, file_name),
             sheetName = 'Difference (nutrients)',
             append = TRUE,
             row.names = FALSE)
  write.xlsx(results[['serves_diff']],
             file.path(file_path, file_name),
             sheetName = 'Difference (serves)',
             append = TRUE,
             row.names = FALSE)
}