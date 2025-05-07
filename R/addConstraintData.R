#' Food constraint data addition
#'
#' Adds nutrients constraint data, according to chosen diet, to foods dataframe.
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param df Foods dataframe.
#' @param diets Chosen diets. Constraint sheets in foods dataset must be of format 'constraints_DIETNAME_diet_foods', then the parameter passed will be DIETNAME. Can be a vector of diets in format c('DIETNAME1','DIETNAME2',...,'DIETNAMEN').
#' @param max_scale Maximum scale.
#' @param override_min If is not null, overrides all minimum values.
#' @return Foods dataframe with constraints columns.
#' @export
addConstraintData <- function(filepath, df, diets, max_scale, override_min = NULL){
  standard_name_check(df, 'food_id', 'food_name', 'food_group')
  for(i in 1:length(diets)){
    sheet <- paste0('constraints_',diets[i],'_diet_foods')
    min <- 'min'
    max <- 'max'
    diet_suffix <- paste0('_',diets[i])
    sheet <- paste0('constraints_',diets[i],'_diet_foods')
    df1 <- upload_data(filepath, sheet)
    standard_name_check(df1, 'food_id', 'food_name', 'serve_size')
    df1$food_group <- sapply(df1$food_group, sauces_protein_discretionary_change)
    lapply(df1$food_name, check_function, 'food names')
    check_spelling(df[,c('food_id', 'food_name')], df1[,c('food_id', 'food_name')],'food_id')
    df1 <- treat_df(df1,min, max, diet_suffix, max_scale, override_min) 
    check_nom_num_df(df1[,!(names(df1) %in% c('food_id', 'food_name', "food_group"))])
    df <- join_function(df, df1, 'food_id')
  }
  message('Food constraints added with success.')
  return(df)
}
