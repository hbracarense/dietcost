#' Food group constraint data addition
#'
#' Adds serves constraints to food groups dataframe
#' @param filepath Path in which the dataset, in .xlsx format, is stored..
#' @param df Food groups dataframe.
#' @param diets Chosen diets. Constraint sheets in foods dataset must be of format 'constraints_DIETNAME_diet_food_groups', then the parameter passed will be DIETNAME. Can be a vector of diets in format c('DIETNAME1','DIETNAME2',...,'DIETNAMEN').
#' @return Food groups dataframe with added constraint data.
#' @examples 
#' food_groups_df <- addFoodGroupsConstraintData(filepath = 'C:/Users/username/Downloads/dataset.xlsx', food_groups_df, diets = c('C','PF', 'H'));
#' @export
addFoodGroupsConstraintData <- function(filepath, df, diets){
  standard_name_check(df, 'food_group')
  for(i in 1:length(diets)){
    sheet <- paste0('constraints_',diets[i],'_diet_food_groups')
    df1 <- upload_data(filepath, sheet)
    standard_name_check(df1, 'food_group')
    check_nom_num_df(df1[,!(names(df1) %in% "food_group")])
    df1 <- treat_groups_df(df1, diets[i])
    df1['food_group'] <- sapply(df1['food_group'], sauces_protein_discretionary_change)
    df <- join_function(df,df1, 'food_group')
    df <- tryCatch(
      expr = starchy_fill(df, 'Starchy vegetables', 'serve', 'max'),
      error = function(e){
        message('Impossible to fill starchy vegetables serves with parameters informed. Please check your dataset and try again!')
        stop(e)
      }
    )
  }
  df[,!(names(df) %in% c('food_group', 'food_group_id'))] <- df[,!(names(df) %in% c('food_group', 'food_group_id'))] %>% replace(is.na(.), 0)
  print('Food group constraint data added with success.')
  return(df)
}
