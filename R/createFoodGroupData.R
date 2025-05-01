#' Food group data creation
#'
#' Creates and populates a food group data dataframe
#' @param df Foods dataframe.
#' @return Food group dataframe.
#' @examples 
#' food_groups_df <- createFoodGroupData(foods_df);
#' @export
createFoodGroupData <- function(df){
  commonly_absent_groups <- c('Starchy vegetables', 'Alcohol', 'Discretionary foods', 'Takeaway')
  standard_name_check(df,'food_group', 'food_group_id')
  groups <- unique(df$food_group)
  for(i in 1:length(commonly_absent_groups)){
    if(!(commonly_absent_groups[i] %in% groups)){
      groups <- c(groups, commonly_absent_groups[i])
    }
  }
  fill_column_food_groups_df <- lapply(groups,function(group){
    food_group <- group
    
    data.frame(
      food_group
    )
  })
  food_groups_df <- do.call(rbind, fill_column_food_groups_df)
  rownames(food_groups_df) <- NULL
  food_groups_df <- join_function(food_groups_df, unique(df[,c('food_group', 'food_group_id')]),'food_group')
  food_groups_df <- food_groups_df[order(food_groups_df$food_group_id),]
  if(nrow(food_groups_df)>1){
    for(i in 1:nrow(food_groups_df)){
      if(is.na(food_groups_df$food_group_id[i])){
        food_groups_df$food_group_id[i] <- food_groups_df$food_group_id[i-1]+1
      }
    }
  }
  
  unique_values(food_groups_df$food_group_id, food_groups_df, 'food_group_id', "food group ID")
  print("Food group dataframe created with success.")
  return(food_groups_df)
}
