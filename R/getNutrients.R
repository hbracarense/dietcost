#' Nutrients values calculator
#' 
#' Calculates nutritional value of meal plan.
#' @param df Random meal plan.
#' @param nutrient_cols Optional parameter. Vector of nutrients column names to be used if nutrients are different from standard dataset.
#' @return Nutrients dataframe.
#' @examples
#' df <- getPerc(getNutrients(random_meal),random_meal);
#' 
#' @export
getNutrients <- function(df, nutrient_cols = NULL){
  if(is.null(nutrient_cols)){
    nutrient_cols <- c('energy_kj_g','fat_g','sat_fat_g','CHO_g','sugars_g','fibre_g','protein_g','sodium_mg')
  }
  for(i in 1:length(nutrient_cols)){
    standard_name_check(df,nutrient_cols[i])
  }
  standard_name_check(df, 'intake', 'serves', 'redmeat', 'food_id', 'food_name', 'food_group', 'food_group_id')
  
  col_n <- integer(length(nutrient_cols))
  suppressWarnings(for(i in 1:length(nutrient_cols)){
    col_n[i] <- grep(nutrient_cols[i], colnames(df))
  })
  
  df <- tryCatch(
    expr = {
      df %>% mutate(
        across(
          .cols = all_of(col_n),
          .fns = function(x){
            (x/100)*df$intake
          }
        )
      )
    },
    error = function(e){
      print('Check your data!')
      stop(e)
    }
  )
  
  nutrient_names <- remove_suffix(nutrient_cols,'_g','_mg')
  if('energy_kj' %in% nutrient_names){
    nutrient_names <- replace(nutrient_names, nutrient_names == 'energy_kj', 'energy')
  }
  nutrients <- data.frame(nutrient = nutrient_names,
                          value = double(length(nutrient_names)))
  for(i in 1:nrow(nutrients)){
    nutrients$value[i] <- sum(df[,col_n[i]])
  }
  nutrients[nrow(nutrients) + 1,] <- c('redmeat',sum(df$intake[df$redmeat == TRUE]))
  
  return(nutrients)
}
