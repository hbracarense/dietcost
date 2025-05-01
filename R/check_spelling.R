#' Spellcheck
#'
#' Checks if two datasets have the same spelling in names column.
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param condition Column to be joined.
#' @examples 
#' check_spelling(foods_df, emission_df, 'food_id')
#' @export
check_spelling <- function(df1, df2, condition){
  df <- join_function(df1, df2, condition)
  df <- df %>% mutate(
    result = case_when(
      df$'food_name.x' == df$'food_name.y'~0,
      df$'food_name.x' != df$'food_name.y'~1
    )
  )
  
  if(any(df$result == 1)){
    df_e = df %>% filter(result == 1)
    for(i in 1:nrow(df_e)){
      e = paste("ID",df_e$'food_id'[i],"has distinct names in both sheets:",df_e$'food_name.x'[i],"and",df_e$'food_name.y'[i],".")
      print(e)
      if(i == nrow(df_e)){
        rm(df)
        rm(df_e)
        stop("Check the data and rerun the application.")
      }
    } 
    
  } else{
    print("No name mismatches between datasets.")
  }
  rm(df)
}