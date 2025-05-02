#' ID mismatch check
#'
#' Checks if a given food has an ID assigned but is absent in another dataset.
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param value Dataset name.
#' @export
check_id_defined <- function(df1, df2, value){
  ids2 <- df2 %>% pull(.data$food_id)
  check_df <- df1 %>% filter(!(.data$food_id %in% unique(ids2))) %>% pull(.data$food_id)
  if(length(check_df)>0){
    for(i in 1:length(check_df)){
      print(paste("ID",check_df[i],"has its ", value," defined but isn't present in food data."))
    }
    stop("Loading failed!")
  } else{
    print(paste("Every ID is accounted for."))
  }
}