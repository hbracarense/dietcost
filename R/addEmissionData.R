#' Emission data addition
#'
#' Adds emission data to foods dataframe.
#' @param filepath Path in which the dataset, in .xlsx format, is stored.
#' @param df Foods dataframe.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @return Food dataframe with emission data.
#' @export
addEmissionData <- function(filepath, df, emission_cols = NULL){
  standard_name_check(df, 'food_id')
  df1 <- upload_data(filepath, 'emissions')
  if(is.null(emission_cols)){
    emission_cols <- c('CF_gCO2eq', 'WF_l', 'EF_g_m2')
  }
  cols <- c('food_id',emission_cols)
  for(i in 1:length(cols)){
    standard_name_check(df1, cols[i])
  }
  check_nom_num_df(df1[,(names(df1) %in% cols)])
  df <- join_function(df,df1[,cols],'food_id')
  message("Emission data added to food dataframe with success.")
  na_rows_emissions <-df[!complete.cases(df),]
  if(nrow(na_rows_emissions) > 0){
    lapply(na_rows_emissions$food_id, function(x) message(paste("ID",x,"is missing in emission sheets.")))
    stop("Check the data and rerun the application.")
  } else{
    message("All logged foods have emission data.")
  }
  return(df)
}