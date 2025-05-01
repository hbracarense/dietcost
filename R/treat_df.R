#' Pre-treatment of constraint data
#'
#' Pre-treatment of constraints dataframe.
#' @param df Dataframe to be treated.
#' @param min_identifier Minimum value column identifier. 'Min' in standard dataset.
#' @param max_identifier Maximum value column identifier. 'Max' in standard dataset.
#' @param suffix Suffix to be added to column name.
#' @param max_scale Maximum scale. Default is two.
#' @param max_scale Maximum scale. Default is two.
#' @param override_min If is not null, overrides all minimum values.
#' @return Treated dataframe.
#' @examples 
#' treat_df(df1,min, max, diet_suffix, max_scale, override_min)
#' @export
treat_df <- function(df, min_identifier, max_identifier, suffix, max_scale, override_min){
  df_max <- df[,grepl(max_identifier, names(df))] %>% replace(is.na(.), 0)
  df_max[] <- lapply(df_max, FUN = function(x) if (is.numeric(x)) return(x * 2 * max_scale) else return(x))
  df_id = df[,c('food_id','serve_size')]
  df_min <- df[,grepl(min_identifier, names(df))] %>% replace(is.na(.), 0)
  if (is.null(override_min)){
    df_min[] <- lapply(df_min, FUN = function(x) if (is.numeric(x)) return(x * 2) else return(x))
  } else{
    df_min[] <- lapply(df_min, FUN = function(x) x = override_min)
    print(paste("All food min overriden to", override_min))
  }
  df <- data.frame(cbind(df_id,df_min, df_max))
  col1 = 'serve_size'
  df <- df %>% rename_with(~paste0(.,suffix),UQ(sym(col1)):UQ(sym(colnames(df)[ncol(df)])))
  rm(df_max)
  rm(df_min)
  rm(df_id)
  return(df)
}
