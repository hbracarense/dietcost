#' Nutrient targets conversion
#' 
#' Converts nutrient targets dataframe to weekly values.
#' @param df Nutrient targets dataframe.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param nutrient_constraints Optional parameter. Vector of nutrients column names to be used if not all nutrients are to be used as constraints.
#' @return Converted nutrient targets dataframe.
#' @examples
#' nutrient_targets_wk <- DIETCOST::convertWeeklyNutrientTargets(DIETCOST::nutrient_targets, 'C', 'man')
#' @export
convertWeeklyNutrientTargets <- function(df, diet, person, nutrient_constraints = NULL){
  permitted_individuals(data.frame(individual = person))
  min <- 'min'
  max <- 'max'
  percentage <- 'perc'
  serve <- 'serve'
  standard_name_check(df, 'individual', 'diet')
  df <- df[(df$diet == diet & df$individual == person),]
  serve_n <- grep(serve,colnames(df))
  if(length(serve_n) > 0){
    df <- subset(df,select = -serve_n)
  }
  individual_n <- grep('individual',colnames(df))
  diet_n <- grep('diet',colnames(df))
  percentage_n <- grep(percentage,colnames(df))
  exclusion_n <- c(individual_n,diet_n,percentage_n)
  df <- converts_dataframe(df, exclusion_n)
  cols_max <- grep('max',colnames(df))
  cols_min <- grep('min',colnames(df))
  nutrient_names <- unlist(strsplit(colnames(df[,cols_max]),'_max',1))
  nutrient_names <- remove_suffix(nutrient_names, '_grams','_mgrams')
  if('energy_kj' %in% nutrient_names){
    nutrient_names <- replace(nutrient_names, nutrient_names == 'energy_kj', 'energy')
  }
  df_targets <- data.frame(nutrient = nutrient_names,
                           min = double(length(nutrient_names)),
                           max = double(length(nutrient_names)))
  for(i in 1:nrow(df_targets)){
    df_targets$min[i] <- df[cols_min[i]]
    df_targets$max[i] <- df[cols_max[i]]
  }
  if(!is.null(nutrient_constraints)){
    for(i in 1:length(nutrient_constraints)){
      drop <- ifelse(grepl('_kj_g',nutrient_constraints[i]),
                     '_kj_g',
                     ifelse(grepl('_kj', nutrient_constraints[i]),
                            '_kj',
                            ifelse(grepl('_g',nutrient_constraints[i]),
                                   '_g',
                                   ifelse(grepl('_grams',nutrient_constraints[i]),
                                          '_grams',
                                          ifelse(grepl('_mg',nutrient_constraints[i]),
                                                 '_mg',
                                                 ifelse(grepl('_mgrams', nutrient_constraints[i]),
                                                        '_mgrams',
                                                        ifelse(grepl('_max',nutrient_constraints[i]),
                                                               '_max',
                                                               ifelse(grepl('_min', nutrient_constraints[i]),
                                                                      '_min',
                                                                      ifelse(grepl(paste0('_',person),nutrient_constraints[i]),
                                                                             paste0('_',person),
                                                                             ifelse(grepl(paste0('_',diet),nutrient_constraints[i]),
                                                                                    paste0('_',diet),
                                                                                    'not'))))))))))
      if(drop != 'not'){
        nutrient_constraints[i] <- unlist(strsplit(nutrient_constraints[i],drop,1))
      }
      if(!(nutrient_constraints[i] %in% df_targets$nutrient)){
        stop(paste(nutrient_constraints[i],"isn't listed in constraints. Check your data!"))
      }
    }
    df_targets <- df_targets[df_targets$nutrient %in% nutrient_constraints,]
  }
  return(df_targets)
}