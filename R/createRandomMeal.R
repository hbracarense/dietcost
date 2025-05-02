#' Random meal plan
#'
#' Creates a random meal plan.
#' @param foods_df Foods dataframe.
#' @param targets_df Nutrient targets dataframe.
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param allowed_varieties Permitted food varieties. Can be a vector of the following: 1,2 and/or 3.
#' @param min_serve_size_difference Multiplier to serve difference. A float between 0 and 1.
#' @param allow_alcohol Boolean variable checking if alcohol is permitted. Default TRUE.
#' @param allow_discretionary Boolean variable checking if discretionary foods are permitted. Default TRUE.
#' @param allow_takeaway Boolean variable checking if takeaway is permitted. Default TRUE.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @param nutrient_cols Optional parameter. Nutrients column names if standard dataset isn't used.
#' @return Random meal plan dataframe.
#' @examples 
#' foods_df <- createRandomMeal(foods_df = foods,
#'                              targets_df = nutrient_targets,
#'                              person = 'man', 
#'                              diet = 'C', 
#'                              allowed_varieties = c(1,2,3), 
#'                              min_serve_size_difference = 0.5, 
#'                              allow_takeaway = TRUE, 
#'                              allow_alcohol = TRUE, 
#'                              allow_discretionary = TRUE)
#' @export
createRandomMeal <- function(foods_df, targets_df, person, diet, allowed_varieties, min_serve_size_difference, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE, emission_cols = NULL, nutrient_cols = NULL){
  add_range(allowed_varieties,1:3,'1, 2, 3')
  permitted_individuals(data.frame(individual = person))
  add_float_range(min_serve_size_difference,0,1)
  serve_size <- paste0('serve_size_', diet)
  min <- paste0(person,'_min_',diet)
  max <- paste0(person,'_max_',diet)
  if(is.null(emission_cols)){
    emission_cols <- c('CF_gCO2eq', 'WF_l', 'EF_g_m2')
  }
  if(is.null(nutrient_cols)){
    nutrient_cols <- c('energy_kj_g', 'fat_g', 'sat_fat_g', 'CHO_g', 'sugars_g', 'fibre_g', 'protein_g', 'sodium_mg')
  }
  standard_name_check(foods_df, 'food_id', 'food_name', 'food_group_id', 'food_group', 'variety', 'redmeat', serve_size, min, max, 'price')
  for(i in 1:length(emission_cols)){
    standard_name_check(foods_df, emission_cols[i])
  }
  for(i in 1:length(nutrient_cols)){
    standard_name_check(foods_df, nutrient_cols[i])
  }
  alcohol_id <- ifelse('Alcohol' %in% foods_df$food_group,
                       unique(foods_df$food_group_id[foods_df$food_group == 'Alcohol']),
                       0)
  
  discretionary_id <- ifelse('Discretionary foods' %in% foods_df$food_group,
                             unique(foods_df$food_group_id[foods_df$food_group == 'Discretionary foods']),
                             0)
  
  takeaway_id <- ifelse('Takeaway' %in% foods_df$food_group,
                        unique(foods_df$food_group_id[foods_df$food_group == 'Takeaway']),
                        0)
  
  diet_f <- diet
  targets_df <- targets_df %>% filter(.data$diet == diet_f & .data$individual == person)
  foods_df <- foods_df %>% filter(.data$variety %in% allowed_varieties) %>% select(all_of('food_id'),
                                                                             all_of('food_name'),
                                                                             all_of('food_group_id'),
                                                                             all_of('food_group'),
                                                                             all_of('redmeat'),
                                                                             all_of(serve_size),
                                                                             all_of(min),
                                                                             all_of(max),
                                                                             all_of(emission_cols),
                                                                             all_of(nutrient_cols),
                                                                             all_of('price'))
  if(!isTRUE(allow_discretionary)||(isTRUE(allow_discretionary) && targets_df$discretionary_perc_max == 0) && ('Discretionary foods' %in% foods_df$food_group)){
    foods_df <- foods_df %>% filter(.data$food_group_id != discretionary_id)
  }
  if(!isTRUE(allow_alcohol)||(isTRUE(allow_alcohol) && targets_df$alcohol_perc_max == 0) && ('Alcohol' %in% foods_df$food_group)){
    foods_df <- foods_df %>% filter(.data$food_group_id != alcohol_id)
  }
  if(!isTRUE(allow_takeaway)||(isTRUE(allow_takeaway) && targets_df$takeaway_perc_max == 0) && ('Takeaway' %in% foods_df$food_group)){
    foods_df <- foods_df %>% filter(.data$food_group_id != takeaway_id)
  }
  foods_df$intake <- double(nrow(foods_df))
  foods_df <- random_plan(foods_df, 'food_group_id', discretionary_id)
  foods_df <- random_plan(foods_df, 'food_group_id', alcohol_id)
  foods_df <- random_plan(foods_df, 'food_group_id', takeaway_id)
  for(i in 1:nrow(foods_df)){
    
    ifelse(unlist(foods_df[i,min])<=unlist(foods_df[i,max]),
           {
             serve_range <- seq(unlist(foods_df[i,min]), unlist(foods_df[i,max]), unlist(foods_df[i,serve_size])*min_serve_size_difference)
             foods_df$intake[i] <- sample_safe(serve_range) 
           },
           stop(paste("Check your data! Food ID",unlist(foods_df[i,'food_id']),"for",person,"with diet",diet,"has minimum serve size column higher than maximum."))
    )
  }
  names(foods_df) <- sapply(strsplit(as.character(names(foods_df)), paste0('_',diet)), `[[`, 1)
  names(foods_df)[names(foods_df) == paste0(person,'_max')] <- 'max'
  names(foods_df)[names(foods_df) == paste0(person,'_min')] <- 'min'
  foods_df$serves <- foods_df$intake/foods_df$serve_size
  return(foods_df)
}
