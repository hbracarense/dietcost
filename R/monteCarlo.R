#' Monte Carlo simulation
#' 
#' Creates a Monte Carlo simulation to a given number of iterations. A hit meal consists of one that returnz zero difference between nutrient targets and random meal plan, food groups serves and respects lower linked foods serves lower or equal to higher linked foods serves, if existent.
#' @param dir_path A string containing the path where a directory will be created.
#' @param iterations Number of iterations. Integer.
#' @param foods_df Foods dataframe.
#' @param nutrient_targets_df Nutrient constraints dataframe.
#' @param food_group_targets_df Food group serves dataframe.
#' @param person Individual whose random meal plan will be created to. Can be one of man, woman, boy or girl.
#' @param diet Chosen diet. Must be DIETNAME from 'constraints_DIETNAME_diet_foods' sheet in dataset.
#' @param allowed_varieties Permitted food varieties. Can be a vector of the following: 1,2 and/or 3.
#' @param min_serve_size_difference Multiplier to serve difference. A float between 0 and 1.
#' @param allow_alcohol Boolean variable checking if alcohol is permitted. Default TRUE.
#' @param allow_discretionary Boolean variable checking if discretionary foods are permitted. Default TRUE.
#' @param allow_takeaway Boolean variable checking if takeaway is permitted. Default TRUE.
#' @param emission_cols Optional parameter. Emission column names if standard dataset isn't used.
#' @param nutrient_cols Optional parameter. Nutrients column names if standard dataset isn't used.
#' @param nutrient_constraints Optional parameter. Vector of nutrients column names to be used if not all nutrients are to be used as constraints.
#' @param linked_low_1 Optional parameter. Vector of lower bound food IDs.
#' @param linked_high_1 Optional parameter. Vector of higher bound food IDs.
#' @param linked_low_2 Optional parameter. Vector of lower bound food IDs.
#' @param linked_high_2 Optional parameter. Vector of higher bound food IDs.
#' @return List of dataframes, containing results of simulation.
#' @export
monteCarlo <- function(dir_path, iterations, foods_df, nutrient_targets_df, food_group_targets_df, person, diet, allowed_varieties, min_serve_size_difference, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE, emission_cols = NULL, nutrient_cols = NULL, nutrient_constraints = NULL, linked_low_1 = NULL, linked_high_1 = NULL, linked_low_2 = NULL, linked_high_2 = NULL){
  if(round(iterations) != iterations || iterations <= 0){
    stop('Iterations must be a positive integer!')
  }
  new_dir <- paste0('results_', format(Sys.time(), '%Y%m%d%H%M%S'))
  dir.create(file.path(dir_path, new_dir))
  hash_list <- list()
  
  meal_plan <- createRandomMeal(foods_df = foods_df, targets_df = nutrient_targets_df, person = person, diet = diet, allowed_varieties = allowed_varieties, min_serve_size_difference = min_serve_size_difference, allow_takeaway = allow_takeaway, allow_alcohol = allow_alcohol, allow_discretionary = allow_discretionary, emission_cols = emission_cols, nutrient_cols = nutrient_cols)
  message(paste('All meals formed will be saved as .csv files in directory', file.path(dir_path, new_dir)))
  for(i in 1:iterations){
    nutrients_plan <- getPerc(getNutrients(df = meal_plan, nutrient_cols = nutrient_cols),meal_plan)
    serves_plan <- getFoodGroupServes(df = meal_plan)
    nutrient_targets_wk <- convertWeeklyNutrientTargets(nutrient_targets_df, diet = diet, person = person, nutrient_constraints = nutrient_constraints)
    food_groups_wk <- convertWeeklyFoodGroups(food_group_targets_df, diet = diet, individual = person)
    nutrients_diff <- getDifference(df_target = nutrient_targets_wk, df_nutrients = nutrients_plan, merge_col = 'nutrient')
    serves_diff <- getDifference(df_target = food_groups_wk, df_nutrients = serves_plan, merge_col = ,c('food_group','food_group_id'))
    off_measures <- NULL
    off_food_groups <- NULL
    off_linked_foods_low <- NULL
    off_linked_foods_high <- NULL
    target_measure <- NULL
    target_fg <- NULL
    serve_range <- NULL
    food <- NULL
    
    if((!is.null(linked_low_1)) && (!is.null(linked_high_1)) && (is.null(linked_low_1)) && (is.null(linked_low_1))){
      linked_sum_1 <- checkLinkedFoods(df = meal_plan, low = linked_low_1, high = linked_high_1)
    }
    if((!is.null(linked_low_1)) && (!is.null(linked_high_1)) && (!is.null(linked_low_1)) && (!is.null(linked_low_1))){
      linked_sum_1 <- checkLinkedFoods(df = meal_plan, low = linked_low_1, high = linked_high_1)
      linked_sum_2 <- checkLinkedFoods(df = meal_plan, low = linked_low_2, high = linked_high_2)
    }
    if(i == 1){
      iterations_constraints = data.frame(nutrient = nutrients_diff$nutrient,
                                          high = integer(nrow(nutrients_diff)),
                                          low = integer(nrow(nutrients_diff)))
      
      iterations_fg = data.frame(food_group = serves_diff$food_group,
                                 food_group_id = serves_diff$food_group_id,
                                 high = integer(nrow(serves_diff)),
                                 low = integer(nrow(serves_diff)))
      
      if((is.null(linked_low_1)) && (is.null(linked_high_1)) && (is.null(linked_low_1)) && (is.null(linked_low_1))){
        iterations_lk <- data.frame(link = 'no_link')
      }
      
      if((!is.null(linked_low_1)) && (!is.null(linked_high_1)) && (is.null(linked_low_1)) && (is.null(linked_low_1))){
        iterations_lk <- data.frame(link = 'pair_1',
                                    high = integer(1),
                                    low = integer(1))
      }
      if((!is.null(linked_low_1)) && (!is.null(linked_high_1)) && (!is.null(linked_low_1)) && (!is.null(linked_low_1))){
        iterations_lk <- data.frame(link = c('pair_1', 'pair_2'),
                                    high = integer(2),
                                    low = integer(2))
      }
      
    }
    message(paste('Iteration:',i))
    
    if(isTRUE(checkZeroDiff(nutrients_diff))){
      if(isTRUE(checkZeroDiff(serves_diff))){
        if((is.null(linked_low_1) && is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2))||(!is.null(linked_low_1) && !is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2) && linked_sum_1 >=0)||(!is.null(linked_low_1) && !is.null(linked_high_1) && !is.null(linked_low_2) && !is.null(linked_high_2) && linked_sum_1 >=0 && linked_sum_2 >=0)){
          message('Hit!')
          hash_diet <- hash(meal_plan)
          if(!(hash_diet %in% hash_list)){
            hash_list[[length(hash_list)+1]] <- hash_diet
            message('Unique diet formed!')
            meal_plan <- priceEmissionData(nutrientDataCalculation(meal_plan, nutrient_cols = nutrient_cols), emission_cols = emission_cols)
            file_name <- paste0('meal_plan_',i,'.csv')
            write.csv(meal_plan, file.path(dir_path, new_dir, file_name), row.names=FALSE)
          } else{
            message('Diet already logged in!')
          }
          food = sample_safe(meal_plan$food_id)
          serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
        } else{
          if(!is.null(linked_low_1) && !is.null(linked_high_1) && is.null(linked_low_2) && is.null(linked_high_2) && linked_sum_1 < 0){
            off_linked_foods_low <- linked_low_1
            off_linked_foods_high <- linked_high_1
          } else{
            if(linked_sum_1 < 0 && linked_sum_2 < 0){
              off_linked_foods_low <- c(linked_low_1, linked_low_2)
              off_linked_foods_high <- c(linked_high_1, linked_high_2)
            } else if(linked_sum_1 < 0 && linked_sum_2 >= 0){
              off_linked_foods_low <- linked_low_1
              off_linked_foods_high <- linked_high_1
            } else if(linked_sum_1 >= 0 && linked_sum_2 < 0){
              off_linked_foods_low <- linked_low_2
              off_linked_foods_high <- linked_high_2
            }
          }
          if(length(off_linked_foods_low) > 0 && length(off_linked_foods_high) > 0){
            direction_choices <- c('<', '>')
            direction <- sample_safe(direction_choices)
            if(direction == '<'){
              tmp <- meal_plan$food_id[meal_plan$food_id %in% off_linked_foods_low]
              food <- sample_safe(tmp)
              if(food %in% linked_low_1){
                fl <- 'pair_1'
              } else{
                fl <- 'pair_2'
              }
              serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
              message(paste0(c(paste0('Food link ', fl, ' is off. ', meal_plan$food_name[meal_plan$food_id == food], ' affects it at lower half. Current intake is ',meal_plan$intake[meal_plan$food_id == food], ' and it must be between ', meal_plan$min[meal_plan$food_id == food], ' and ', meal_plan$max[meal_plan$food_id == food],'. Options: '),serve_range), collapse = " "))
              iterations_lk$low[iterations_lk$link == fl] <- iterations_lk$low[iterations_lk$link == fl] + 1
            } else{
              tmp <- meal_plan$food_id[meal_plan$food_id %in% off_linked_foods_high]
              food <- sample_safe(tmp)
              if(food %in% linked_high_1){
                fl <- 'pair_1'
              } else{
                fl <- 'pair_2'
              }
              serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
              message(paste0(c(paste0('Food link ', fl, ' is off. ', meal_plan$food_name[meal_plan$food_id == food], ' affects it at upper half. Current intake is ',meal_plan$intake[meal_plan$food_id == food], ' and it must be between ', meal_plan$min[meal_plan$food_id == food], ' and ', meal_plan$max[meal_plan$food_id == food],'. Options: '),serve_range), collapse = " "))
              iterations_lk$high[iterations_lk$link == fl] <- iterations_lk$high[iterations_lk$link == fl] + 1
            }
          }
        }
        
      } else{
        off_food_groups <- serves_diff[serves_diff$value != 0,]
        target_fg <- sample_safe(off_food_groups$food_group_id)
        
        foods_impacted <- meal_plan$food_id[meal_plan$food_group_id == target_fg]
        if(length(foods_impacted) == 0){
          message(paste('No food impact group:',target_fg))
          next
        }
        food <- sample_safe(foods_impacted)
        fg <- meal_plan$food_group[meal_plan$food_id == food]
        if(off_food_groups$value[off_food_groups$food_group_id == target_fg] > 0){
          message(paste0('Food group ', fg, ' has too many serves. Current: ', serves_plan$value[serves_plan$food_group_id == target_fg],'. Max: ', food_groups_wk$max[food_groups_wk$food_group_id == target_fg]))
          serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
          iterations_fg$high[iterations_fg$food_group_id == target_fg] <- iterations_fg$high[iterations_fg$food_group_id == target_fg] + 1
        } else{
          message(paste0('Food group ', fg, ' has too few serves. Current: ', serves_plan$value[serves_plan$food_group_id == target_fg],'. Min: ', food_groups_wk$min[food_groups_wk$food_group_id == target_fg]))
          serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
          iterations_fg$low[iterations_fg$food_group_id == target_fg] <- iterations_fg$low[iterations_fg$food_group_id == target_fg] + 1
        }
        message(paste0(c(paste0(meal_plan$food_name[meal_plan$food_id == food]," has current intake of ",meal_plan$intake[meal_plan$food_id == food]," and it must be between ",meal_plan$min[meal_plan$food_id == food]," and ",meal_plan$max[meal_plan$food_id == food],". Options: "),serve_range), collapse = " "))
      }
    } else{
      off_measures <- nutrients_diff[nutrients_diff$value != 0,]
      target_measure <- sample_safe(off_measures$nutrient)
      
      if(target_measure == 'alcohol_perc'){
        foods_impacted <- meal_plan$food_id[meal_plan$food_group == 'Alcohol']
      } else if(target_measure == 'discretionary_perc'){
        foods_impacted <-meal_plan$food_id[meal_plan$food_group == 'Discretionary foods']
      } else if(target_measure == 'takeaway_perc'){
        foods_impacted <-meal_plan$food_id[meal_plan$food_group == 'Takeaway']
      } else if(target_measure == 'redmeat'){
        foods_impacted <-meal_plan$food_id[meal_plan$redmeat == TRUE]
      } else if(target_measure == 'sodium'){
        foods_impacted <-meal_plan$food_id[meal_plan$sodium_mg > 0]
      } else if(target_measure == 'energy'){
        foods_impacted <-meal_plan$food_id[meal_plan$energy_kj_g > 0]
      } else{
        col <- target_measure
        if(grepl('perc', col)){
          col <- unlist(strsplit(col, '_perc',1))
        }
        col <- paste0(col, '_g')
        foods_impacted <- meal_plan$food_id[meal_plan[col] > 0]
      }
      if(length(foods_impacted) == 0){
        message(paste('No food impact measure:',target_measure))
        next
      }
      food <- sample_safe(foods_impacted)
      if(off_measures$value[off_measures$nutrient == target_measure] > 0){
        message(paste0('We are too high on ',target_measure,'. Current: ',nutrients_plan$value[nutrients_plan$nutrient == target_measure],'. Max: ',nutrient_targets_wk$max[nutrient_targets_wk$nutrient == target_measure]))
        serve_range <- sort(seq(meal_plan$min[meal_plan$food_id == food], meal_plan$intake[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
        if(length(serve_range) > 10){
          serve_range <- tail(serve_range, 10)
        }
        iterations_constraints$high[iterations_constraints$nutrient == target_measure] <- iterations_constraints$high[iterations_constraints$nutrient == target_measure] + 1
      } else{
        message(paste0('We are too low on ',target_measure,'. Current: ',nutrients_plan$value[nutrients_plan$nutrient == target_measure],'. Min: ',nutrient_targets_wk$min[nutrient_targets_wk$nutrient == target_measure]))
        serve_range <- sort(seq(meal_plan$intake[meal_plan$food_id == food], meal_plan$max[meal_plan$food_id == food], meal_plan$serve_size[meal_plan$food_id == food]*min_serve_size_difference))
        if(length(serve_range) > 10){
          serve_range <- head(serve_range, 10)
        }
        iterations_constraints$low[iterations_constraints$nutrient == target_measure] <- iterations_constraints$high[iterations_constraints$nutrient == target_measure] + 1
      }
      message(paste0(paste0(c(paste0(meal_plan$food_name[meal_plan$food_id == food]," impacts ", target_measure, " and intake must be between ", meal_plan$min[meal_plan$food_id == food]," and ",meal_plan$max[meal_plan$food_id == food],". Options:"),serve_range), collapse = " "),". Current: ",meal_plan$intake[meal_plan$food_id == food]))
    }
    
    if(!is.null(serve_range)){
      new_intake <- sample_safe(serve_range)
      message(paste0('Changing ', meal_plan$food_name[meal_plan$food_id == food],' intake from ',meal_plan$intake[meal_plan$food_id == food],' to ',new_intake))
      meal_plan$intake[meal_plan$food_id == food] <- new_intake
    }
  }
  results <- list(path_file = file.path(dir_path, new_dir),
                  meals_created = length(hash_list),
                  last_meal = priceEmissionData(nutrientDataCalculation(meal_plan, nutrient_cols = nutrient_cols), emission_cols = emission_cols),
                  iterations_constraints = iterations_constraints,
                  iterations_fg = iterations_fg,
                  iterations_lk = iterations_lk,
                  nutrients_diff = nutrients_diff,
                  serves_diff = serves_diff,
                  nutrient_targets_wk = nutrient_targets_wk,
                  food_groups_wk = food_groups_wk)
  return(results)
}