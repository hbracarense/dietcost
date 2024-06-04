os#Installs and imports libraries-------------------------------------------------
install.packages('devtools')
library(devtools)
install_github("zzz13hj/dietcost")
library(DIETCOST)

#Global parameters--------------------------------------------------------------
setwd('C:/Users/username/directory')
redmeat_ids <- c(71003, 71008, 71041, 81005, 81021, 81022, 81026, 81027, 81029)
filepath <- 'dataset.xlsx'
linked_bread_low <- c("79034", "79015", "79016", "79032", "65048")
linked_bread_high <- c("80001", "80019", "81029", "81027", "81021", "70044", "69009")
linked_milk_low <- c("69029", "79012", "70038", "69043")
linked_milk_high <- c("79020", "79001", "79036", "79003", "79039")

#Initiates analysis-------------------------------------------------------------
foods_df <- createFoodData(filepath = filepath, redmeat_ids = redmeat_ids)
foods_df <- addEmissionData(filepath = filepath, df = foods_df)
foods_df <- addConstraintData(filepath = filepath, df = foods_df, diet = c('C', 'PF', 'H'), max_scale = 2)
foods_df <- addNutrientData(filepath = filepath, df = foods_df)
foods_df <- addPriceData(filepath = filepath, foods_df)
food_groups_df <- createFoodGroupData(foods_df)
food_groups_df <- addFoodGroupsConstraintData(filepath = filepath, food_groups_df, diets = c('C','PF', 'H'))
nutrient_targets <- createNutrientTargets(filepath = filepath, allow_takeaway = FALSE, alcohol_perc_max = 20, discretionary_perc_max = 100)
results <- monteCarlo(100000, foods_df, nutrient_targets, food_groups_df, person = 'woman', 'PF', c(1,2,3), 0.5, allow_discretionary = TRUE, allow_alcohol = TRUE, allow_takeaway = TRUE, linked_low_1 = linked_bread_low, linked_high_1 = linked_bread_high, linked_low_2 = linked_milk_low, linked_high_2 = linked_milk_high)
