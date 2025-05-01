library(DIETCOST)

#Global parameters--------------------------------------------------------------
setwd('home/inst/extdata/')
redmeat_ids <- c(71003, 71008, 71041, 81005, 81021, 81022, 81026, 81027, 81029)
filepath <- 'dataset.xlsx'
linked_bread_low <- c("79034", "79015", "79016", "79032", "65048")
linked_bread_high <- c("80001", "80019", "81029", "81027", "81021", "70044", "69009")
linked_milk_low <- c("69029", "79012", "70038", "69043")
linked_milk_high <- c("79020", "79001", "79036", "79003", "79039")

#Initiates analysis-------------------------------------------------------------
nutrient_targets <- createNutrientTargets(filepath = filepath, allow_takeaway = FALSE, alcohol_perc_max = 20, discretionary_perc_max = 100)
nutrient_targets <- nutrient_targets[!(nutrient_targets$diet %in% 'PV'),]

#Exports to data/ folder--------------------------------------------------------
usethis::use_data(nutrient_targets, overwrite = TRUE)
