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
foods <- foodData(filepath = filepath, redmeat_ids = redmeat_ids, diets = c('C', 'PF', 'H'), max_scale = 2)

#Exports to data/ folder--------------------------------------------------------
usethis::use_data(foods, overwrite = TRUE)
