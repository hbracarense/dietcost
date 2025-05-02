#' Foods dataset example
#' 
#' A set of data containing commonly available foods based on a Brazilian typical diet.
#' @docType data
#' @keywords dataset
#' @name foods
#' @format A dataframe with 99 rows and 45 columns:
#' \describe{
#'  \item{food_group}{Food group, i.e. 'Fruit' or 'Vegetable'}
#'  \item{food_group_id}{Numerical code for dood group}
#'  \item{food_name}{Food name, i.e. LEMON}
#'  \item{food_id}{Unique numerical food id}
#'  \item{variety}{Variety. Must be 1, 2 or 3}
#'  \item{redmeat}{Boolean redmeat identifier}
#'  \item{CF_gCO2eq}{Carbon footprint}
#'  \item{WF_l}{Water footprint}
#'  \item{EF_g_m2}{Ecological footprint}
#'  \item{serve_size_C}{Serve size for current diet, in grams}
#'  \item{man_min_C}{Minimal current diet intake for males, in grams}
#'  \item{woman_min_C}{Minimal current diet intake for females, in grams}
#'  \item{boy_min_C}{Minimal current diet intake for boys, in grams}
#'  \item{girl_min_C}{Minimal current diet intake for girls, in grams}
#'  \item{man_max_C}{Maximal current diet intake for males, in grams}
#'  \item{woman_max_C}{Maximal current diet intake for females, in grams}
#'  \item{boy_max_C}{Maximal current diet intake for boys, in grams}
#'  \item{girl_max_C}{Maximal current diet intake for girls, in grams}
#'  \item{serve_size_PF}{Serve size for EAT-Lancet diet, in grams}
#'  \item{man_min_PF}{Minimal EAT-Lancet diet intake for males, in grams}
#'  \item{woman_min_PF}{Minimal EAT-Lancet diet intake for females, in grams}
#'  \item{boy_min_PF}{Minimal EAT-Lancet diet intake for boys, in grams}
#'  \item{girl_min_PF}{Minimal EAT-Lancet diet intake for girls, in grams}
#'  \item{man_max_PF}{Maximal EAT-Lancet diet intake for males, in grams}
#'  \item{woman_max_PF}{Maximal EAT-Lancet diet intake for females, in grams}
#'  \item{boy_max_PF}{Maximal EAT-Lancet diet intake for boys, in grams}
#'  \item{girl_max_PF}{Maximal EAT-Lancet diet intake for girls, in grams}
#'  \item{serve_size_H}{Serve size for healthy diet, in grams}
#'  \item{man_min_H}{Minimal healthy diet intake for males, in grams}
#'  \item{woman_min_H}{Minimal healthy diet intake for females, in grams}
#'  \item{boy_min_H}{Minimal healthy diet intake for boys, in grams}
#'  \item{girl_min_H}{Minimal healthy diet intake for girls, in grams}
#'  \item{man_max_H}{Maximal healthy diet intake for males, in grams}
#'  \item{woman_max_H}{Maximal healthy diet intake for females, in grams}
#'  \item{boy_max_H}{Maximal healthy diet intake for boys, in grams}
#'  \item{girl_max_H}{Maximal healthy diet intake for girls, in grams}
#'  \item{energy_kj_g}{Energy content of food, in kJ/g}
#'  \item{fat_g}{Fat content of food per grams}
#'  \item{sat_fat_g}{Saturated fat content of food per grams}
#'  \item{CHO_g}{Carbohydrates content of food per grams}
#'  \item{sugars_g}{Sugars content of food per grams}
#'  \item{fibre_g}{Fibre content of food per grams}
#'  \item{protein_g}{Protein content of food per grams}
#'  \item{sodium_mg}{Sodium content of food per miligrams}
#'  \item{price}{Price of food per 100g}              
#' }
#' 
#' @source Elaborated by authors.
"foods"