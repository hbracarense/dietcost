#' Food groups dataset example
#' 
#' A set of data containing commonly available food groups based on a Brazilian typical diet.
#' 
#' @format ##`food_groups`
#' A dataframe with 12 rows and 74 columns:
#' \describe{
#'  \item{food_group}{Food group, i.e. 'Fruit' or 'Vegetable'}
#'  \item{food_group_id}{Numerical code for dood group}
#'  \item{man_min_g_C}{Weekly minimal current diet intake for males, in grams}
#'  \item{man_max_g_C}{Weekly maximal current diet intake for males, in grams}
#'  \item{man_target_g_C}{Weekly target current diet intake for males, in grams}
#'  \item{man_min_serve_C}{Weekly minimal current diet intake for males, in serves}
#'  \item{man_max_serve_C}{Weekly maximal current diet intake for males, in serves}
#'  \item{man_target_serve_C}{Weekly target current diet intake for males, in serves}
#'  \item{woman_min_g_C}{Weekly minimal current diet intake for females, in grams}
#'  \item{woman_max_g_C}{Weekly maximal current diet intake for females, in grams}
#'  \item{woman_target_g_C}{Weekly target current diet intake for females, in grams}
#'  \item{woman_min_serve_C}{Weekly minimal current diet intake for females, in serves}
#'  \item{woman_max_serve_C}{Weekly maximal current diet intake for females, in serves}
#'  \item{woman_target_serve_C}{Weekly target current diet intake for females, in serves}
#'  \item{boy_min_g_C}{Weekly minimal current diet intake for boys, in grams}
#'  \item{boy_max_g_C}{Weekly maximal current diet intake for boys, in grams}
#'  \item{boy_target_g_C}{Weekly target current diet intake for boys, in grams}
#'  \item{boy_min_serve_C}{Weekly minimal current diet intake for boys, in serves}
#'  \item{boy_max_serve_C}{Weekly maximal current diet intake for boys, in serves}
#'  \item{boy_target_serve_C}{Weekly target current diet intake for boys, in serves}
#'  \item{girl_min_g_C}{Weekly minimal current diet intake for girls, in grams}
#'  \item{girl_max_g_C}{Weekly maximal current diet intake for girls, in grams}
#'  \item{girl_target_g_C}{Weekly target current diet intake for girls, in grams}
#'  \item{girl_min_serve_C}{Weekly minimal current diet intake for girls, in serves}
#'  \item{girl_max_serve_C}{Weekly maximal current diet intake for girls, in serves}
#'  \item{girl_target_serve_C}{Weekly target current diet intake for girls, in serves}
#'  \item{man_min_g_PF}{Weekly minimal EAT-Lancet diet intake for males, in grams}
#'  \item{man_max_g_PF}{Weekly maximal EAT-Lancet diet intake for males, in grams}
#'  \item{man_target_g_PF}{Weekly target EAT-Lancet diet intake for males, in grams}
#'  \item{man_min_serve_PF}{Weekly minimal EAT-Lancet diet intake for males, in serves}
#'  \item{man_max_serve_PF}{Weekly maximal EAT-Lancet diet intake for males, in serves}
#'  \item{man_target_serve_PF}{Weekly target EAT-Lancet diet intake for males, in serves}
#'  \item{woman_min_g_PF}{Weekly minimal EAT-Lancet diet intake for females, in grams}
#'  \item{woman_max_g_PF}{Weekly maximal EAT-Lancet diet intake for females, in grams}
#'  \item{woman_target_g_PF}{Weekly target EAT-Lancet diet intake for females, in grams}
#'  \item{woman_min_serve_PF}{Weekly minimal EAT-Lancet diet intake for females, in serves}
#'  \item{woman_max_serve_PF}{Weekly maximal EAT-Lancet diet intake for females, in serves}
#'  \item{woman_target_serve_PF}{Weekly target EAT-Lancet diet intake for females, in serves}
#'  \item{boy_min_g_PF}{Weekly minimal EAT-Lancet diet intake for boys, in grams}
#'  \item{boy_max_g_PF}{Weekly maximal EAT-Lancet diet intake for boys, in grams}
#'  \item{boy_target_g_PF}{Weekly target EAT-Lancet diet intake for boys, in grams}
#'  \item{boy_min_serve_PF}{Weekly minimal EAT-Lancet diet intake for boys, in serves}
#'  \item{boy_max_serve_PF}{Weekly maximal EAT-Lancet diet intake for boys, in serves}
#'  \item{boy_target_serve_PF}{Weekly target EAT-Lancet diet intake for boys, in serves}
#'  \item{girl_min_g_PF}{Weekly minimal EAT-Lancet diet intake for girls, in grams}
#'  \item{girl_max_g_PF}{Weekly maximal EAT-Lancet diet intake for girls, in grams}
#'  \item{girl_target_g_PF}{Weekly target EAT-Lancet diet intake for girls, in grams}
#'  \item{girl_min_serve_PF}{Weekly minimal EAT-Lancet diet intake for girls, in serves}
#'  \item{girl_max_serve_PF}{Weekly maximal EAT-Lancet diet intake for girls, in serves}
#'  \item{girl_target_serve_PF}{Weekly target EAT-Lancet diet intake for girls, in serves}
#'  \item{man_min_g_H}{Weekly minimal healthy diet intake for males, in grams}
#'  \item{man_max_g_H}{Weekly maximal healthy diet intake for males, in grams}
#'  \item{man_target_g_H}{Weekly target healthy diet intake for males, in grams}
#'  \item{man_min_serve_H}{Weekly minimal healthy diet intake for males, in serves}
#'  \item{man_max_serve_H}{Weekly maximal healthy diet intake for males, in serves}
#'  \item{man_target_serve_H}{Weekly target healthy diet intake for males, in serves}
#'  \item{woman_min_g_H}{Weekly minimal healthy diet intake for females, in grams}
#'  \item{woman_max_g_H}{Weekly maximal healthy diet intake for females, in grams}
#'  \item{woman_target_g_H}{Weekly target healthy diet intake for females, in grams}
#'  \item{woman_min_serve_H}{Weekly minimal healthy diet intake for females, in serves}
#'  \item{woman_max_serve_H}{Weekly maximal healthy diet intake for females, in serves}
#'  \item{woman_target_serve_H}{Weekly target healthy diet intake for females, in serves}
#'  \item{boy_min_g_H}{Weekly minimal healthy diet intake for boys, in grams}
#'  \item{boy_max_g_H}{Weekly maximal healthy diet intake for boys, in grams}
#'  \item{boy_target_g_H}{Weekly target healthy diet intake for boys, in grams}
#'  \item{boy_min_serve_H}{Weekly minimal healthy diet intake for boys, in serves}
#'  \item{boy_max_serve_H}{Weekly maximal healthy diet intake for boys, in serves}
#'  \item{boy_target_serve_H}{Weekly target healthy diet intake for boys, in serves}
#'  \item{girl_min_g_H}{Weekly minimal healthy diet intake for girls, in grams}
#'  \item{girl_max_g_H}{Weekly maximal healthy diet intake for girls, in grams}
#'  \item{girl_target_g_H}{Weekly target healthy diet intake for girls, in grams}
#'  \item{girl_min_serve_H}{Weekly minimal healthy diet intake for girls, in serves}
#'  \item{girl_max_serve_H}{Weekly maximal healthy diet intake for girls, in serves}
#'  \item{girl_target_serve_H}{Weekly target healthy diet intake for girls, in serves}            
#' }
#' 
#' @source Elaborated by authors.