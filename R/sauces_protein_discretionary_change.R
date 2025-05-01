#' Sauces, protein and discretionary food groups treatment
#'
#' Treats above said food name groups to the format used in the package.
#' @param group Food group column in dataframe.
#' @return Treated dataframe.
#' @examples 
#'df$food_group <- sapply(df$food_group, sauces_protein_discretionary_change)
#' @export
sauces_protein_discretionary_change <- function(group){
  ifelse(group == 'Sauces, dressings, spreads, sugars',
         'Sauces',
         ifelse(grepl('Protein',group),
                'Protein',
                ifelse(group == ' Discretionary foods',
                       'Discretionary foods',
                       ifelse(grepl('starchy', group,ignore.case = TRUE), 'Starchy vegetables',group))))
}