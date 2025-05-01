#' Permitted individuals check
#'
#' Checks if logged individuals are one or all of the following: man, woman, boy or girl.
#' @param df Variable.
#' @examples 
#' permitted_individuals(data.frame(individual = person))
#' @export
permitted_individuals <- function(df){
  possible_individuals <- c('man', 'woman', 'boy','girl')
  check <- df %>% filter(!(individual %in% possible_individuals)) %>% pull(individual)
  if(length(check)>0){
    for(i in 1:length(check)){
      print(paste("Individual",check[i],"isn't allowed."))
    }
    stop("Check your data! The only possible individuals in standard table mode are 'man', 'woman', 'boy' and 'girl'.")
  }
}