#' Suffix removal
#' 
#' Removes one of two suffixes from column names
#' @param vector Vector of column names
#' @param suffix_1 First suffix to be removed.
#' @param suffix_2 Second suffix to be removed.
#' @return Vector of column names without suffixes.
#' @examples
#'nutrient_names <- remove_suffix(nutrient_names, '_grams','_mgrams')
#' 
#' @export
remove_suffix <- function(vector, suffix_1, suffix_2){
  for(i in 1:length(vector)){
    drop <- ifelse(grepl(suffix_1,vector[i]),
                   suffix_1,
                   ifelse(grepl(suffix_2,vector[i]),
                          suffix_2,
                          'not'))
    
    if(drop != 'not'){
      vector[i] <- unlist(strsplit(vector[i],drop,1))
    }
    
  }
  return(vector)
}