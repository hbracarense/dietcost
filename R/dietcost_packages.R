#' @importFrom readxl read_excel
#' @importFrom rlang UQ sym := hash is_empty
#' @importFrom dplyr group_by summarise filter left_join mutate case_when rename_with pull across select
#' @importFrom tidyselect all_of 
#' @importFrom stats complete.cases runif median sd na.omit qt qnorm
#' @importFrom xlsx write.xlsx
#' @importFrom utils read.csv write.csv tail head
#' @importFrom writexl write_xlsx

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

NULL