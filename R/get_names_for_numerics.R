#' Get a vector of names of numeric variables
#'
#' @param questions dataframe; "survey" sheet of xls-form
#' @param labels logical; if dataframe's headers are xls-labels instead of names
#' @param lang character; label's language (if labels are used instead of names)
#'
#' @import tidyverse
#'
#' @return character vector
#' @export
#'
#' @examples
get_names_for_numerics <- function(questions, labels=F, lang = "English"){

  if(labels == T){
    nam_lab <- names(questions)[grepl(paste0("abel::",lang), names(questions))]
  } else {
    nam_lab <- "name"
  }


  nums <- questions |> filter(str_detect(type, "integer|decimal")) |>
    select(!!sym(nam_lab)) |>
    as.vector() |>
    unlist()

  return(nums)
}
