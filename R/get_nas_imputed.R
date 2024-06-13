#' Impute NAs with a median in numeric variables
#'
#' @param ds dataframe
#' @param questions dataframe; "survey" sheet of xls-form
#' @param idvar character; name of unique observations IDs variable
#' @param labels logical; if dataframe's headers are xls-labels instead of names
#' @param lang character; label's language (if labels are used instead of names)
#' @param exclude character; vector of variables' names that should not be imputed
#'
#' @import tidyverse
#'
#' @return
#' @export
#'
#' @examples

get_nas_imputed <- function(ds, questions, idvar = "_uuid", labels = F, lang = "English", exclude = c()){
  if(labels == T){
    nam_lab <- names(questions)[grepl(paste0("abel::",lang), names(questions))]
  } else {
    nam_lab <- "name"
  }

  nums <- questions |>
    select(type, !!sym(nam_lab)) |>
    filter(type %in% c("integer", "decimal"))

  nums <- as.vector(nums[,2])
  nums <- setdiff(nums, exclude)


  ds <- ds |> mutate(across(nums, ~replace_na(., median(., na.rm = T))))

  return(ds)
}




