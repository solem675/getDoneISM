#' Get a template for creating custom logic check
#'
#' @return dataframe; logic check template
#' @export
#'
#' @examples

get_logic_check_template <- function(){
  cat_cat <- data.frame(target_var = c(NA),
                        target_val = c(NA),
                        cross_var = c(NA),
                        cross_val = c(NA))

  cat_num <- data.frame(target_var = c(NA),
                        target_val = c(NA),
                        cross_var = c(NA),
                        cross_val = c(NA),
                        sign = c(NA))
  template <- list("categorical_vs_categorical" = cat_cat,
                   "categorical_vs_numeric" = cat_num)
}

# get_logic_check_template <- function(idvar = "_uuid",
#                                      enum_col = NA,
#                                      date_col = NA,
#                                      enum_com_col = NA){
#   colnams <- c(idvar, enum_col, date_col, enum_com_col, "issue", "comment", "variable_to_change", "correct_value")
#
#   colnams <- colnams[!is.na(colnams)]
#
#   lct <- as.data.frame(matrix(nrow = 0, ncol = length(colnams)))
#
#   lct <- lct |> setNames(colnams)
#
#   return(lct)
# }


