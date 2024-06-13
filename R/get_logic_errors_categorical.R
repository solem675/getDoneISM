#' Find categorical to categorical errors in a survey dataframe
#'
#' @param ds dataframe;
#' @param logic_frame dataframe; list of survey logic checks. The template for it is produced with `get_logic_check_template()`
#' @param idvar character; name of unique observations IDs variable
#' @param enum_col character; (optional) name of a variable with enumerator's/interviewer's ID
#' @param date_col character; (optional) name of a variable, containing survey date
#' @param enum_com_col character; (optional) name of a variable containing enumerator's (interviewer's comment)
#' @param index_col character; (optional) name of column containing index of observation (for loops in xls forms)
#'
#' @return dataframe
#' @export
#'
#' @examples
get_logic_errors_categorical <- function(ds, logic_frame, idvar = "_uuid", enum_col="complementary_col", date_col="complementary_col", enum_com_col="complementary_col", index_col="complementary_col"){

  ds <- ds |>
    mutate(complementary_col = NA_character_)

  log_checks_issues <- data.frame(matrix(nrow = 0, ncol = 9))

  colnames(log_checks_issues) <- c(idvar, enum_col, date_col, enum_com_col, index_col, "issue", "comment", "variable_to_change", "correct_value")
  names(log_checks_issues) <- names(log_checks_issues)[!duplicated(names(log_checks_issues))]

  for (i in 1:nrow(logic_frame)){
    var <- logic_frame$target_var[i]
    crvar <- logic_frame$cross_var[i]

    val <- logic_frame$target_val[i]
    crval <- logic_frame$cross_val[i]

    logic_temp <- ds |>
      filter(!!sym(var) == paste0(val) & !!sym(crvar) == paste0(crval)) |>
      select(!!sym(idvar), !!sym(enum_col), !!sym(date_col), !!sym(enum_com_col), !!sym(index_col)) |>
      mutate(issue = paste0("Inconsistent answers: ", val, " in ", var, " and ", crval, " in ", crvar),
             comment = NA_character_,
             variable_to_change = NA_character_,
             correct_value = NA_character_)

    log_checks_issues <- rbind(log_checks_issues, logic_temp)
  }

  log_checks_issues <- log_checks_issues |>
    mutate(across(everything(), ~as.character(.))) |>
    select(-contains("complementary_col"))

  return(log_checks_issues)
}
