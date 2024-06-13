#' Find categorical to numeric errors in a survey dataframe
#'
#' @param ds dataframe;
#' @param logic_frame dataframe; list of survey logic checks. The template for it is produced with `get_logic_check_template()`
#' @param idvar character; name of unique observations IDs variable
#' @param enum_col character; (optional) name of a variable with enumerator's/interviewer's ID
#' @param date_col character; (optional) name of a variable, containing survey date
#' @param enum_com_col character; (optional) name of a variable containing enumerator's (interviewer's comment)
#' @param index_col character; (optional) name of column containing index of observation (for loops in xls forms)
#'
#' @import tidyverse
#'
#' @return dataframe
#' @export
#'
#' @examples
get_logic_errors_categorical_numeric  <- function(ds, logic_frame, idvar = "_uuid", enum_col="complementary_col", date_col="complementary_col", enum_com_col="complementary_col", index_col="complementary_col"){

  ds <- ds |> mutate(complementary_col = NA_character_)

  log_checks_issues_num <- data.frame(matrix(nrow = 0, ncol = 8))

  colnames(log_checks_issues_num) <- c(idvar, enum_col, date_col, enum_com_col, "issue", "comment", "variable_to_change", "correct_value")
  names(log_checks_issues_num) <- names(log_checks_issues_num)[!duplicated(names(log_checks_issues_num))]

  ds <- ds |>
    mutate(across(is.logical, as.numeric))

  for (i in 1:nrow(logic_frame)){
    var <- logic_frame$target_var[i]
    crvar <- logic_frame$cross_var[i]

    val <- logic_frame$target_val[i]
    crval <- logic_frame$cross_val[i]

    sign <- logic_frame$sign[i]

    `%.%` <- get(sign)

    logic_temp <- ds |>
      filter(!!sym(var) == paste0(val) & !!sym(crvar) %.% paste0(crval)) |>
      select(!!sym(idvar), !!sym(enum_col), !!sym(date_col), !!sym(enum_com_col), !!sym(index_col)) |>
      mutate(issue = paste0("Inconsistent answers: ", val, " in ", var, " and ", crval, " is ", sign, " ", crvar), comment = NA_character_)

    log_checks_issues_num <- rbind(log_checks_issues_num, logic_temp)
    log_checks_issues_num <- log_checks_issues_num |>
      select(-contains("complementary_col"))
  }

  log_checks_issues_num$variable_to_change <- NA
  log_checks_issues_num$correct_value <- NA

  return(log_checks_issues_num)
}
