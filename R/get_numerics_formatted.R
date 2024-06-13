#' Add xls labels to the set of numeric summaries
#'
#' @param numerics_tab dataframe; set of summary statistics obtained with `get_numerics_summaries()` or `get_weighted_numerrics_summaries()`
#' @param questions dataframe; "survey" sheet of xls-form
#' @param lang character; xls-label's language (if labels are used instead of names)
#'
#' @return dataframe
#' @export
#'
#' @examples

get_numerics_formatted <- function(numerics_tab, questions, lang = "English"){

  language_label <- names(questions)[grepl(paste0("abel::",lang), names(questions))]

  frmt_tab <- numerics_tab |>
    left_join(questions |> select(name, !!sym(language_label)), by = c("variable" = "name")) |>
    rename(indicator = !!sym(language_label))
    #filter(is.finite(value))

  return(frmt_tab)
  }

