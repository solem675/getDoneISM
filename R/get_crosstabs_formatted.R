
#' Formatting a set of crosstabulations
#'
#' @param crosstabs dataframe; a set of crosstabulations generated with `get_crosstabs()` or  `get_weighted_crosstabs()`
#' @param questions dataframe; the "survey" sheet from an xls-form
#' @param choices dataframe; the "choices" sheet from an xls-form
#' @param sep character; a separator of variable name and choice name for "select_multiple" sets of variables
#' @param labels logical; if a dataframe uses xls value labels instead of value names
#' @param lang character; language of labels from xls-form
#'
#' @import tidyverse
#'
#' @return dataframe; a set of crosstabulations with variable and value names
#' @export
#'
#' @examples
get_crosstabs_formatted <- function(crosstabs, questions, choices, sep = "/", labels = FALSE, lang = "English"){

  sep1 <- ifelse(sep == ".", "\\.", sep)

  cr_tabs1 <- crosstabs |>
    filter(!(str_detect(indicator, sep) & var == "0")) |>
    separate(indicator, into = c("indicator", "option"), sep = sep1)

  cr_tabs1$option <- coalesce(cr_tabs1$option, cr_tabs1$var)

  # if it has variable labels - adding questions is impossible because it grabs data from name
  # if it has vale labels only - select one will be labelled initially and select multiple - no
  # for select multiple labelling will still be needed
  if(labels == FALSE){

    language_label <- names(questions)[grepl(paste0("abel::",lang), names(questions))]

    questions <- questions |>
      separate(type, into = c("type", "list_name"), sep = " ")

    quest_cho <- questions |>
      left_join(
        choices |> select(list_name, name, !!sym(language_label)),
        by = "list_name", suffix = c("", "_ch")) #many to many here

    quest_cho <- quest_cho |>
      mutate(name_ch = as.character(name_ch))

    cr_tabs1 <- cr_tabs1 |>
      left_join(
        quest_cho |>
          select(name, list_name, !!sym(paste0(language_label)), relevant, type),
        by = c("indicator" = "name"))

    cr_tabs1 <- cr_tabs1 |>
      left_join(
        quest_cho |>
          select(name, name_ch, !!sym(paste0(language_label, "_ch"))),
        by = c("indicator" = "name", "option" = "name_ch"))

  cr_tabs1$choice_label <- coalesce(cr_tabs1[, paste0(language_label, "_ch")], cr_tabs1[, "option"])
  }
  supressWarnings(cr_tabs1 <- cr_tabs1 |> unnest())
  cr_tabs1 <- cr_tabs1 |> distinct()
    return(cr_tabs1)
}



