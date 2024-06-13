#' Extract open-ended "other" responses from a survey dataset
#'
#' @param ds dataframe;
#' @param idvar character; a name of unique observation ID (default "_uuid")
#' @param suffix character; suffix that differentiates a variable name, containing open-ended specification of other value from the variable, containing the general question
#' @param questions dataframe; "survey" sheet of xls-form
#' @param choices dataframe; "choices" sheet of xls-form
#' @param lang character; xls-label's language (if labels are used instead of names)
#'
#' @return dataframe
#' @export
#'
#' @examples

get_others_extracted <- function(ds, idvar = "_uuid", suffix = "_other", questions, choices, lang = "English"){


  # selecting variables with "others"
  others_ds <- ds |>
    select(!!sym(idvar), ends_with(paste0(suffix)) & is.character) |>
    select(where(~sum(!is.na(.x)) > 0)) |>
    pivot_longer(-!!sym(idvar), names_to = "variable", values_to = "other_value") |>
    filter(!is.na(other_value))
  # dragging an "organic" variable from variable name
  var_others <- others_ds |>
    select(variable) |>
    #mutate(variable_org = stringr::str_remove(variable, "_please_specify_other")) |>
    mutate(variable_org = stringr::str_remove(variable, paste0(suffix)))
  #matching variables names to a xls questionnaire
  var_others <-  var_others |>
    left_join(questions |>
                select(type, name), by = c("variable_org" = "name")) |>
    left_join(questions |>
                select(name, any_of(c(paste0("Label::", lang), paste0("label::", lang)))), by = c("variable_org" = "name"))

 suppressWarnings(var_others <- var_others |>
    separate(type, into = c("type", "list"), sep = " "))
  #matching variable names to xls choices
  lang_label <- names(choices)[grepl(paste0("::", lang), names(choices))]

  choices_merged <- choices |>
    mutate(name_label = paste0(name, " - ", !!sym(lang_label))) |>
    group_by(list_name) |>
    summarize(options = paste0(name_label, collapse = ", "))

  var_others <- var_others |>
    left_join(choices_merged, by = c("list" = "list_name"), relationship = "many-to-many")
  #compile an array for recoding
  others_main_ds <- others_ds |>
    mutate(recoded_into_var = NA, recoded_into_val = NA, other_column = NA, is_valid = NA, keep = NA) |>
    left_join(var_others, by = "variable", relationship = "many-to-many") |>
    group_by(variable) |>
    distinct(!!sym(idvar), .keep_all = T)

  others_main_ds <- others_main_ds[, c(1, 2, 12, 3, 4:11, 13)]

  return(others_main_ds)

}



