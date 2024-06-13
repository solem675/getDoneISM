#' Get a vector with dependent variables' names for crosstabulations
#'
#' @param ds dataframe;
#' @param questions dataframe; "survey" sheet of xls-form
#' @param ind_var character; name of variable containing unique observation IDs
#' @param labels logical; if dataframe's headers are xls-labels instead of names
#' @param lang character; label's language (if labels are used instead of names)
#' @param exclude character; vector of variables' names that should be excluded from the list of dependent variables
#' @param tech character; prefix for technical variables ("_" by default)
#'
#' @import tidyverse
#'
#' @return character vector
#' @export
#'
#' @examples
get_names_for_crosstabs <-  function(ds, questions, ind_var, labels = F, lang = "English", exclude = c(), tech = "_"){

  tech_defaults <- c("start", "today", "deviceid", "end", "instanceID",
                     names(ds)[str_starts(names(ds), tech)])

  if(labels == T){
    nam_lab <- names(questions)[grepl(paste0("abel::",lang), names(questions))]
  } else {
    nam_lab <- "name"
  }

  suppressWarnings(questions_non_quan <- questions |>
    separate(type, into = "type", sep = " ") |>
    filter(type != "select_one") |>
    select(!!sym(nam_lab)) |>
    as.vector() |>
    unlist())



  vars_to_crosstab <- setdiff(names(ds),
                              c(questions_non_quan, tech_defaults, exclude, ind_var))

  return(vars_to_crosstab)
}

