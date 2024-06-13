#' Remove choice labels from "select multiple" questions
#'
#' @param ds dataframe;
#' @param sep  character; separator used in "select multiple" questions
#' @param label_true character; label, used for "true"
#' @param label_false character; label used for "false"
#'
#' @return dataframe;
#' @export
#'
#' @examples

get_sm_delabelled <- function(ds, sep, label_true, label_false){
  ds <- ds |>
    mutate(across(contains(sep), .fns = ~case_when(
      . == label_true ~ "1",
      . == label_false ~ "0"

    )))

  ds <-  ds |>
    mutate(across(contains(sep), ~as.numeric(.)))
}

# ds = dataset3
# sep = "."
# label_true = "yes"
# label_false = "no"
