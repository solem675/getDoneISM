#' Set of weighted crosstabulations from a dataset
#'
#' @param ds dataframe;
#' @param ind_var character; name of an independent variable
#' @param dep_vars character vector; names of dependent variables (can be generated with `get_names_for_crosstabs()`)
#' @param weights character; name of the weighting variable
#' @param wide logical; "wide-formatted" data
#'
#' @return dataframe; a set of crosstabulations
#' @export
#'
#' @examples
get_weighted_crosstabs <- function(ds, ind_var, dep_vars, weights,  wide = T){
  var_group <- function(di) {
    ttab <- ds |>
      dplyr::group_by(!!sym(ind_var), !!sym(di)) |>
      filter(!is.na(!!sym(di))) |>
      summarize(n = n(), wtd_n = sum(!!sym(weights)),  .groups = "drop_last") |>
      mutate(base = sum(n), wtd_prop = wtd_n/sum(wtd_n), variable = paste0(di))
    names(ttab) <- c("unit", "var", "n", "weighted_n", "base", "wtd_prop", "indicator")
    ttab$var <-  as.character(ttab$var)
    return(ttab)
  }
  var_overall <- function(di) {
    ttab <- ds |>
      dplyr::group_by(!!sym(di)) |>
      filter(!is.na(!!sym(di))) |>
      summarize(n = n(), wtd_n = sum(!!sym(weights)), .groups = "drop_last") |>
      mutate(base = sum(n), wtd_prop = wtd_n/sum(wtd_n), variable = paste0(di))
    names(ttab) <- c("var", "n", "weighted_n", "base", "wtd_prop", "indicator")
    ttab$var <-  as.character(ttab$var)
    return(ttab)
  }
  unit_list <- purrr::map(dep_vars, var_group)
  overall_list <- purrr::map(dep_vars, var_overall)

  unit_df <- as.data.frame(do.call(rbind, unit_list))
  overall_df <- as.data.frame(do.call(rbind, overall_list))
  overall_df$unit <- "Overall"
  long_df <- bind_rows(unit_df, overall_df)
  long_df <- long_df |> select(-weighted_n)

  if (wide == T) {
    result_df <- long_df |> select(!base) |> pivot_wider(names_from = c("unit"), values_from = c(wtd_prop, n))
  } else {
    result_df <- long_df
  }
  result_df <- result_df |> select(indicator, everything())
  return(result_df)
}
