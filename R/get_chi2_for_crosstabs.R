#' CHi-squared statis for crosstabulations
#'
#' @param tables dataframe/tibble; a set of crosstabulations created with `get_crosstabs()` or `get_weighted_crosstabs()`
#' @param strict logical; If true, tables with <5 observations in a cell, get marked as "not enough observations" if False, number of observations <30 per table will produce this mark
#' @param prefix character; prefix of observation count columns in a set of tables; "n_"
#'
#' @import tidyverse
#'
#' @return dataframe; set of tables with columns (see `chisq.test()` for statistical test details)
#' \itemize{
#'  \item chisq_stat - chi-squared statistics;
#'  \item chisq_pval - pvalue for chi-squared;
#'  \item hyp - variables' independence;
#'  \item t_assumption_n - sufficiency of observations for test validity;
#'  }
#' @export
#'
#' @examples

get_chi2_for_crosstabs <- function(tables, strict = F, prefix = "n_"){
  tbl <- tables |>
    #filter(indicator != "A_3_respondent_hohh") |>
    select(indicator, var, starts_with(prefix), -c(!!sym(paste0(prefix, "Overall")))) |>
    mutate(across(starts_with(prefix), ~replace_na(.x, 0))) |>
    #mutate(across(everything(), ~replace(., . == 0, 1))) |>
    group_by(indicator) |>
    mutate(var  = as.factor(var)) |>
    summarise(chi_sq = list(chisq.test(across(starts_with(prefix)))), base = sum(across(starts_with(prefix))), min_n = min(across(starts_with(prefix)))) |>
    rowwise() |>
    mutate(chiSq_stat = chi_sq$statistic,
           chisq_pval = chi_sq$p.value,
           hyp = ifelse(chisq_pval <= 0.05, "h1 - not independent", "h0 - independent"))

  table_chi <- left_join(tables, tbl, by = c("indicator")) |>
    mutate(t_assumption_n = case_when(
      strict == T & min_n >= 5 ~ "enough observations ncell > 5",
      strict == T & min_n <= 5 ~ "not enough observations ncell < 5",
      strict == F & base >= 30 ~ "enough observations n > 30",
      strict == F & base <= 30 ~ "not enough observations n < 30",
      TRUE ~ NA_character_
    )) |>
    select(-chi_sq)
  return(table_chi)
}
