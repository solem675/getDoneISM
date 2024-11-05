#' Set of weighted summary statistics for numeric variables
#'
#' @param ds dataframe
#' @param numeric_vars character; vector of numeric variables' names
#' @param ind_var character; name of independent variable
#' @param weights character; name of the weighting variable
#'
#' @return dataframe
#' @export
#'
#' @examples

get_weighted_numerics <- function(ds, numeric_vars, ind_var, weights){
  nam_ds <- names(ds)

  numericds <- as.vector(numeric_vars[numeric_vars %in% nam_ds])

  num_wtds <- ds |>
    select(all_of(numericds), !!sym(weights), !!sym(ind_var))

  num_wtds <- num_wtds |>
    mutate(across(setdiff(names(num_wtds), paste0(ind_var)), as.numeric))

  vars <- names(num_wtds)[!(names(num_wtds) %in% c(paste0(weights), paste0(ind_var)))]

  num_wt_mends <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~weighted.mean(.,  w = !!sym(weights), na.rm = T)) |>
    mutate(stat = "weighted mean")

  num_wt_mends_all <- num_wtds |>
    summarise_at(vars(vars), ~weighted.mean(.,  w = !!sym(weights), na.rm = T)) |>
    mutate(stat = "weighted mean")

  num_wt_medds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ergm::wtd.median(.,  weight = !!sym(weights), na.rm = T)) |>
    mutate(stat = "weighted median")

  num_wt_medds_all <- num_wtds |>
    summarise_at(vars(vars), ~ergm::wtd.median(.,  weight = !!sym(weights), na.rm = T)) |>
    mutate(stat = "weighted median")

  num_q1ds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.25), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q1")

  num_q1ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.25), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q1")

  num_q1ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.25), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q1")

  num_q3ds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.75), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q3")

  num_q3ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.75), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q3")

  num_q3ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ ifelse(all(is.na(.)), NA_real_, Hmisc::wtd.quantile(., probs = c(0.75), weight = !!sym(weights), na.rm = T))) |>
    mutate(stat = "q3")

  num_minds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~min(., na.rm = T)) |>
    mutate(stat = "min")

  num_minds_all <- num_wtds |>
    summarise_at(vars(vars), ~min(.,  na.rm = T)) |>
    mutate(stat = "min")

  num_maxds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~max(., na.rm = T)) |>
    mutate(stat = "max")

  num_maxds_all <- num_wtds |>
    summarise_at(vars(vars), ~max(.,  na.rm = T)) |>
    mutate(stat = "max")

  num_wt_couds <- num_wtds|>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~sum(!is.na(.))) |>
    mutate(stat = "observations count")

  num_wt_couds_all <- num_wtds|>
    summarise_at(vars(vars), ~sum(!is.na(.))) |>
    mutate(stat = "observations count")

  numerics_wtds <- plyr::rbind.fill(num_wt_minds, num_wt_minds_all,
                                    num_wt_q1ds, num_wt_q1ds_all,
                                    num_wt_mends, num_wt_mends_all,
                                    num_wt_medds, num_wt_medds_all,
                                    num_wt_q3ds, num_wt_q3ds_all,
                                    num_wt_maxds, num_wt_maxds_all,
                                    num_wt_couds, num_wt_couds_all)

  numerics_wtds <- numerics_wtds |>
    mutate(!!sym(ind_var):= ifelse(is.na(!!sym(ind_var)), "Total", !!sym(ind_var))) |>
    mutate(colname = paste0(stat, "_", !!sym(ind_var))) |>
    select(colname, everything())

  if(wide == T){
    numerics_wtds_t <- numerics_wtds |>
      t() |>
      as.data.frame()

    names(numerics_wtds_t) <- numerics_wtds_t[1,]
    numerics_wtds_t$variable <- rownames(numerics_wtds_t)

    numerics_wtds_t <- numerics_wtds_t |>
      select(variable, everything())

    numerics_wtds_t <- numerics_wtds_t |>
      filter(!variable %in% c("colname", paste0(ind_var), "stat"))

    return(numerics_wtds_t)
  } else{
    numerics_ds_long <- numerics_ds |>
      pivot_longer(cols = -c(colname, !!sym(ind_var), stat), names_to = "variable", values_to = "value")
    return(numerics_ds_long)

    return(numerics_wtds)
  }


}

