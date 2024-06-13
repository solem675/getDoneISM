#' Set of summary statistics for numeric variables
#'
#' @param ds dataframe
#' @param numeric_vars character; vector of numeric variables' names
#' @param ind_var character; name of independent variable
#' @param wide logical; "wide-formatted" data
#'
#' @return dataframe
#' @export
#'
#' @examples

get_numerics_summaries <- function(ds, numeric_vars, ind_var, wide = T){
  nam_ds <- names(ds)

  numericds <- as.vector(numeric_vars[numeric_vars %in% nam_ds])

  num_wtds <- ds |>
    select(all_of(numericds), !!sym(ind_var))

  num_wtds <- num_wtds |>
    mutate(across(setdiff(names(num_wtds), paste0(ind_var)), as.numeric))

  vars <- names(num_wtds)[!(names(num_wtds) %in% c(paste0(ind_var)))]

  num_mends <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ mean(., na.rm = T)) |>
    mutate(stat = "mean")

  num_mends_all <- num_wtds |>
    summarise_at(vars(vars), ~ mean(., na.rm = T)) |>
    mutate(stat = "mean")

  num_medds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ median(., na.rm = T)) |>
    mutate(stat = "median")

  num_medds_all <- num_wtds |>
    summarise_at(vars(vars), ~ median(.,  na.rm = T)) |>
    mutate(stat = "median")

  num_q1ds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ quantile(., probs=(0.25), na.rm = T)) |>
    mutate(stat = "q1")

  num_q1ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ quantile(., probs=(0.25),  na.rm = T)) |>
    mutate(stat = "q1")

  num_q3ds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ quantile(., probs=(0.75), na.rm = T)) |>
    mutate(stat = "q3")

  num_q3ds_all <- num_wtds |>
    summarise_at(vars(vars), ~ quantile(., probs=(0.75),  na.rm = T)) |>
    mutate(stat = "q3")

  num_minds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ min(., na.rm = T)) |>
    mutate(stat = "min")

  num_minds_all <- num_wtds |>
    summarise_at(vars(vars), ~ min(.,  na.rm = T)) |>
    mutate(stat = "min")

  num_maxds <- num_wtds |>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ max(., na.rm = T)) |>
    mutate(stat = "max")

  num_maxds_all <- num_wtds |>
    summarise_at(vars(vars), ~ max(.,  na.rm = T)) |>
    mutate(stat = "max")

  num_couds <- num_wtds|>
    group_by(!!sym(ind_var)) |>
    summarise_at(vars(vars), ~ sum(!is.na(.))) |>
    mutate(stat = "observations count")

  num_couds_all <- num_wtds|>
    summarise_at(vars(vars), ~sum(!is.na(.))) |>
    mutate(stat = "observations count")

  numerics_ds <- plyr::rbind.fill(num_minds, num_minds_all, num_q1ds, num_q1ds_all, num_mends, num_mends_all, num_medds, num_medds_all, num_q3ds, num_q3ds_all, num_maxds, num_maxds_all, num_couds, num_couds_all)

  numerics_ds <- numerics_ds |>
    mutate(!!sym(ind_var):= ifelse(is.na(!!sym(ind_var)), "Total", !!sym(ind_var))) |>
    mutate(colname = paste0(stat, "_", !!sym(ind_var))) |>
    select(colname, everything())

  if(wide == T){
    numerics_ds_t <- numerics_ds |>
      t() |>
      as.data.frame()

    names(numerics_ds_t) <- numerics_ds_t[1,]
    numerics_ds_t$variable <- rownames(numerics_ds_t)

    numerics_ds_t <- numerics_ds_t |>
      select(variable, everything())

    numerics_ds_t <- numerics_ds_t |>
      filter(!variable %in% c("colname", paste0(ind_var), "stat"))

    return(numerics_ds_t)
  } else{
    numerics_ds_long <- numerics_ds |>
      pivot_longer(cols = -c(colname, !!sym(ind_var), stat), names_to = "variable", values_to = "value") |>
      rename(unit = !!sym(ind_var))
    return(numerics_ds_long)
  }


}
