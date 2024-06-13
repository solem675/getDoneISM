#' One-sample t-test for numeric variables
#'
#' @param num_vars character; vector of names of numeric variables to be tested (can be generated with `get_names_for_numerics()`)
#' @param group character; name of an inependent variable
#' @param ds dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples

get_t_mean_groups <- function(num_vars, group, ds){
  results <- data.frame()

  for(var in num_vars){
      mean_var <- mean(ds[, var], na.rm = T)
    res <- ds |>
      select(!!sym(var), !!sym(group)) |>
      filter(!is.na(!!sym(var))) |>
      mutate("{group}" := as.character(!!sym(group))) |>
      group_by(!!sym(group)) |>
      filter(n() > 1) |>
      filter(n_distinct(!!sym(var)) > 1)

    if (nrow(res >0)){

      res <-  res |> summarise(ttest = list(broom::tidy(t.test(!!sym(var), mu = mean_var, na.rm = T))),
                n = n(),
                mean = mean(!!sym(var), na.remove = T),
                sd = sd(!!sym(var)), median = median(!!sym(var), na.rm = T),
                min = min(!!sym(var), na.rm = T),
                max = max(!!sym(var), na.rm = T)) |>
        unnest(cols = ttest) |>
        mutate(ref_mean = mean_var,
               hyp = ifelse(p.value < 0.05, "h1 - significant diff", "h0 - not significant diff"),
               var_name = var)

      results <- rbind(results, res)
    }



  }

  results <- results |>
    select(var_name, everything())
  return(results)
}
