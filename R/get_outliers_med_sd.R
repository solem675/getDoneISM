#' Exptract values beyond menian +- 3sd
#'
#' @param ds dataframe
#' @param questions dataframe; "survey" sheet of xls-form
#' @param idvar character; a name of unique observation ID (default "_uuid")
#' @param labels logical; if dataframe's headers are xls-labels instead of names
#' @param lang character; xls-label's language (if labels are used instead of names)
#' @param exclude character; variable names to be excluded from imputation
#' @param lower logical; not only +3sd but -3sd outliers are tracked
#'
#' @import tidyverse
#'
#' @return
#' @export
#'
#' @examples

get_outliers_med_sd <- function(ds, questions, idvar = "_uuid", labels = F, lang = "English", exclude = c(), lower = F){

  if(labels == T){
    nam_lab <- names(questions)[grepl(paste0("abel::",lang), names(questions))]
  } else {
    nam_lab <- "name"
  }

  nums <- questions |>
    select(type, !!sym(nam_lab)) |>
    filter(type %in% c("integer", "decimal"))

  nums <- as.vector(nums[,2])

  nums <- setdiff(nums, exclude)

  ds <- ds |>
    mutate(across(all_of(nums), as.numeric))

  i <- 1
  outliers <- data.frame()
  for(num in nums){
    #for(i in 1:nrow(ds)){
    outlier <- ds |> select(all_of(idvar), !!sym(num)) |>
      filter(!!sym(num) > 0) |>
      mutate(ifoutlier_upper = ifelse(!!sym(num) > median(!!sym(num), na.rm = T)+3*sd(!!sym(num)), "> median+3sd", "no")) |>
      mutate(median = median(!!sym(num), na.rm = T),
             sd = sd(!!sym(num), na.rm = T),
             q99 = quantile(!!sym(num), 0.99)) |>
      filter(ifoutlier_upper == "> median+3sd") |>
      mutate(variable = paste0(num)) |>
      setNames(c("idvar", "value", "ifoutlier", "median_value", "sd", "q_v", "variable_name"))
    outliers <- rbind(outliers, outlier)
    #}
  }

  if(lower == T){
    i <- 1
    for(num in nums){
      #for(i in 1:nrow(ds)){
      outlier <- ds |> select(idvar, !!sym(num)) |>
        filter(!!sym(num) > 0) |>
        mutate(ifoutlier_lower = ifelse(!!sym(num) < median(!!sym(num), na.rm = T)-3*sd(!!sym(num)), "< median-3sd", "no")) |>
        mutate(median = median(!!sym(num), na.rm = T),
               sd = sd(!!sym(num), na.rm = T),
               q2 = quantile(!!sym(num), 0.01)) |>
        filter(ifoutlier_lower == "< median-3sd") |>
        mutate(variable = paste0(num)) |>
        setNames(c("idvar", "value", "ifoutlier", "median_value", "sd", "q_v", "variable_name"))
      outliers <- rbind(outliers, outlier)
      #}
    }
  }
  outliers <- outliers |>
    mutate(keep = NA,
           action = NA)
  return(outliers)

}
