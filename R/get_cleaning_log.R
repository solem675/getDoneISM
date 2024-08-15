#' Cleaning log (value and variable change)
#'
#' @param raw dataframe; raw/original dataset
#' @param clean dataframe; clean/processed dataset
#' @param idvar character; variable name for unique observation IDs (e.g. "uuid")
#' @param moda logical; in "select multiple" substitutions of TRUE/FALSE to 0/1 is not tracked
#'
#' @import tidyverse
#' @importFrom purrr map_dfr
#'
#' @return list containing 3 dataframes;
#' \itemize{
#'  \item deletion - observations deleted from a clean dataset
#'  \item variable change - indication of variables, present in one of datasets only
#'  \item value cleaning - changes in variable's values between raw and clean dataset
#' }
#' @export
#'
#' @examples
get_cleaning_log <- function(raw, clean, idvar = "_uuid", moda = F){
  # adjusting variable names
  names(clean) <- make.unique(names(clean), sep = "_")
  names(raw) <- make.unique(names(raw), sep = "_")
  setdiff(names(clean), names(raw))

  # tracking variables addition/deletion
  n_raw <- names(raw)
  n_clean <- names(clean)
  in_raw <- n_raw[which(!(names(raw) %in% names(clean)))]
  in_clean <- n_clean[which(!(names(clean) %in% names(raw)))]
  d_in_raw <- as.data.frame(in_raw)
  d_in_raw$where <- rep("In RAW dataset, but not in CLEAN dataset", nrow(d_in_raw))
  d_in_clean <- as.data.frame(in_clean)
  d_in_clean$where <- rep("In CLEAN dataset, but not in RAW dataset", nrow(d_in_clean))
  names(in_raw) <- c("var_name", "where")
  names(in_clean) <- c("var_name", "where")
  diff_vars <- rbind(d_in_raw, d_in_clean)

  #adding missing variables to dataframes to allow for value change comparison
  i <- 1
  for(i in 1:length(in_raw)){
    if(length(in_raw) > 0){
      clean[, paste0(in_raw[i])] <- NA
    }
  }

  i <- 1
  for(i in 1:length(in_clean)){
    if(length(in_clean) > 0){
      raw[, paste0(in_clean[i])] <- NA
    }
  }

  # tracking deleted observations
  deleted <- raw |> anti_join(clean, by = idvar) |> select(paste0(idvar)) |> mutate(reason = "")

  # adjusting number of rows (for value change comparison)
  raw_dl <- raw |> filter(!!sym(idvar) %in% clean[, idvar])

  # value change function
  value_change <- function(tar_var, raw, clean, idvar = "uuid"){
    val_changes <- data.frame(key = character(), variable = character(), old_value = character(), new_value = character())
    i <- 1
    for(uuid in clean[,idvar]){

      if (mapply(identical, clean[which(clean[, idvar] == uuid), paste0(tar_var)], raw[which(raw[, idvar] == uuid), paste0(tar_var)]) == F){
        key <-  clean[which(clean[, idvar] == uuid), paste0(idvar)]
        variable <- tar_var
        old_value <- raw[which(raw[, idvar] == uuid), paste0(tar_var)]
        new_value <- clean[which(clean[, idvar] == uuid), paste0(tar_var)]
      } else {
        next
      }
      t_val_changes <- cbind(key, variable, old_value, new_value)
      val_changes <- rbind(val_changes, t_val_changes)
    }
    return(val_changes)
  }


  #applying value change function
  nam <- names(raw)
  log <- purrr::map_dfr(nam, value_change, raw_dl, clean, idvar)

  # remove SM type conversion for MoDA

  if(moda == F){
    log <- log |>
      filter(!(old_value == "FALSE" & new_value == "0"),
             !(old_value == "TRUE" & new_value == "1"))
  }

  change_log <- list("deletion" = deleted,
                     "variable change" = diff_vars,
                     "value cleaning" = log)

  return(change_log)
}

