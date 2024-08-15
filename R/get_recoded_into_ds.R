#' Return recoded "other" responses to a dataset
#'
#' @param others_ds_done dataframe; manually reviewed and recoded set of "other" responses
#' @param ds dataframe;
#' @param questions dataframe; "survey" sheet of xls-form
#' @param idvar character; a name of unique observation ID (default "_uuid")
#' @param sep character; separator used in "select multiple" questions
#'
#' @return dataframe
#' @export
#'
#' @examples

get_recoded_into_ds <- function(others_ds_done, ds, questions, idvar = "_uuid", sep = "."){
  # deselect ones that have to be kept
  others_ds_done <- others_ds_done |>
    filter(is.na(keep))

  # deselect ones that are not recoded
  others_ds_done <- others_ds_done |>
    filter(!is.na(recoded_into_var))

  nam_ds <- names(ds)
  nam_other <- others_ds_done$variable
  id_other <- others_ds_done[, idvar]
  nam_ds <- setdiff(nam_ds, idvar)

  ds_rec <- ds #|>
    #mutate(across(contains(sep), as.character))
  #adding new variables (created from the prevalence of "others")
  new_vars <- setdiff(others_ds_done$recoded_into_var, names(ds_rec))
  new_vars <- new_vars[!is.na(new_vars)]
  ds_new_vars <- as.data.frame(matrix(nrow = nrow(ds_rec), ncol = length(new_vars)))
  names(ds_new_vars) <- new_vars
  ds_rec <- cbind(ds_rec, ds_new_vars)


  # replace selections
  #vi = nam_other[1] # - DEBUGGING
  for(vi in nam_other){ #for each "other" variable
    id_other_i <- others_ds_done[others_ds_done$variable == vi, idvar]

    for(id in id_other_i){ #for each id
      into_var_ij <- others_ds_done$recoded_into_var[which(others_ds_done$variable == vi & others_ds_done == id)]

      into_val_ij <-  others_ds_done$recoded_into_val[which(others_ds_done$variable == vi & others_ds_done[, idvar] == id)]
      other_col_var_ij <- others_ds_done$other_column[which(others_ds_done$variable == vi & others_ds_done[, idvar] == id)]
      j <- 1
      for(j in 1:length(into_var_ij)){
        ds_rec[ which(ds_rec[,idvar] == id), vi] <- NA
        ds_rec[ which(ds_rec[,idvar] == id), into_var_ij[j]] <- ifelse(into_val_ij[j] == "1", as.numeric(into_val_ij[j]), into_val_ij[j])
        if(others_ds_done$type[which(others_ds_done$variable == vi & others_ds_done[,idvar] == id)[1]] == "select_multiple" & !is.na(other_col_var_ij[j])){
          ds_rec[ which(ds_rec[,idvar] == id), other_col_var_ij[j]] <- 0

        }
      }
    }

  }

  # mutate extra 0s - defining variables

  ds_rec <- ds_rec |> mutate(across(contains(sep), as.logical))

  ds_rec <- ds_rec |> mutate(across(contains(sep), as.numeric))

  questions_sm <- questions |> filter(str_detect(type, "select_multiple")) |> select(name)
  questions_sm <- unlist(as.vector(questions_sm))

  questions_sm_main <- questions_sm[questions_sm %in% names(ds_rec)]
  if (length(questions_sm_main) == 0){
    ds_rec[,questions_sm] <- NA

    questions_sm_main <- questions_sm[questions_sm %in% names(ds_rec)]
  }
  ds_rec <- ds_rec |> mutate(across(is.logical, as.numeric))
  ds_rec <- ds_rec |> mutate(across(starts_with(paste0(questions_sm_main, sep)), as.numeric))
  n_mds <- names(ds_rec)

  # mutate extra 0s - looping across a dataframe

  i <- 1
  r <- 1
  for(i in 1: length(questions_sm_main)){
    nam_sm_main <- n_mds[grepl(paste0(questions_sm_main[i], sep), n_mds)]
    for(r in 1: nrow(ds_rec)){
      #r <- 29
      arr.ind <- which(ds_rec[r,nam_sm_main] == 1, arr.ind = T)
      ds_rec[r, paste0(questions_sm_main[i])] <- paste0(as.vector(colnames(ds_rec[nam_sm_main])[ arr.ind[, 2] ]), collapse = " ")
      ds_rec[r, paste0(questions_sm_main[i])] <- str_remove_all(ds_rec[r, paste0(questions_sm_main[i])], paste0(questions_sm_main[i], sep))
      ds_rec[r, paste0(questions_sm_main[i])] <- str_trim(ds_rec[r, paste0(questions_sm_main[i])])
      ds_rec[r, paste0(questions_sm_main[i])] <- ifelse(ds_rec[r, paste0(questions_sm_main[i])]=="", NA, ds_rec[r, paste0(questions_sm_main[i])])
      if(is.na(ds_rec[r,paste0(questions_sm_main[i])])){
        ds_rec[r,nam_sm_main] <- NA}
    }
  }

  for(var in new_vars){
    aggr_var <- sub("/.*", "", var)
    ds_rec <- ds_rec |>
      mutate(!!sym(var):= case_when(
        !is.na(!!sym(aggr_var)) & !is.na(!!sym(var)) ~ !!sym(var),
        !is.na(!!sym(aggr_var)) & is.na(!!sym(var)) ~ 0,
        is.na(!!sym(aggr_var)) ~ NA_real_
      ))
  }

  return(ds_rec)
}


