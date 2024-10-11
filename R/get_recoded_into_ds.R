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
  others_ds_done <- others_ds_done |>
    filter(is.na(keep))

  # deselect ones that are not recoded
  others_invalid <- others_ds_done |> filter(is_valid == F)

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
  # for(vi in nam_other){ #for each "other" variable
  #   id_other_i <- others_ds_done[others_ds_done$variable == vi, idvar]
  #
  #   for(id in id_other_i){ #for each id
  #     into_var_ij <- others_ds_done$recoded_into_var[which(others_ds_done$variable == vi & others_ds_done == id)]
  #
  #     into_val_ij <-  others_ds_done$recoded_into_val[which(others_ds_done$variable == vi & others_ds_done[, idvar] == id)]
  #     other_col_var_ij <- others_ds_done$other_column[which(others_ds_done$variable == vi & others_ds_done[, idvar] == id)]
  #     j <- 1
  #     for(j in 1:length(into_var_ij)){
  #       ds_rec[ which(ds_rec[,idvar] == id), vi] <- NA
  #       ds_rec[ which(ds_rec[,idvar] == id), into_var_ij[j]] <- ifelse(into_val_ij[j] == "1", as.numeric(into_val_ij[j]), into_val_ij[j])
  #       if(others_ds_done$type[which(others_ds_done$variable == vi & others_ds_done[,idvar] == id)[1]] == "select_multiple" & !is.na(other_col_var_ij[j])){
  #         ds_rec[ which(ds_rec[,idvar] == id), other_col_var_ij[j]] <- 0
  #
  #       }
  #     }
  #   }
  #
  # }
  ## for each other variable select ids, in each id select the cell into (var, val) and column with others
  ## for these cells turn text to na,

  ##### rewrite ##################################################################

  ds_rec1 <- ds_rec
  # nam_other_r <- unique(others_ds_done$recoded_into_var)
  #
  # # replace selections
  # vi = nam_other_r[136] # - DEBUGGING
  # for(vi in nam_other_r){ #for each "other" variable
  #   id_other_i <- others_ds_done[others_ds_done$recoded_into_var == vi, idvar]
  #
  #   other_text <- unique(others_ds_done[others_ds_done$recoded_into_var == vi, "variable"])
  #
  #   other_num <- unique(others_ds_done[others_ds_done$recoded_into_var == vi, "other_column"])
  #
  #   ds_rec <- ds_rec |>
  #     rowwise() |>
  #     #  filter(!!(idvar) %in% id_other_i) |>
  #     group_by(!!sym(idvar)) |>
  #     mutate(!!sym(vi) := ifelse(!!sym(idvar) %in% id_other_i,
  #                                others_ds_done$recoded_into_val[which(!!sym(idvar) == id_other_i[id_other_i == !!sym(idvar)] & others_ds_done$recoded_into_var == vi)], !!sym(vi)))
  #
  #   if(!is.na(other_num)){
  #     ds_rec <- ds_rec |>
  #       mutate(!!sym(other_num) := ifelse(!!sym(idvar) %in% id_other_i, "0", !!sym(other_num)))
  #   }
  #
  # }
  #
  # #duplicates <- others_ds_done %>%
  # # select(`_uuid`, recoded_into_var, recoded_into_val) %>%
  # #filter(duplicated(.) | duplicated(., fromLast = TRUE))
  #
  # # duplicates <- others_ds_done %>%
  # # select(`_uuid`, other_column) %>%
  # # filter(duplicated(.) | duplicated(., fromLast = TRUE))
  #
  # others_ds_done_w <- others_ds_done |>
  #   distinct(`_uuid`, recoded_into_var, recoded_into_val, .keep_all = T) |>
  #   select(!!sym(idvar), variable, other_value, recoded_into_var, recoded_into_val, other_column) |>
  #   pivot_wider(id_cols = !!sym(idvar), names_from = recoded_into_var, values_from = recoded_into_val, values_fill = NA)
  #
  # ds_rec <-  ds_rec1 |>
  #   left_join(others_ds_done_w, by = idvar, suffix = c("", "_rec")) |>
  #   mutate(across(all_of(unique(others_ds_done$recoded_into_var)),
  #                 ~ ifelse(!is.na(!!sym(paste0(cur_column(), "_rec"))), !!sym(paste0(cur_column(), "_rec")), .))) %>%
  #   select(-ends_with("_rec"))
  #
  #
  #
  # others_null <- others_ds_done |>
  #   filter(!is.na(other_column)) |>
  #   mutate(other_dummy = 0) |>
  #   distinct(`_uuid`, other_column, .keep_all = T) |>
  #   select(!!sym(idvar), other_column, other_dummy) |>
  #   pivot_wider(id_cols = !!sym(idvar), names_from = other_column, values_from = other_dummy, values_fill = NA)
  #
  # ds_rec <-  ds_rec |>
  #   left_join(others_null, by = idvar, suffix = c("", "_nul")) |>
  #   mutate(across(all_of(unique(others_ds_done$other_column)),
  #                 ~ ifelse(!is.na(other_column), recoded_into_val, .))) %>%
  #   select(-ends_with("_nul"))
  ###
  ds_rec_long <- ds_rec1 |>
    mutate(across(is.numeric, ~as.character(.))) |>
    pivot_longer(cols = -!!sym(idvar), names_to = "column", values_to = "value")

  others_long_1 <- others_ds_done |>
    select(!!sym(idvar), recoded_into_var, recoded_into_val) |>
    rename(column = recoded_into_var,
           value = recoded_into_val)

  other_long_0 <-   others_ds_done |>
    filter(!is.na(other_column)) |>
    mutate(dummy_other = 0) |>
    select(!!sym(idvar), other_column, dummy_other) |>
    rename(column = other_column,
           value = dummy_other)

  other_txt_rm <- others_ds_done |>
    filter(variable != recoded_into_var | is_valid == F) |>
    select(!!sym(idvar), variable) |>
    rename(column = variable) |>
    mutate(value = NA)

  other_to_upd <- rbind(others_long_1, other_long_0, other_txt_rm) |>
    distinct(!!sym(idvar), column, .keep_all = T)


  duplicates <- other_to_upd %>%
    select(`_uuid`, column) %>%
    filter(duplicated(.) | duplicated(., fromLast = TRUE))

  ds_rec_long_1 <- ds_rec_long |>
    rows_update(other_to_upd, by = c(paste0(idvar), "column"))

  ds_rec <- ds_rec_long_1 |>
    pivot_wider(id_cols = !!sym(idvar), names_from = column, values_from = value)
  ###

  ## for each other variable select ids, in each id select the cell into (var, val) and column with others
  ## for these cells turn text to na,



  # rows_update(others_ds_done |>
  #               filter(recoded_into_var == paste0(v)) |>
  #               rename(!!sym(v) := recoded_into_val) |>
  #               distinct(!!sym(v), !!sym(idvar)) |>
  #               select(!!sym(idvar), !!sym(v)),
  #             by = paste0(idvar)
  #             )

  # ds_rec <- ds_rec |>
  #   left_join(ds_rec1 |> select(!!sym(v), idvar), by = paste0(idvar), suffix = c("", "_XX_org")) |>
  #   mutate(!!sym(v):= coalesce(!!sym(v), as.character(!!sym(paste0(v, "_XX_org"))))) |>
  #   select(-ends_with("_XX_org"))
#a}



#cl_ds <- get_cleaning_log(raw = ds_rec, clean = ds_rec1)

#
#   for(oth_text in unique(others_ds_done$variable)){
#
#     ds_rec <- ds_rec |>
#       rows_update(others_ds_done |>
#                     mutate(oth_unceck = "0") |>
#                     filter(variable == paste0(oth_text)) |>
#                     mutate(!!sym(oth_text) := NA) |>
#                     distinct(!!sym(oth_text), !!sym(idvar)) |>
#                     select(!!sym(idvar), !!sym(oth_text)),
#                   by = paste0(idvar),
#                   unmatched = "error")
#
#
#   }
#     #oth_text <- unique(others_ds_done$variable[others_ds_done$recoded_into_var == v])
#     #oth_num <- unique(others_ds_done$other_column[others_ds_done$recoded_into_var == v])
#
#   for(oth_num in unique(others_ds_done$other_column[!is.na(others_ds_done$other_column)])){
#     #oth_num <- others_ds_done$other_column[1]
#     ds_rec <- ds_rec |>
#       mutate(!!sym(oth_num) := ifelse(is.numeric(!!sym(oth_num)) | is.logical(!!sym(oth_num)), as.character(!!sym(oth_num)), !!sym(oth_num)))|>
#       rows_update(others_ds_done |>
#                     mutate(oth_uncheck = "0") |>
#                     filter(other_column == paste0(oth_num)) |>
#                     mutate(!!sym(oth_num) := "0") |>
#                     distinct(!!sym(oth_num), !!sym(idvar)) |>
#                     select(!!sym(idvar), !!sym(oth_num)),
#                   by = paste0(idvar),
#                   unmatched = "error")
#
#     ds_rec <- ds_rec |>
#       left_join(ds_rec1 |> select(!!sym(oth_num), idvar), by = paste0(idvar), suffix = c("", "_XX_org")) |>
#       mutate(!!sym(v):= coalesce(!!sym(oth_num), as.character(!!sym(paste0(oth_num, "_XX_org"))))) |>
#       select(-ends_with("_XX_org"))
#   }
#
#
# for(viv in unique(others_invalid$variable)){
#   ds_rec <- ds_rec |>
#     rows_update(others_invalid |>
#                   filter(variable == paste0(viv)) |>
#                   rename(!!sym(viv):= recoded_into_val) |>
#                 distinct(!!sym(viv), !!sym(idvar)) |>
#                 select(!!sym(viv), !!sym(idvar)),
#                 by = paste0(idvar),
#                 unmatched = "error")
# }
#
#

# mutate extra 0s - defining variables

#ds_rec <- ds_rec |> mutate(across(contains(sep), as.logical))

ds_rec <- ds_rec |> mutate(across(contains(sep), ~as.numeric(as.character(.))))

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
