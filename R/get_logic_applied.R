#' Applying corrections to the survey logic
#'
#' @param ds dataframe
#' @param logic_frame_issues dataframe; reviewed survey logic issues frame (the issues frame is generated with `get_logic_errors_categorical()` or `get_logic_errors_categorical_numeric()`)
#' @param idvar character; a name of unique observation ID (default "_uuid")
#'
#' @return dataframe; updated dataset
#' @export
#'
#' @examples

get_logic_applied <- function(ds, logic_frame_issues, idvar = "_uuid"){
  nam_ds <- names(ds)
  nam_logic <- logic_frame_issues$variable_to_change
  id_logic <- logic_frame_issues[idvar]
  nam_ds <- setdiff(nam_ds, idvar)


  for(vi in nam_logic){
    id_logic_i <- logic_frame_issues[logic_frame_issues$variable_to_change == vi, idvar]

    for(id in id_logic_i){

      ds[which(ds[idvar] == id), vi] <- logic_frame_issues$correct_value[which(logic_frame_issues[idvar] == id & logic_frame_issues$variable_to_change == vi)]

    }

  }
  return(ds)
}
