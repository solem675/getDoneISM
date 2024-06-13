#' Replace outliers in a dataset with user-defned values
#'
#' @param outliers dataframe; manually edited dataframe with outliers (initially generated with `get_outliers_med_sd()`)
#' @param ds dataframe;
#' @param idvar character; a name of unique observation ID (default "_uuid")
#'
#' @return dataframe
#' @export
#'
#' @examples

get_outliers_replaced <- function(outliers, ds, idvar = "_uuid"){
  numeric_outliers <- outliers |>
    filter(is.na(keep) | keep %in% c(FALSE, "FALSE"))


  nam_ds <- names(ds)
  nam_outl <- numeric_outliers$variable
  id_outl <- numeric_outliers[,"idvar"]
  nam_ds <- setdiff(nam_ds, "idvar")


  for(vi in unique(nam_outl)){
    id_outl_i <- numeric_outliers[numeric_outliers$variable == vi, "idvar"]

    for(id in id_outl_i){
      #replace with median
      if(numeric_outliers[numeric_outliers$variable == vi & numeric_outliers$idvar == id, "action"] == "median"){
        ds[which(ds[,idvar] == id), vi] <- numeric_outliers[which(numeric_outliers[,"idvar"] == id & numeric_outliers[,"idvar"] == id), "median_value"]
      }
      #replace with quantile value (1st or 99th)
      if(numeric_outliers[numeric_outliers$variable == vi & numeric_outliers$idvar == id, "action"] == "q_v"){
        ds[which(ds[,idvar] == id), vi] <- numeric_outliers[which(numeric_outliers[,"idvar"] == id & numeric_outliers[,"idvar"] == id), "q_v"]
      }
      #replace with NA
      if(numeric_outliers[numeric_outliers$variable == vi & numeric_outliers$idvar == id, "action"] == "na"){
        ds[which(ds[,idvar] == id), vi] <- NA
      }

    }

  }
  return(ds)
}

# replace to NA
# replace to median (of non-zero values)

