#' Swaping dataframe's header from labels to names and vice versa
#'
#' @param ds dataframe;
#' @param codebook dataframe; contains column with xls names and matching xls labels
#' @param name_col character; a column from the codebook containing xls names
#' @param label_col character;a column from the codebook containing xls labels
#' @param result character; target result: "names" or "labels"
#' @param double_header logical; if the dataframe has value labels in its first row
#'
#' @import tidyverse
#' @importFrom data.table copy
#' @importFrom data.table setnames
#'
#' @return dataframe
#' @export
#'
#' @examples

get_labels_names_swaped <- function(ds, codebook, name_col = "name", label_col = "label", result = "names", double_header = FALSE){

  codebook <- codebook |>
    filter(complete.cases(codebook))

  ds_new <- data.table::copy(ds)

  if (double_header == T){
    ds_new  <- ds_new[-1,]
  } else if(result == "names"){
    ds_new <- ds_new |>
      setNames(make.unique(names(ds_new))) |>
      data.table::setnames(new = codebook[, name_col], old = make.unique(codebook[, label_col]))
  } else if(result == "labels"){
    ds_new <- ds_new |>
      data.table::setnames(new = make.unique(codebook[, label_col]), old = codebook[, name_col])
  }
  return(ds_new)
}

# ds = dataset3_1
# codebook = codebook3
# name_col = "variable"
# label_col = "label"
# result = "labels"
