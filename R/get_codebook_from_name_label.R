#' Get a codebook from a dataset with a double header
#'
#' @param ds dataframe; a dataset, where the header is xls name, while the first row is the xls label
#'
#' @return dataframe; a codebook with correspondence between xls name and xls label
#' @export
#'
#' @examples
get_codebook_from_name_label <- function(ds){
  codebook <- data.frame(name = names(ds),
                         label = t(ds[1, ]))
  return(codebook)
}


