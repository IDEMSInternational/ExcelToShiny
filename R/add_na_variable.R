#' Add a variable to the data containing NA values
#' @description If a variable is missing in the data frame, add in that variable (with a warning) with NA as its values
#'
#' @param data Data frame to add the variable to.
#' @param variable Variable name to add in the data frame.
#'
#' @return Data frame
#' @export
#'
#' @examples # TODO
add_na_variable <- function(data, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}