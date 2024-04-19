#' Add NA Variable
#'
#' Add NA values to the specified variables in the data frame if they do not exist.
#'
#' @param data A data frame. Default is `contacts_unflat`.
#' @param variable A character vector specifying the variable(s) to add NA values to.
#'
#' @return The modified data frame with NA values added.
#'
#' @examples
#' add_na_variable(data = mtcars, variable = c("cyl", "var2"))
#'
#' @export
add_na_variable <- function(data = contacts_unflat, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}