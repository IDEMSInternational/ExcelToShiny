#' Add NA Values to Missing Variables
#'
#' This function checks if the specified variables exist in a given data frame.
#' If any of the specified variables do not exist, they are added to the data frame
#' with all values set to `NA`.
#'
#' @param data A data frame to modify.
#' @param variable A character vector specifying the names of the variables to check
#' and potentially add with `NA` values.
#'
#' @return The modified data frame with the specified variables added if they were missing,
#' containing only `NA` values.
#'
#' @examples
#' # Add missing variables to the 'mtcars' data frame
#' df <- add_na_variable(data = mtcars, variable = c("cyl", "var2"))
#'
#' @export
add_na_variable <- function(data, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}