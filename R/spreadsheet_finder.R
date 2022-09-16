#' Find values from a spreadsheet
#'
#' @param data Name of spreadsheet.
#' @param column Column to filter to.
#'
#' @return value from column
#' @export
spreadsheet_finder <- function(data, column){
  if (column %in% data$names) {
    col <- which(data$names == column)
    value <- data$values[col]
  } else {
    value <- NULL
  }
  return(value)
}