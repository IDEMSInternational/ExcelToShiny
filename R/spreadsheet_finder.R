#' Find a Value from a Spreadsheet
#'
#' This function retrieves a value from a specific column in a spreadsheet-like data structure.
#'
#' @param data A data frame or list representing the spreadsheet.
#' @param column The name of the column to search for.
#'
#' @return The value from the specified column, or `NULL` if the column is not found.
spreadsheet_finder <- function(data, column){
  if (column %in% data$names) {
    col <- which(data$names == column)
    value <- data$values[col]
  } else {
    value <- NULL
  }
  return(value)
}