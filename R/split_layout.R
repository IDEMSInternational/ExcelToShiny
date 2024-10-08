#' Create a Split Layout for Shiny UI
#'
#' This function creates a split layout for use in Shiny applications, allowing multiple UI elements to be displayed side by side. 
#' It is highly customisable with options for adjusting cell widths and passing additional arguments to individual cells.
#'
#' @param ... UI elements to be included in the layout.
#' @param cellWidths A character vector specifying the widths of the cells. If not provided, cells are given equal width.
#' @param cellArgs A list of additional arguments to pass to each cell.
#'
#' @return A `div` element containing the specified UI elements, arranged in a split layout.
split_layout <- function(..., cellWidths = NULL, cellArgs = list()){
  children <- (...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  count <- length(children)
  if (length(cellWidths) == 0 || isTRUE(is.na(cellWidths))) {
    cellWidths <- sprintf("%.3f%%", 100/count)
  }
  cellWidths <- rep(cellWidths, length.out = count)
  cellWidths <- sapply(cellWidths, shiny::validateCssUnit)
  do.call(shiny::tags$div, c(list(class = "shiny-split-layout"), 
                      attribs, mapply(children, cellWidths, FUN = function(x, 
                                                                           w) {
                        do.call(shiny::tags$div, c(list(style = sprintf("width: %s;", 
                                                                 w)), cellArgs, list(x)))
                      }, SIMPLIFY = FALSE)))
}
