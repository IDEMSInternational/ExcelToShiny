#' Title
#'
#' @param ... todo
#' @param cellWidths todo 
#' @param cellArgs  todo
#'
#' @return todo
#' @export
#'
#' @examples #todo
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
  cellWidths <- sapply(cellWidths, validateCssUnit)
  do.call(tags$div, c(list(class = "shiny-split-layout"), 
                      attribs, mapply(children, cellWidths, FUN = function(x, 
                                                                           w) {
                        do.call(tags$div, c(list(style = sprintf("width: %s;", 
                                                                 w)), cellArgs, list(x)))
                      }, SIMPLIFY = FALSE)))
}