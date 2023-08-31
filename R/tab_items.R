#' Title
#'
#' @param ...   todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
tab_items <- function(...) {
  lapply(..., shinydashboard:::tagAssert, class = "tab-pane")
  shiny::div(class = "tab-content", ...)
}