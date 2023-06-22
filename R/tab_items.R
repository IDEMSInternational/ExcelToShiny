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
  div(class = "tab-content", ...)
}