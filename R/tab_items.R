#' Create Tab Items for a Shiny Dashboard
#'
#' This function creates tab items for a Shiny dashboard. Each tab item corresponds to a panel within the tab content.
#'
#' @param ... A series of tab panels to be included as tab items.
#'
#' @return A `div` element containing the tab content for a Shiny dashboard.
tab_items <- function(...) {
  lapply(..., shinydashboard:::tagAssert, class = "tab-pane")
  shiny::div(class = "tab-content", ...)
}
