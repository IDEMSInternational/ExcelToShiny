#' Generate Shiny Dashboard Menu Items
#'
#' This function generates a list of menu items for a `shinydashboard` based on the provided contents list. Each menu item corresponds to a tab in the dashboard.
#'
#' @param contents_list A data frame containing the details for each menu item to be generated. The data frame should include columns such as `name`, `ID`, and `icon`.
#'                       Default is `data_list$contents`.
#'
#' @return A list of `shinydashboard` menu items, which can be directly added to a dashboard's sidebar.
menu_items <- function(contents_list = data_list$contents){ #todo fix that
  add_menu <- NULL
  for (i in 1:nrow(contents_list)){
    add_menu[[i]] <- shinydashboard::menuItem(contents_list$name[[i]], tabName = contents_list$ID[[i]], icon = shiny::icon(contents_list$icon[[i]]))
  }
  return(add_menu)
}