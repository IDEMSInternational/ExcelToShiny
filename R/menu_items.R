#' Title
#'
#' @param contents_list  todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
menu_items <- function(contents_list = data_list$contents){ #todo fix that
  add_menu <- NULL
  for (i in 1:nrow(contents_list)){
    add_menu[[i]] <- shinydashboard::menuItem(contents_list$name[[i]], tabName = contents_list$ID[[i]], icon = shiny::icon(contents_list$icon[[i]]))
  }
  return(add_menu)
}