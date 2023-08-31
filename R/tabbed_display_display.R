#' Title
#'
#' @param spreadsheet_ID_names todo
#' @param data_list todo
#' @param d_box todo
#' @param q todo
#'
#' @return todo
#' @export
#'
#' @examples #todo
tabbed_display_display <- function(spreadsheet_ID_names, data_list, d_box, q = 1){
  ## --- Contents for 1st display tab --- ##
  spreadsheet_ID_name <- spreadsheet_ID_names[[q]] # for now
  spreadsheet_ID <- data_list[[spreadsheet_ID_name]]
  d_box <- d_box[[q]]  # for now
  split_row_j <- NULL
  k <- 1
  for (i in 1:length(d_box)){
    split_row_j[[i]] <- d_box[[i]][[1]]
    k <- k + 1
  }
  
  # split across rows
  tab_item_objects <- NULL
  for (l in 1:max(spreadsheet_ID[["row"]])){
    row_l_set <- list()
    row_l <- (spreadsheet_ID %>% dplyr::filter(row == l))$name
    k <- 1
    for (i in 1:length(d_box)){
      if (d_box[[i]]$ID %in% row_l){
        row_l_set[[k]] <- split_row_j[[i]]
        k <- k + 1
      }
    }
    tab_item_objects[[l]] <- split_layout(row_l_set)
  }
  
  for (l in 1:length(tab_item_objects)){
    tab_item_objects[[l]] <- shiny::fluidRow(shiny::column(12,
                                                           align = "center",
                                                           tab_item_objects[[l]]),
                                             width = 10)
  }
  return(tab_item_objects)
}