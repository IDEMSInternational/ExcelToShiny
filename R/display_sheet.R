#' Title
#'
#' @param data_list todo
#' @param spreadsheet_name todo
#' @param d_box todo
#' @param status todo
#' @param colour todo
#' @param j todo
#'
#' @return todo
#' @export
#'
#' @examples #todo
display_sheet <- function(data_list, spreadsheet_name, d_box, status, colour, j = 1){
  # Create the "div" for each row.
  # each row is stored in a list, split_row[[j]] (j = row)
  spreadsheet <- data_list[[spreadsheet_name]]
  split_row_j <- NULL
  k <- 1
  for (i in 1:length(d_box)){
    split_row_j[[i]] <- d_box[[i]][[1]]
    k <- k + 1
  }
  
  tab_item_objects <- NULL
  for (l in 1:max(spreadsheet[["row"]])){
    row_l_set <- list()
    row_l <- (spreadsheet %>% filter(row == l))$name
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
  
  tab_item <- shinydashboard::tabItem(tabName = data_list$contents$ID[[j]],
                                      
                                      # Stuff for the top of the tab
                                      shiny::fluidRow(shiny::column(12,
                                                                    align = "center",
                                                                    shinydashboard::box(shiny::splitLayout(shiny::h2(data_list$contents$name[[1]]), 
                                                                                                           shiny::icon(data_list$contents$icon[[1]], "fa-6x"),
                                                                                                           cellArgs = list(style = "vertical-align: top"), 
                                                                                                           cellWidths = c("80%", "20%")),
                                                                                        status = status,
                                                                                        background = colour,
                                                                                        width = 10,
                                                                                        title = NULL,
                                                                                        collapsible = FALSE,
                                                                                        solidHeader = TRUE,
                                                                                        height = "95px"))),
                                      
                                      # Tab contents
                                      tab_item_objects
                                      
  )
  return(tab_item)
}