#' Create a Display Sheet for Shiny Dashboard
#'
#' This function generates a tab item for a Shiny dashboard based on the contents of a spreadsheet. It organises display boxes within a tab layout, allowing for flexible row-based arrangement of elements in the dashboard.
#'
#' @param data_list A list containing data and configurations for the dashboard. It should include the `contents` data frame, which specifies the layout and structure.
#' @param spreadsheet_name A character string specifying the name of the spreadsheet within `data_list` that defines the content for this tab item.
#' @param d_box A list of display box objects created by the `display_sheet_setup` function, to be arranged within the tab.
#' @param status A character string indicating the status of the tab item, used for styling. Default is `"primary"`.
#' @param colour A character string specifying the background colour of the tab item. Default is `"blue"`.
#' @param j An integer index used for identifying the current tab item within the dashboard. Default is `1`.
#'
#' @return A `tabItem` object for inclusion in a Shiny dashboard, containing the organised display elements.
display_sheet <- function(data_list, spreadsheet_name, d_box, status = "primary", colour = "blue", j = 1){
  # Create the "div" for each row.
  # each row is stored in a list, split_row[[j]] (j = row)
  spreadsheet <- data_list[[spreadsheet_name]]
  split_row_j <- NULL
  k <- 1
  for (i in 1:length(d_box)){
    split_row_j[[i]] <- d_box[[i]][[1]]
    k <- k + 1
  }
  
  # split across rows
  tab_item_objects <- NULL
  for (l in 1:max(spreadsheet[["row"]])){
    row_l_set <- list()
    row_l <- (spreadsheet %>% dplyr::filter(row == l))$name
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
  
  main_page_info <- which(data_list[["contents"]][["ID"]] == spreadsheet_name)
  main_page_info <- data_list[["contents"]][main_page_info,]
  
  if (is.null(main_page_info$icon) || (!is.null(main_page_info$icon) & is.na(main_page_info$icon))){
    shiny_layout_display <- shiny::h2(main_page_info$name)                                                                
  } else {
    shiny_layout_display <- shiny::splitLayout(shiny::h2(main_page_info$name), 
                                               shiny::icon(main_page_info$icon, "fa-6x"),
                                               cellArgs = list(style = "vertical-align: top"), 
                                               cellWidths = c("80%", "20%"))
  }
  
  
  tab_item <- shinydashboard::tabItem(tabName = data_list$contents$ID[[j]],
                                      
                                      # Stuff for the top of the tab
                                      shiny::fluidRow(shiny::column(12,
                                                                    align = "center",
                                                                    shinydashboard::box(shiny_layout_display,
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