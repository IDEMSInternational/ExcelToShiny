#' Create a Tabbed Sheet Layout for Shiny
#'
#' This function generates a tabbed sheet layout for a Shiny application, where each tab corresponds to a different display type.
#' It dynamically creates tab panels based on the provided spreadsheet data.
#'
#' @param data_list A list containing the data associated with each spreadsheet.
#' @param spreadsheet_name The name of the spreadsheet to generate the tabs from.
#' @param d_box A list containing display box configurations for each element.
#' @param status The status of the display box (e.g., "primary").
#' @param colour The background colour of the display box.
#' @param j An integer indicating the tab panel index.
#'
#' @return A `shinydashboard::tabItem` object representing the tabbed sheet layout.
tabbed_sheet <- function(data_list = data_list,
                         spreadsheet_name = data_list$contents$ID[[i]],
                         d_box,
                         status = "primary",
                         colour = "blue",
                         j = 1){
  
  spreadsheet <- data_list[[spreadsheet_name]]

  # TODO: tabbed_sheet only works for display types at the moment!
  spreadsheet_display <- spreadsheet %>% dplyr::filter(type == "Display")
  
  tab_panel_i <- NULL
  for (i in 1:nrow(spreadsheet_display)){
    tab_item_objects <- tabbed_display_display(spreadsheet_ID_names = spreadsheet_display[["ID"]],
                                               data_list = data_list,
                                               d_box = d_box,
                                               q = i) # q = 1 then 2.
    tab_panel_i[[i]] <- shiny::tabPanel(spreadsheet_display[["name"]][[i]],
                                        tab_item_objects,
                                        value = spreadsheet_display[["ID"]][[i]]
    ) 
  }
    
  for (i in 1:length(tab_panel_i)){
    tab_panel_i[[i]]
  }
  
  main_page_info <- which(data_list[["contents"]][["ID"]] == spreadsheet_name)
  main_page_info <- data_list[["contents"]][main_page_info,]
  
  # Create the tabset panel with do.call
  args <- c(list(id = paste0("tab_panel_", j)), tab_panel_i)
  tab_panel_items <- do.call(shiny::tabsetPanel, args)
  
  # tab item
  tab_item <- shinydashboard::tabItem(tabName = data_list$contents$ID[[j]],
                                      shiny::fluidRow(shiny::column(12,
                                                                    align = "center",
                                                                    shinydashboard::box(shiny::splitLayout(shiny::h2(main_page_info$name[[1]]), 
                                                                                                           shiny::icon(main_page_info$icon[[1]], "fa-6x"),
                                                                                                           cellArgs = list(style = "vertical-align: top"), 
                                                                                                           cellWidths = c("80%", "20%")),
                                                                                        status = status,
                                                                                        background = colour,
                                                                                        width = 10,
                                                                                        title = NULL,
                                                                                        collapsible = FALSE,
                                                                                        solidHeader = TRUE,
                                                                                        height = "95px"))),
                                      
                                      tab_panel_items
  ) #closes tabItem
  return(tab_item)
}
