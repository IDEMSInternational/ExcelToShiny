#' Title
#'
#' @param data_list todo
#' @param spreadsheet_name todo
#' @param d_box todo
#' @param j todo
#'
#' @return todo
#' @export
#'
#' @examples #todo
tabbed_sheet <- function(data_list = data_list,
                         spreadsheet_name = data_list$contents$ID[[i]],
                         d_box,
                         j = 1){
  
  # TODO: tabbed_sheet only works for display types at the moment!
  spreadsheet <- data_list[[spreadsheet_name]]
  spreadsheet_display <- spreadsheet %>% dplyr::filter(type == "Display")
  
  tab_panel_i <- NULL
  for (i in 1:nrow(spreadsheet_display)){
    tab_item_objects <- tabbed_display_display(spreadsheet_ID_names = spreadsheet_display[["ID"]],
                                               data_list = data_list,
                                               d_box = d_box,
                                               q = i) # q = 1 then 2. 
    tab_panel_i[[i]] <- shiny::tabPanel(spreadsheet_display[["name"]][[i]],
                                        tab_item_objects
    ) 
  }
  
  for (i in 1:length(tab_panel_i)){
    tab_panel_i[[i]]
  }
  
  main_page_info <- which(data_list[["contents"]][["ID"]] == spreadsheet_name)
  main_page_info <- data_list[["contents"]][main_page_info,]
  
  # assuming only two tab items
  tab_panel_items <- do.call(shiny::tabsetPanel, tab_panel_i)
  
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
