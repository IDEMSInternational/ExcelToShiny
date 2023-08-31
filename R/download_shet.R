#' Title
#'
#' @param data_list   todo
#' @param spreadsheet_name   todo
#' @param j   todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
download_sheet <- function(data_list, spreadsheet_name, j = 1){
  # this is jth sheet on downloading data - does this work for multiple sheets?
  # what about multiple downloads on one page?
  spreadsheet <- data_list[[spreadsheet_name]]
  data_label <- (spreadsheet %>% dplyr::filter(type == "Data label"))$name
  download_label <- (spreadsheet %>% dplyr::filter(type == "Download label"))$name
  data_names <- (spreadsheet %>% dplyr::filter(type == "Data"))$name
  
  # be able to edit choices, format (csv, etc), table name.
  tab_item_objects <- shiny::fluidRow(
    shinydashboard::box(width = 6, shiny::selectInput(paste0("dataset", j), data_label, choices = data_names),
        # Button
        shiny::downloadButton(paste0("downloadData", j), download_label)),
    shiny::fluidRow(box(width = 12, shiny::dataTableOutput(paste0("table", j))))
  )
  
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