#' Create a Download Sheet for Shiny Dashboard
#'
#' This function generates a tab item in a Shiny dashboard specifically for downloading data. It sets up the layout for download labels, data options, and integrates user authentication where needed.
#'
#' @param data_list A list containing data and configurations for the dashboard. It includes the `contents` data frame, which specifies the layout and structure of the tabs.
#' @param spreadsheet_name A character string specifying the name of the spreadsheet within `data_list` that defines the content and download options for this tab item.
#' @param j An integer index used to uniquely identify the current tab item within the dashboard. Default is `1`.
#' @param status A character string indicating the status of the tab item, used for styling. Default is `"primary"`.
#' @param colour A character string specifying the background colour of the tab item. Default is `"blue"`.
#'
#' @return A `tabItem` object for inclusion in a Shiny dashboard, configured for downloading data.
download_sheet <- function(data_list, spreadsheet_name, status = "primary", colour = "blue", j = 1){
  # this is jth sheet on downloading data - does this work for multiple sheets?
  # what about multiple downloads on one page?
  spreadsheet <- data_list[[spreadsheet_name]]
  data_label <- (spreadsheet %>% dplyr::filter(type == "Data label"))$name
  download_label <- (spreadsheet %>% dplyr::filter(type == "Download label"))$name
  data_names <- (spreadsheet %>% dplyr::filter(type == "Data"))$name

  # be able to edit choices, format (csv, etc), table name.
  
  main_page_info <- which(data_list[["contents"]][["ID"]] == spreadsheet_name)
  main_page_info <- data_list[["contents"]][main_page_info,]
  tab_item <- shinydashboard::tabItem(tabName = data_list$contents$ID[[j]],
                                      
                                      # Stuff for the top of the tab
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
                                      
                                      # Tab contents
                                      shinyjs::useShinyjs(),
                                      shinyauthr::loginUI(paste0("login", j)),
                                      uiOutput(paste0("build_download", j))
  )
  
  return(tab_item)
}