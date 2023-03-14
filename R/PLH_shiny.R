#' Function to create Shiny
#'
#' @param title Title of the dashboard.
#' @param spreadsheet Spreadsheet that contains meta information to put in the box.
#' @param data_frame Spreadsheet that contains information to put in the box.
#' @param colour Skin colour of the Shiny App.
#' @param date_from Initial date to filter from.
#'
#' @return Shiny App
#' @export
#'
PLH_shiny <- function (title, data_list, data_frame, colour = "blue", date_from = "2021-10-14"){
  colour <- tolower(colour)
  if (colour == "blue") {
    status = "primary"
  }
  else if (colour == "green") {
    status = "success"
  }
  else if (colour == "light blue") {
    status = "info"
  }
  else if (colour == "orange") {
    status = "warning"
  }
  else if (colour == "red") {
    status = "danger"
  }
  else {
    warning("Valid colours are blue, green, light blue, orange, red")
    status = "primary"
  }
  
  # Setting up bits to insert into sheets --------------------
  contents <- data_list$contents
  # Display type sheets --------------------------------------
  sheets_to_display <- contents %>% filter(type == "Display")
  no_display <- nrow(sheets_to_display)
  names_display <- sheets_to_display$ID
  display_box <- NULL
  for (i in 1:nrow(contents)){
    if (contents$type[[i]] == "Display"){
      spreadsheet <- data_list[[names_display[[i]]]]
      display_box[[i]] <- display_sheet_setup(spreadsheet_data = spreadsheet,
                                              data_frame = data_frame,
                                              j = i)
      }
  }

  # Populate items for the tabs
  my_tab_items <- create_tab_items(data_list = data_list,
                                   d_box = display_box,
                                   status = status,
                                   colour = colour)
  
  # value box
  spreadsheet_shiny_value_box <- data_list$main_page %>% dplyr::filter(type == "value_box")
  
  shiny_top_box_i <- NULL
  for (i in 1:nrow(spreadsheet_shiny_value_box)){
    shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name, width = 3)
  }

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    dashboardPage(
      # 
      header = shinydashboard::dashboardHeader(title = paste(title, "Dashboard")),
      skin = colour,
      
      # todo: fix up this function
      sidebar = dashboardSidebar(sidebarMenu(menu_items(data_list$contents)[[1]],
                                             menu_items(data_list$contents)[[2]])),

      shinydashboard::dashboardBody(
        #value input boxes
        shiny::fluidRow(shiny_top_box_i),
        # tabs info
        shiny::column(6, align = "center",
                      shinydashboard::box(width = NULL,
                                          collapsible = FALSE,
                                          solidHeader = TRUE,
                                          shiny::splitLayout(shiny::textInput(inputId = "datefrom_text", 
                                                                              label = "Date from:", value = date_from), 
                                                             cellArgs = list(style = "vertical-align: top"),
                                                             cellWidths = c("80%", "20%")))),
        tab_items(my_tab_items)
        
      )
    )
  )
  
  server <- function(input, output) {
      
      # To get all the "display" sheets sorted -----------------------------------------
      display_sheet_plot <- function(j = 1, i){
        return(output[[paste0("plot_", j, "_", i)]] <- plotly::renderPlotly({display_box[[j]][[i]]$plot_obj}))
      }
      display_sheet_table <- function(j = 1, i){
        return(output[[paste0("table_", j, "_", i)]] <-  shiny::renderTable({(display_box[[j]][[i]]$table_obj)}, striped = TRUE))
      }
      for (j in 1:length(display_box)){
        for (i in 1:length(display_box[[j]])){
          display_sheet_plot(j = j, i = i)
          display_sheet_table(j = j, i = i)
        }
      }
      
      
      # value boxes
      display_value_boxes <- function(i = 1){
        ID <- spreadsheet_shiny_value_box[i,]$name
        top_box <- top_value_boxes(data_frame = data_frame,
                                   spreadsheet = spreadsheet_shiny_value_box,
                                   unique_ID = ID)
        
        output[[ID]] <- shinydashboard::renderValueBox({ top_box })
      }
      
     for (i in 1:nrow(spreadsheet_shiny_value_box)) {
       display_value_boxes(i = i)
     }
    
    # 
    # 
    # output$myvaluebox2 <- shinydashboard::renderValueBox({ top_box[[2]] })
    # output$myvaluebox3 <- shinydashboard::renderValueBox({ top_box[[3]] })
    # output$myvaluebox4 <- shinydashboard::renderValueBox({ top_box[[4]] })
    
  }
  shiny::shinyApp(ui = ui, server = server)
}
