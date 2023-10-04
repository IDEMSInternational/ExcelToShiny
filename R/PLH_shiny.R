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
PLH_shiny <- function (title, data_list, data_frame, status = "primary", colour = "blue", date_from = "2021-10-14"){
  colour <- tolower(colour)
  if (colour == "blue") {
    status = "primary"
  } else if (colour == "green") { status = "success"
  } else if (colour == "light blue") { status = "info"
  } else if (colour == "orange") { status = "warning"
  } else if (colour == "red") { status = "danger"
  } else {
    warning("Valid colours are blue, green, light blue, orange, red")
    status = "primary"
  }
  
  # Setting up (pre-UI and pre-server items) --------------------------------
  contents <- data_list$contents
  
  # Contents to display
  # TODO: hopefully works for multiple tab displays! :) 
  display_box <- display_contents(data_frame = data_frame, contents1 = contents, data_list = data_list, k = which(data_list$contents$type == "Tabbed_display"))
  # Populate items for the tabs ---
  # investigate my_tab_items[[4]]
  my_tab_items <- create_tab_items(data_list = data_list,
                                   d_box = display_box,
                                   status = status,
                                   colour = colour)
  
  # value box for main page ---
  shiny_top_box_i <- NULL
  if (!is.null(data_list$main_page)){
    spreadsheet_shiny_value_box <- data_list$main_page %>% dplyr::filter(type %in% c("value_box", "mean_box", "mean_sd_box"))
    for (i in 1:nrow(spreadsheet_shiny_value_box)){
      if (nrow(spreadsheet_shiny_value_box) <= 4){
        shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name, width = 12/nrow(spreadsheet_shiny_value_box))
      } else {
          shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name)
      }
    } 
  }
  sidebar_menu <- do.call(shinydashboard::sidebarMenu, menu_items(data_list$contents))
  # Set up UI -------------------------------------------------------
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinydashboard::dashboardPage(
      # 
      header = shinydashboard::dashboardHeader(title = paste(title, "Dashboard")),
      skin = colour,
      
      # todo: fix up this function to allow N items (rather than having to tell it how many)
      sidebar = shinydashboard::dashboardSidebar(sidebar_menu),
      
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
    # value boxes at the top of the thing --------------------------------
    if (!is.null(data_list$main_page)){
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
    }
    
    # The "display" sheets -----------------------------------------
    display_sheet_plot <- function(j = 1, i){
      return(output[[paste0("plot_", j, "_", i)]] <- plotly::renderPlotly({display_box[[j]][[i]]$plot_obj}))
    }
    display_sheet_table <- function(j = 1, i){
      return(output[[paste0("table_", j, "_", i)]] <-  shiny::renderTable({(display_box[[j]][[i]]$table_obj)}, striped = TRUE))
    }
    for (j in which(data_list$contents$type == "Display")){
      purrr::map(1:length(display_box[[j]]), .f = ~ display_sheet_table(j = j, i = .x))
      purrr::map(1:length(display_box[[j]]), .f = ~ display_sheet_plot(j = j, i = .x))
    }
    
    # The tab-display sheets ---------------------------------------------
    if (nrow(data_list$contents %>% dplyr::filter(type == "Tabbed_display")) > 0){
      for (k in which(data_list$contents$type == "Tabbed_display")){
        tab_display_sheet_plot <- function(k = 4, j = 1, i){ # TODO fix for all tab 1_
          # instead of 1_ we want to say k_ really.
          return(output[[paste0(k, "_plot_", j, "_", i)]] <- plotly::renderPlotly({display_box[[k]][[j]][[i]]$plot_obj}))
        }
        tab_display_sheet_table <- function(k = 4, j = 1, i){
          return(output[[paste0(k, "_table_", j, "_", i)]] <-  shiny::renderTable({(display_box[[k]][[j]][[i]]$table_obj)}, striped = TRUE))
        }
        #for (k in which(data_list$contents$type == "Tabbed_display")){
        # TODO: works for multiple tab displays?
        for (j in 1:length(display_box[[k]])){
          purrr::map(1:length(display_box[[k]][[j]]), .f = ~ tab_display_sheet_table(k = k, j = j, i = .x))
          purrr::map(1:length(display_box[[k]][[j]]), .f = ~ tab_display_sheet_plot(k = k, j = j, i = .x))
        }
      }
    }

    # The "download" sheets -----------------------------------------
    # todo: CSV set up - function that writes multiple formats to use instead of write.csv
    # `write`?
    if (length(which(data_l$contents$type == "Download")) > 1){
      warning("Use only one download tab. It will be implemented later to have multiple download tabs, but currently this is not available.")
    }
    
    render_table <- function(j = 1){
      return(output[[paste0("table", j)]] <- shiny::renderDataTable({datasetInput()}))
    }
    
    # only csv is accepted at the moment
    download_table <- function(j){
      download_item <- shiny::downloadHandler(
        filename = function() {
          paste(input[[paste0("dataset", j)]], ".csv", sep = "")
        },
        content = function(file) {
          utils::write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
      return(output[[paste0("downloadData", j)]] <- download_item)
    }
    
    datasets <- NULL
    for (j in which(data_list$contents$type == "Download")){
      spreadsheet <- data_list$contents$ID[j]
      data_to_download <- data_list[[spreadsheet]] %>% dplyr::filter(type == "Data")
      
      # currently this overwrites by having two sheets to download - todo: fix up so we can have multiple downloaded pages. 
      datasets[[j]] <- get_data_to_download(data_to_download)

      # Define a reactive to select the dataset
      datasetInput <- reactive({
        selected_dataset <- datasets[[j]][[input[[paste0("dataset", j)]]]]
        return(selected_dataset)
      })
      
      # render table and downloadability
      render_table(j = j)
      download_table(j = j)
    }
    
    }
  shiny::shinyApp(ui = ui, server = server)
}