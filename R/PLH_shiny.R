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
PLH_shiny <- function (title, data_list, data_frame, status = "primary", colour = "blue", date_from = "2021-10-14", key_var = NULL){
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
    # value boxes
    spreadsheet_shiny_value_box <- data_list$main_page %>% dplyr::filter(type %in% c("value_box"))
    for (i in 1:nrow(spreadsheet_shiny_value_box)){
      if (nrow(spreadsheet_shiny_value_box) <= 4){
        shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name, width = 12/nrow(spreadsheet_shiny_value_box))
      } else {
        shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name)
      }
    } 
    
    # checkbox filters
    checkbox_data <- data_list$main_page %>% dplyr::filter(type %in% c("filter_box"))
    if (nrow(checkbox_data) > 0) {
      filter_on_main_page <- main_page_filter(spreadsheet = checkbox_data)
    } else {
      filter_on_main_page <- NULL
    }
  } else {
    filter_on_main_page <- NULL
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
                      filter_on_main_page
                      # shiny::splitLayout(shiny::textInput(inputId = "datefrom_text", 
                      #                                     label = "Date from:", value = date_from), 
                      #                    cellArgs = list(style = "vertical-align: top"),
                      #                    cellWidths = c("80%", "20%")))
        ),
        tab_items(my_tab_items)
        
      )
    )
  )
  
  server <- function(input, output) {
    contents <- data_list$contents
    list_of_reactives <- NULL
    
    # run through the other sheets and get all data frame names
    # then save them all as "name_1" to call later.
    # this is for filtering purposes
    complete_dfs <- NULL
    list_of_df_names <- NULL
    for (i in 1:length(data_list)){
      data_l_dfs <- data_list[[i]]$data
      if (!is.null(data_l_dfs)){
        list_of_df_names[[i]] <- unique(data_l_dfs)
      }
    }
    list_of_df_names <- unlist(list_of_df_names)
    for (df_name in list_of_df_names){
      new_name <- paste0(df_name, "_1")
      stored_data <- get(df_name)
      assign(new_name, stored_data, envir = environment())
      complete_dfs[[paste0(df_name, "_1")]] <- get(paste0(df_name, "_1"))
    }
    
    # main page - adding filters
    
    # todo: what about filtering other dfs, not the data_frame df.
    # we create them before. What if they are summary data frames?
    if (is.null(data_list$main_page)){
      filtered_data  <- reactive({ data_frame })
    } else {
      filter_box_data <- (data_list$main_page %>% dplyr::filter(type == "filter_box"))
      
      # if we have filtering involved
      if (nrow(filter_box_data) > 0){
        filtered_data  <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
          filtered_data <- data_frame
          variable <- filter_box_data$variable
          name <- filter_box_data$name
          # filter for each variable specified.
          for (i in 1:nrow(filter_box_data)){
              current_var <- variable[[i]]
              current_name <- input[[name[[i]]]]
              
              if (is.character(current_name)) {
                # For character variables, use %in% for exact matching
                filtered_data <- filtered_data %>% 
                  filter(.data[[current_var]] %in% current_name)
              } else if (is.numeric(current_name)) {
                # For numeric variables, we'll assume they are exact values to match
                filtered_data <- filtered_data %>%
                  filter(.data[[current_var]] %in% current_name)
              }
          }
          return(filtered_data)
        })
        if (!is.null(key_var)){
          valid_ids <- reactive({
            filtered_data() %>% dplyr::pull({{ key_var }})
          })
          create_reactive_expression <- function(df_name, complete_dfs, key_var, valid_ids) {
            force(df_name) # Force the evaluation of df_name
            reactive({
              filtered_data_frame <- complete_dfs[[paste0(df_name, "_1")]]
              if (key_var %in% names(filtered_data_frame)){
                filtered_data_frame %>% 
                  dplyr::filter(.data[[key_var]] %in% valid_ids())
              } else {
                filtered_data_frame
              }
              return(filtered_data_frame)
            })
          }
          
          for (df_name in list_of_df_names) {
            list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, key_var, valid_ids)
          }
        } else {
          # TODO : what if there is no key?
        }
      } else {
        filtered_data  <- reactive({ data_frame })
      }
    }
    
    # display content is for displaying the content for where?
    # event reactive?
    # Display content is a list containing all the content to display later
    # We currently only run it for df and for our final item in list_of_reactives
    display_content <- reactive({
      # TODO: we want this to display for ALL reactives
      # so we want to repeat this for all reactives. 
      server_display_contents(data_frame = filtered_data (),
                                 contents1 = contents, data_list = data_list,
                                 k = which(data_list$contents$type == "Tabbed_display"),
                                 list_of_reactives = list_of_reactives)
    })
    
    # Function to process spreadsheet data
    process_spreadsheet_function <- function(spreadsheet) {
      spreadsheet_df <- spreadsheet %>%
        dplyr::filter(type == "value_box") %>%
        dplyr::select(name, parameter_list) %>%
        # Separate the parameter_list into multiple rows
        tidyr::separate_rows(parameter_list, sep = ",\\s*") %>%
        # Extract parameter name and value
        tidyr::separate(parameter_list, into = c("names", "values"), sep = "\\s*=\\s*") %>%
        dplyr::mutate(values = stringr::str_remove_all(values, stringr::fixed("\"")))
      return(spreadsheet_df)
    }

    # value boxes at the top of the thing --------------------------------
    if (!is.null(spreadsheet_shiny_value_box)){
      # Process spreadsheet data outside of the top_value_boxes function
      processed_spreadsheet_data <- process_spreadsheet_function(spreadsheet_shiny_value_box)
      
      observe({
        # running for changed elements.
        lapply(seq_len(length(unique(spreadsheet_shiny_value_box$name))), function(i) {
          ID <- spreadsheet_shiny_value_box[i,]$name
          top_box <- top_value_boxes(data_frame = filtered_data(),
                                     spreadsheet = spreadsheet_shiny_value_box,
                                     processed_spreadsheet = processed_spreadsheet_data,
                                     unique_ID = ID)
          output[[ID]] <- shinydashboard::renderValueBox(top_box)
        })
      })
    }

    # The "display" sheets -----------------------------------------
    display_sheet_plot <- function(j = 1, i){
      return(output[[paste0("plot_", j, "_", i)]] <- plotly::renderPlotly({
        display_content()[[j]][[i]]$plot_obj
      }))
    }
    display_sheet_table <- function(j = 1, i){
      return(output[[paste0("table_", j, "_", i)]] <-  shiny::renderTable({(
        display_content()[[j]][[i]]$table_obj)}, striped = TRUE))
      # TODO: create the table object here
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
          return(output[[paste0(k, "_plot_", j, "_", i)]] <- plotly::renderPlotly({
            display_content()[[k]][[j]][[i]]$plot_obj}))
        }
        tab_display_sheet_table <- function(k = 4, j = 1, i){
          return(output[[paste0(k, "_table_", j, "_", i)]] <-  shiny::renderTable({(display_content()[[k]][[j]][[i]]$table_obj)}, striped = TRUE))
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
    if (length(which(data_list$contents$type == "Download")) > 1){
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
