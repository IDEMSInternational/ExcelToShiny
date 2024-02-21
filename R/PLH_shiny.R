#' PLH SHINY FUNCTION - exploring using observe() and lazy load.
#'
#'
#' Function to create Shiny
#'
#' @param title Title of the dashboard.
#' @param spreadsheet Spreadsheet that contains meta information to put in the box.
#' @param data_frame Spreadsheet that contains information to put in the box.
#' @param colour Skin colour of the Shiny App.
#' @param date_from Initial date to filter from.
#' @param key_var Name of the variable to link different data frames together.
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
  
  for (i in 1:length(data_list)){
    if (is.null(data_list[[i]]$name)){
      data_list[[i]]$name <- paste0("box", 1:nrow(data_list[[i]]))
    }
  }
  
  # Populate items for the tabs ------------------------------------------------
  display_box <- display_contents(data_frame = data_frame, contents1 = contents, data_list = data_list, k = which(data_list$contents$type == "Tabbed_display"))
  my_tab_items <- create_tab_items(data_list = data_list,
                                   d_box = display_box,
                                   status = status,
                                   colour = colour)
  
  # value box for main page ----------------------------------------------------
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
    
    # checkbox group bys
    group_by_data_box <- data_list$main_page %>% dplyr::filter(type %in% c("group_by_box"))
    if (nrow(group_by_data_box) > 0) {
      group_on_main_page <- main_page_group(spreadsheet = group_by_data_box)
    } else {
      group_on_main_page <- NULL
    }
  } else {
    filter_on_main_page <- NULL
    group_on_main_page <- NULL
  }
  # Assuming menu_items(data_list$contents) returns a list of menuItem objects
  menu_items_list <- menu_items(data_list$contents)
  
  # Add the 'id' to the list of arguments
  args <- c(list(id = "tab"), menu_items_list)
  
  # Create the sidebar menu with do.call
  sidebar_menu <- do.call(shinydashboard::sidebarMenu, args)
  
  #sidebar_menu <- do.call(shinydashboard::sidebarMenu, menu_items(data_list$contents))
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
                      filter_on_main_page,
                      group_on_main_page
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
    list_of_df_names <- unique(unlist(list_of_df_names))
    if (!is.null(list_of_df_names) && !is.na(list_of_df_names)){
      for (df_name in list_of_df_names){
        new_name <- paste0(df_name, "_1")
        stored_data <- get(df_name)
        assign(new_name, stored_data, envir = environment())
        complete_dfs[[paste0(df_name, "_1")]] <- get(paste0(df_name, "_1"))
      }
    }
    
    create_reactive_expression <- function(df_name, complete_dfs, key_var, valid_ids, grouped_vars = NULL) {
      force(df_name) # Force the evaluation of df_name
      shiny::reactive({
        filtered_data_frame <- complete_dfs[[paste0(df_name, "_1")]]
        if (!is.null(grouped_vars)){
          filtered_data_frame <- filtered_data_frame %>%
            dplyr::full_join(grouped_vars)
        }
        if (!is.null(key_var) && key_var %in% names(filtered_data_frame)){
          filtered_data_frame <- filtered_data_frame %>% 
            dplyr::filter(.data[[key_var]] %in% valid_ids())
        } else {
          filtered_data_frame
        }
        return(filtered_data_frame)
      })
    }
    
    # main page - handling filters and grouped data --------------------------
    filtered_data  <- shiny::reactive({ data_frame })
    
    # we create them before. What if they are summary data frames?
    if (!is.null(data_list$main_page)){
      filter_box_data <- (data_list$main_page %>% dplyr::filter(type == "filter_box"))
      group_box_data <- (data_list$main_page %>% dplyr::filter(type == "group_by_box"))
      
      # if we have grouping involved - ######################################-
      if (nrow(group_box_data) > 0){
        # this doesn't run on other tabs atm.
        # TODO: need this to work elsewhere, and to have it work on other dfs
        grouped_data <- shiny::eventReactive(input$group_by_button, {
          grouped_data <- data_frame
          
          if (input$group_by_button){
            grouped_data <- grouped_data %>%
              dplyr::group_by(!!sym(group_box_data$variable), .add = TRUE)
          } else {
            grouped_data <- grouped_data %>% ungroup()
          }
          return(grouped_data)
        })
        # then we need to group by this variable in the other data frames, IF this variable is present.
        # if this variable isn't present, then:
        # # we have to do something fancy with with IDs
        # # add a new variable to the initial df stating the new groups,
        # # and then merge that into the other dfs using the key_var
        # # and group by that. 
      } else {
        grouped_data <- shiny::reactive({ data_frame })
      }
      
      # if we have filtering involved -######-
      if (nrow(filter_box_data) > 0){
        filtered_data <- shiny::eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
          filtered_data <- grouped_data()
          variable <- filter_box_data$variable
          name <- filter_box_data$name
          # filter for each variable specified.
          for (i in 1:nrow(filter_box_data)){
            current_var <- variable[[i]]
            current_name <- input[[name[[i]]]]
            
            if (is.character(current_name)) {
              # For character variables, use %in% for exact matching
              filtered_data <- filtered_data %>% 
                dplyr::filter(.data[[current_var]] %in% current_name)
            } else if (is.numeric(current_name)) {
              # For numeric variables, we'll assume they are exact values to match
              filtered_data <- filtered_data %>%
                dplyr::filter(.data[[current_var]] %in% current_name)
            }
          }
          return(filtered_data)
        })
        
        if (!is.null(key_var)){
          valid_ids <- shiny::reactive({
            filtered_data() %>% dplyr::pull({{ key_var }})
          })
          
          if (nrow(group_box_data) > 0){
            grouped_vars <- shiny::eventReactive(input$group_by_button, {
              grouped_data() %>% dplyr::select(c(key_var, group_box_data$variable))
            })
          } else {
            null_function <- function() {
              return(NULL)
            }
            grouped_vars <- shiny::reactive(null_function())
          }
          
          for (df_name in list_of_df_names) {
            list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, key_var, valid_ids, grouped_vars())
            }
        } else {
          # TODO : what if there is no key?
        }
      } else {
        filtered_data  <- shiny::reactive({ grouped_data() })#data.frame
        for (df_name in list_of_df_names) {
          list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, NULL, valid_ids, NULL)
        }
      }
    } else {
      filtered_data <- shiny::reactive({ data_frame })
      for (df_name in list_of_df_names) {
        list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, NULL, valid_ids, NULL)
      }
    }
    
    # display content is for displaying the content for where?
    # event reactive?
    # Display content is a list containing all the content to display later
    # We currently only run it for df and for our final item in list_of_reactives
    tab_names <- data_list$contents$ID
    #need_update <- rep(TRUE, length(tab_names))
    # Initialise flags to indicate whether each tab's content needs to be updated
    # TODO: reset all as TRUE whenever we click to update
    # shiny::observe(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
    #   need_update <- rep(TRUE, length(tab_names))
    # })
    
    display_content <- shiny::reactiveVal()
    shiny::observeEvent(c(input$tab, ifelse(input$goButton_group == 0, 1, input$goButton_group)), {
      # Update content for the current tab
      display_content_by_tab(input$tab)
      # Set flag to indicate that the content for the current tab is up-to-date
      #need_update[input$tab] <- FALSE
    })
    
    # update list_of_reactives and filtered_data to contain bits from grouped_data
    
    
    display_content_by_tab <- function(tab_name){
      # Check if the current tab matches the given tab name
      if (input$tab == tab_name) {
        display_content(server_display_contents(data_frame = filtered_data(),
                                                contents1 = contents, data_list = data_list,
                                                k = which(data_list$contents$type == "Tabbed_display"),
                                                id_name = tab_name,
                                                list_of_reactives = list_of_reactives))
      }
    }
    
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
    
    # value boxes at the top of the thing
    if (!is.null(filter_on_main_page) | !is.null(group_on_main_page)){
      # Process spreadsheet data outside of the top_value_boxes function
      processed_spreadsheet_data <- process_spreadsheet_function(spreadsheet_shiny_value_box)
      
      shiny::observe({ # observeEvent - this should occur when filtered_data() is updated
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
    
    # blanking these out: Then it runs right away (because we're not running the tables stuff?)
    # Keeping these in: it runs right away (because we're not running the tables stuff?)
    # if it takes X minutes per person, then ...
    
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
      # AIM: Get which of my display_box[[j]][[i]]$label_table are NOT NULL
      j_element <- display_box[[j]]
      display_table <- NULL
      filled_indices <- 1
      for (i in seq_along(j_element)) {
        if (!is.null(j_element[[i]]$label_table)) {
          display_table[filled_indices] <- i
          filled_indices <- filled_indices + 1
        }
      }
      purrr::map(.x = display_table, .f = ~ display_sheet_table(j = j, i = .x))
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
      datasetInput <- shiny::reactive({
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
