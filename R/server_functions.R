# Helper Function 1: Prepare the data_list and extract df names
extract_df_names <- function(data_list, data_frame_name) {
  for (i in seq_along(data_list)) {
    if ("data" %in% names(data_list[[i]])) {
      data_list[[i]] <- data_list[[i]] %>%
        dplyr::mutate(data = ifelse(is.na(data), data_frame_name, data))
    }
  }
  
  df_names <- purrr::map(data_list, ~ .x$data) %>% unlist() %>% unique()
  df_names <- df_names[!is.na(df_names)]
  
  return(list(data_list = data_list, list_of_df_names = df_names))
}

# Helper Function 2: Generate complete_dfs environment
copy_dfs_for_filtering <- function(df_names, env = parent.frame()) {
  complete_dfs <- list()
  for (df_name in df_names) {
    new_name <- paste0(df_name, "_1")
    stored_data <- get(df_name, envir = env)
    assign(new_name, stored_data, envir = env)
    complete_dfs[[new_name]] <- stored_data
  }
  complete_dfs
}

# Helper Function 3: Create reactive data filtering
create_reactive_expression <- function(df_name, complete_dfs, key_var, valid_ids, grouped_vars = NULL) {
  force(df_name)
  shiny::reactive({
    filtered_data_frame <- complete_dfs[[paste0(df_name, "_1")]]
    
    if (!is.null(grouped_vars)) {
      filtered_data_frame <- filtered_data_frame %>%
        dplyr::full_join(grouped_vars)
    }
    
    if (!is.null(key_var) && key_var %in% names(filtered_data_frame)) {
      filtered_data_frame <- filtered_data_frame %>% 
        dplyr::filter(.data[[key_var]] %in% valid_ids())
    }
    
    return(filtered_data_frame)
  })
}

# Helper Function 4: Display content by tab
display_content_by_tab <- function(tab_name, input, filtered_data, contents, data_list, list_of_reactives) {
  if (input$tab == tab_name) server_display_contents(data_frame = filtered_data(), contents = contents, data_list = data_list, k = which(data_list$contents$type == "Tabbed_display"), id_name = tab_name, list_of_reactives = list_of_reactives)
  else NULL
}

# Helper Function 5: Process spreadsheet for value boxes
process_main_page_function <- function(spreadsheet) {
  spreadsheet_df <- spreadsheet %>%
    dplyr::filter(type == "value_box") %>%
    dplyr::select(name, parameter_list) %>%
    tidyr::separate_rows(parameter_list, sep = ",\\s*") %>%
    tidyr::separate(parameter_list, into = c("names", "values"), sep = "\\s*=\\s*") %>%
    dplyr::mutate(values = stringr::str_remove_all(values, stringr::fixed("\"")))
  return(spreadsheet_df)
}

# Helper Function 6: Render value boxes
draw_top_value_boxes <- function(spreadsheet_shiny_value_box, processed_spreadsheet_data, filtered_data, list_of_reactives, output) {
  shiny::observe({
    lapply(seq_len(length(unique(spreadsheet_shiny_value_box$name))), function(i) {
      ID <- spreadsheet_shiny_value_box[i,]$name
      top_box <- top_value_boxes(
        data_frame = filtered_data(),
        spreadsheet = spreadsheet_shiny_value_box,
        processed_spreadsheet = processed_spreadsheet_data,
        unique_ID = ID,
        list_of_reactives = list_of_reactives
      )
      output[[ID]] <- shinydashboard::renderValueBox(top_box)
    })
  })
}

# Helper Function 7: Render download UI
render_download_ui <- function(j, spreadsheet, datasets, input, output) {
  data_label <- dplyr::filter(spreadsheet, type == "Data label")$name
  download_label <- dplyr::filter(spreadsheet, type == "Download label")$name
  data_names <- dplyr::filter(spreadsheet, type == "Data")$name
  
  output[[paste0("build_download", j)]] <- shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shinydashboard::box(
          width = 6,
          shiny::selectInput(paste0("dataset", j), data_label, choices = data_names),
          shiny::downloadButton(paste0("downloadData", j), download_label)
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            shiny::dataTableOutput(paste0("table", j)),
            style = 'width:100%;overflow-x: scroll;'
          )
        )
      )
    )
  })
  
  datasetInput <- shiny::reactive({
    selected_dataset <- datasets[[j]][[input[[paste0("dataset", j)]]]]
    return(selected_dataset)
  })
  
  output[[paste0("table", j)]] <- shiny::renderDataTable({ datasetInput() })
  
  output[[paste0("downloadData", j)]] <- shiny::downloadHandler(
    filename = function() {
      paste(input[[paste0("dataset", j)]], ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

# Helper Function 8: Extract group input IDs from UI
extract_group_input_ids <- function(group_ui_list) {
  purrr::map_chr(group_ui_list, function(x) {
    x$children[[1]]$children[[1]]$children[[1]]$attribs$id
  })
}

# Helper Function 9: Render plots and tables for Display tabs
render_display_items <- function(display_box, display_content, data_list, output) {
  for (j in which(data_list$contents$type == "Display")) {
    j_element <- display_box[[j]]
    filled_indices <- which(purrr::map_lgl(j_element, ~ !is.null(.x$label_table)))
    
    for (i in filled_indices) {
      local({
        ii <- i
        jj <- j
        output[[paste0("table_", jj, "_", ii)]] <- shiny::renderTable({
          display_content()[[jj]][[ii]]$table_obj
        }, striped = TRUE)
      })
    }
    
    for (i in seq_along(j_element)) {
      local({
        ii <- i
        jj <- j
        output[[paste0("plot_", jj, "_", ii)]] <- plotly::renderPlotly({
          display_content()[[jj]][[ii]]$plot_obj
        })
      })
    }
  }
}

# Helper Function 10: Render plots and tables for Tabbed_display tabs
render_tabbed_display_items <- function(display_box, display_content, data_list, output) {
  for (k in which(data_list$contents$type == "Tabbed_display")) {
    for (j in seq_along(display_box[[k]])) {
      tab_items <- display_box[[k]][[j]]
      for (i in seq_along(tab_items)) {
        local({
          kk <- k
          jj <- j
          ii <- i
          
          output[[paste0(kk, "_table_", jj, "_", ii)]] <- shiny::renderTable({
            display_content()[[kk]][[jj]][[ii]]$table_obj
          }, striped = TRUE)
          
          output[[paste0(kk, "_plot_", jj, "_", ii)]] <- plotly::renderPlotly({
            display_content()[[kk]][[jj]][[ii]]$plot_obj
          })
        })
      }
    }
  }
}

# Server function factory
build_server <- function(data_list, data_frame, key_var, data_frame_name) {
  function(input, output, session) {
    prepared <- extract_df_names(data_list, data_frame_name)
    data_list <- prepared$data_list
    list_of_df_names <- prepared$list_of_df_names
    
    complete_dfs <- copy_dfs_for_filtering(list_of_df_names)
    filtered_data <- shiny::reactive({ data_frame })
    list_of_reactives <- list()
    
    if (!is.null(data_list$main_page)) {
      filter_box_data <- dplyr::filter(data_list$main_page, type == "filter_box")
      group_box_data <- dplyr::filter(data_list$main_page, type == "group_by_box")
      
      group_on_main_page <- main_page_group(spreadsheet = group_box_data)
      group_input_ids <- extract_group_input_ids(group_on_main_page)
      
      grouped_data <- if (nrow(group_box_data) > 0) {
        shiny::eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
          grouped_data <- data_frame
          if (input$goButton_group) {
            active_groups <- purrr::keep(group_input_ids, ~ isTRUE(input[[.x]]))
            
            if (length(active_groups) > 0) {
              group_box_data_i <- dplyr::filter(group_box_data, name == active_groups[1])
              grouped_data <- grouped_data %>%
                dplyr::group_by(!!rlang::sym(group_box_data_i$variable), .add = TRUE)
            } else {
              grouped_data <- grouped_data %>% dplyr::ungroup()
            }
          } else {
            grouped_data <- grouped_data %>% dplyr::ungroup()
          }
          grouped_data
        })
      } else {
        shiny::reactive({ data_frame })
      }
      
      if (nrow(filter_box_data) > 0) {
        filtered_data <- shiny::eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
          filtered_data <- grouped_data()
          
          for (i in seq_len(nrow(filter_box_data))) {
            current_var <- filter_box_data$variable[[i]]
            current_name <- input[[filter_box_data$name[[i]]]]
            current_data <- filter_box_data$data[[i]]
            
            if (is.null(current_data)) {
              filtered_data <- filtered_data %>%
                dplyr::filter(.data[[current_var]] %in% current_name)
            } else {
              current_data <- get(current_data)
              key_vars <- dplyr::filter(current_data, .data[[current_var]] %in% current_name) %>%
                dplyr::pull(key)
              filtered_data <- dplyr::filter(filtered_data, key %in% key_vars)
            }
          }
          filtered_data
        })
      } else {
        filtered_data <- shiny::reactive({ grouped_data() })
      }
      
      if (!is.null(key_var)) {
        valid_ids <- shiny::reactive({ dplyr::pull(filtered_data(), {{ key_var }}) })
        
        grouped_vars <- if (nrow(group_box_data) > 0) {
          shiny::eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
            dplyr::select(grouped_data(), c(key_var, group_box_data$variable))
          })
        } else {
          shiny::reactive(NULL)
        }
        
        for (df_name in list_of_df_names) {
          list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, key_var, valid_ids, grouped_vars())
        }
      } else {
        for (df_name in list_of_df_names) {
          list_of_reactives[[df_name]] <- create_reactive_expression(df_name, complete_dfs, NULL, NULL, NULL)
        }
      }
      
      contents <- data_list$contents
      tab_names <- contents$ID
      display_content <- shiny::reactiveVal()
      shiny::observeEvent(c(input$tab, ifelse(input$goButton_group == 0, 1, input$goButton_group)), {
        display_content(display_content_by_tab(input$tab, input, filtered_data, contents, data_list, list_of_reactives))
      })
      
      spreadsheet_shiny_value_box <- dplyr::filter(data_list$main_page, type == "value_box")
      processed_spreadsheet_data <- process_main_page_function(spreadsheet_shiny_value_box)
      draw_top_value_boxes(spreadsheet_shiny_value_box, processed_spreadsheet_data, filtered_data, list_of_reactives, output)
      
      display_box <- display_contents(
        data_frame = data_frame,
        contents = contents,
        data_list = data_list,
        k = which(data_list$contents$type == "Tabbed_display")
      )
      
      render_display_items(display_box, display_content, data_list, output)
      render_tabbed_display_items(display_box, display_content, data_list, output)
    }
    
    # Setup downloads (with optional credentials)
    datasets <- list()
    for (j in which(data_list$contents$type == "Download")) {
      sheet_name <- data_list$contents$ID[j]
      spreadsheet <- data_list[[sheet_name]]
      data_to_download <- dplyr::filter(spreadsheet, type == "Data")
      datasets[[j]] <- get_data_to_download(data_to_download)
      
      if ("Credentials" %in% spreadsheet$type) {
        credentials <- shinyauthr::loginServer(
          id = paste0("login", j),
          data = credentials_data,
          user_col = user,
          pwd_col = password
        )
        
        output[[paste0("build_download", j)]] <- shiny::renderUI({
          shiny::req(credentials()$user_auth)
          if (credentials()$info$user == "admin") {
            render_download_ui(j, spreadsheet, datasets, input, output)
          }
        })
      } else {
        render_download_ui(j, spreadsheet, datasets, input, output)
      }
    }
    datasets <- list()
    for (j in which(data_list$contents$type == "Download")) {
      sheet_name <- data_list$contents$ID[j]
      spreadsheet <- data_list[[sheet_name]]
      data_to_download <- dplyr::filter(spreadsheet, type == "Data")
      datasets[[j]] <- get_data_to_download(data_to_download)
      
      render_download_ui(j, spreadsheet, datasets, input, output)
    }
  }
}
