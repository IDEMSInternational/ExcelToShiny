#' Setup Display Sheets for Shiny Application
#'
#' This function sets up display sheets for a Shiny application by creating boxes for each relevant item in the provided spreadsheet data. It handles the creation of labels for tables and plots, and filters the data to include only those entries marked as "box" type.
#'
#' @param spreadsheet_data A data frame containing the data for the spreadsheet, which includes the information required to set up the display boxes.
#' @param data_frame The main data frame containing the data to be displayed in the boxes.
#' @param j An integer index used for creating unique labels for tables and plots within the Shiny app.
#' @param loop An optional parameter used to handle nested loops for label creation. If not provided, the function will create labels without a loop suffix.
#'
#' @return A list of display boxes (`d_box`) ready to be integrated into a Shiny application.
display_sheet_setup <- function(spreadsheet_data, data_frame, j, loop) {
  # Filter the data once and store it in a variable
  filtered_spreadsheet_data <- spreadsheet_data %>% dplyr::filter(type == "box")
  
  # Define a helper function to create label names
  create_labels <- function(index) {
    suffix <- if (is.null(loop)) "" else paste0(loop, "_")
    list(
      label_table = paste0(suffix, "table_", j, "_", index),
      label_plot = paste0(suffix, "plot_", j, "_", index)
    )
  }
  
  # Use lapply for more concise code
  d_box <- lapply(seq_len(nrow(filtered_spreadsheet_data)), function(i) {
    ID <- filtered_spreadsheet_data[i,]$name
    labels <- create_labels(i)
      box_function(data_frame = data_frame, 
                   spreadsheet = filtered_spreadsheet_data,
                   unique_ID = ID,
                   label_table = labels$label_table,
                   label_plot = labels$label_plot)
  })
  return(d_box)
}

#' Creating a Box for Use in the `PLH_shiny` Function
#'
#' This function generates a box element for use in `Shiny` applications, tailored for the `PLH_shiny` function. It reads parameters from a spreadsheet template and constructs the box accordingly.
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier used to select the appropriate row from the spreadsheet.
#' @param label_table Identifier for the table output within the box.
#' @param label_plot Identifier for the plot output within the box.
#'
#' @return A list containing the constructed box for use in `Shiny` applications, along with associated labels and identifiers.
box_function <- function(data_frame, spreadsheet, unique_ID, label_table, label_plot){
  all_return <- NULL
  
  # we repeat for each row later in the plh_shiny function
  # for now, just get the data
  #spreadsheet <- testing_shiny
  spreadsheet <- spreadsheet %>% dplyr::filter(name == unique_ID)
  spreadsheet_parameters <- spreadsheet$parameter_list
  spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, stringr::fixed("\", "), simplify = TRUE))
  #spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, stringr::fixed(", "), simplify = TRUE))
  spreadsheet_parameters_names <- sub("\\=.*", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- gsub(".*=", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- stringr::str_remove_all(spreadsheet_parameters_values, stringr::fixed("\""))
  values <- spreadsheet_parameters_values
  names <- trimws(spreadsheet_parameters_names)
  spreadsheet_df <- data.frame(names, values)
  
  #repeat for all variables like text, etc. so make into a function?
  content_text <- trimws(spreadsheet_finder(data = spreadsheet_df, "content_text"))
  text <- trimws(spreadsheet_finder(data = spreadsheet_df, "text"))
  width <- trimws(spreadsheet_finder(data = spreadsheet_df, "width"))
  colour <- trimws(spreadsheet_finder(data = spreadsheet_df, "colour"))
  footer <- trimws(spreadsheet_finder(data = spreadsheet_df, "footer"))
  colour <- tolower(colour)
  if (length(colour) == 0){
    status = "primary"
  } else {
    if (colour == "blue") {
      status = "primary"
    } else if (colour == "green") {
      status = "success"
    } else if (colour == "light blue") {
      status = "info"
    } else if (colour == "orange") {
      status = "warning"
    } else if (colour == "red") {
      status = "danger"
    } else {
      warning("Valid colours are blue, green, light blue, orange, red")
      status = "primary"
    }
  }
  
  if (spreadsheet$value == "specify_plot"){
    if (is.null(spreadsheet$table_manip) | (!is.null(spreadsheet$table_manip) && is.na(spreadsheet$table_manip))){
      all_return[[1]] <- shinydashboard::box(width=NULL,
                                             collapsible = FALSE,
                                             title = text,
                                             status = status, # primary, success, info, warning, danger
                                             solidHeader = TRUE,
                                             footer = footer,
                                             content_text,
                                             style='width:100%;overflow-x: scroll;',
                                             plotly::plotlyOutput(outputId = label_plot, height = "240"))
      all_return[[4]] <- NULL
      all_return[[5]] <- label_plot 
    } else {
      all_return[[1]] <- shinydashboard::box(width=NULL,
                                             collapsible = FALSE,
                                             title = text,
                                             status = status, # primary, success, info, warning, danger
                                             solidHeader = TRUE,
                                             footer = footer,
                                             content_text,
                                             style='width:100%;overflow-x: scroll;',
                                             plotly::plotlyOutput(outputId = label_plot, height = "240"),
                                             shiny::tableOutput(label_table))
      all_return[[4]] <- label_table
      all_return[[5]] <- label_plot
    }
  } else if (spreadsheet$value %in% c("data_frame", "specify_table")){
    all_return[[1]] <- shinydashboard::box(width=NULL,
                                           collapsible = FALSE,
                                           title = text,
                                           status = status, # primary, success, info, warning, danger
                                           solidHeader = TRUE,
                                           footer = footer,
                                           content_text,
                                           style='width:100%;overflow-x: scroll;',
                                           shiny::tableOutput(label_table))
    all_return[[4]] <- label_table
    all_return[[5]] <- NULL
  } else if (!is.null(spreadsheet$table_manip) && !is.na(spreadsheet$table_manip) && (spreadsheet$table_manip == "none")){
    all_return[[1]] <- shinydashboard::box(width=NULL,
                                           collapsible = FALSE,
                                           title = text,
                                           status = status, # primary, success, info, warning, danger
                                           solidHeader = TRUE,
                                           footer = footer,
                                           content_text,
                                           style='width:100%;overflow-x: scroll;',
                                           plotly::plotlyOutput(outputId = label_plot, height = "240"))
    all_return[[4]] <- NULL
    all_return[[5]] <- label_plot
  } else {
    all_return[[1]] <- shinydashboard::box(width=NULL,
                                           collapsible = FALSE,
                                           title = text,
                                           status = status, # primary, success, info, warning, danger
                                           solidHeader = TRUE,
                                           footer = footer,
                                           content_text,
                                           style='width:100%;overflow-x: scroll;',
                                           plotly::plotlyOutput(outputId = label_plot, height = "240"),
                                           shiny::tableOutput(label_table))
    all_return[[4]] <- label_table
    all_return[[5]] <- label_plot
  }
  
  all_return[[2]] <- "A"
  all_return[[3]] <- "B"
  all_return[[6]] <- unique_ID
  names(all_return) <- c("gui_obj", "table_obj", "plot_obj", "label_table", "label_plot", "ID")
  return(all_return)
}


