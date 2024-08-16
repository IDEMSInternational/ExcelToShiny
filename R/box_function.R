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
  spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, fixed("\", "), simplify = TRUE))
  #spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, fixed(", "), simplify = TRUE))
  spreadsheet_parameters_names <- sub("\\= .*", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- gsub(".*= ", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- stringr::str_remove_all(spreadsheet_parameters_values, stringr::fixed("\""))
  values <- spreadsheet_parameters_values
  names <- spreadsheet_parameters_names
  spreadsheet_df <- data.frame(names, values)
  
  #repeat for all variables like text, etc. so make into a function?
  content_text <- trimws(spreadsheet_finder(data = spreadsheet_df, "content_text "))
  text <- trimws(spreadsheet_finder(data = spreadsheet_df, "text "))
  width <- trimws(spreadsheet_finder(data = spreadsheet_df, "width "))
  colour <- trimws(spreadsheet_finder(data = spreadsheet_df, "colour "))
  footer <- trimws(spreadsheet_finder(data = spreadsheet_df, "footer "))
  colour <- tolower(colour)
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
  
  if (!is.null(spreadsheet$table_manip) && is.na(spreadsheet$table_manip) && spreadsheet$value == "specify_plot"){
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
  }
  
  all_return[[2]] <- "A"
  all_return[[3]] <- "B"
  all_return[[5]] <- label_plot
  all_return[[6]] <- unique_ID
  names(all_return) <- c("gui_obj", "table_obj", "plot_obj", "label_table", "label_plot", "ID")
  
  return(all_return)
}

