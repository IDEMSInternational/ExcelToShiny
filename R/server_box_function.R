#' Server Box Function
#'
#' This function processes and generates box plots and frequency tables for a Shiny application server. It reads data from a data frame and applies specified filters and operations based on spreadsheet data.
#'
#' @param data_frame The data frame used for generating box plots and frequency tables.
#' @param spreadsheet A data frame containing spreadsheet data that defines the operations and filters to be applied.
#' @param unique_ID An identifier for the current operation.
#'
#' @return A list containing table and plot objects resulting from the processing of the spreadsheet data.
#'
#' @export
#'
#' @examples
#' # Process spreadsheet data using server_box_function
#' data_frame <- data.frame(...)  # Define your data frame
#' spreadsheet_data <- data.frame(...)  # Define your spreadsheet data
#' unique_ID <- "boxplot_variable1"
#' result <- server_box_function(data_frame, spreadsheet_data, unique_ID)
#'
#' # Access table and plot objects from the result
#' table_obj <- result$table_obj
#' plot_obj <- result$plot_obj
server_box_function <- function(data_frame, spreadsheet, unique_ID, list_of_reactives) {
  # Initial Checks for validity of input parameters
  if (!exists("data_frame") || !exists("spreadsheet") || is.null(unique_ID) || !exists("list_of_reactives")) {
    stop("Invalid input parameters")
  }
  
  # Filter spreadsheet data once at the start
  filtered_spreadsheet <- dplyr::filter(spreadsheet, name == unique_ID)
  
  # Check if the spreadsheet data is empty after filtering
  if (nrow(filtered_spreadsheet) == 0) {
    warning("No data found for the provided unique_ID")
    return(NULL)
  }
  
  # get data frame
  if (is.null(filtered_spreadsheet$data)){
    data_frame_read <- data_frame
  } else {
    data_frame_read <- list_of_reactives[[filtered_spreadsheet$data]]()
  }
  
  # Check if variable exists in data_frame_read
  variable <- filtered_spreadsheet$variable
  # if (!variable %in% names(data_frame_read)) {
  #   stop(paste0(variable, " not in data."))
  # }
  
  value <- spreadsheet$value
  variable <- spreadsheet$variable
  filter_value <- spreadsheet$filter_value
  filter_variable <- spreadsheet$filter_variable
  if (!is.null(spreadsheet$filter_variable)){
    if (!is.na(spreadsheet$filter_variable)){
      if (!is.na(spreadsheet$filter_value)){
        data_frame_read <- data_frame_read %>% filter(get(filter_variable) %in% filter_value)
      } else {
        warning("NA given for filter_value. Filtering to NA values.")
        data_frame_read <- data_frame_read %>% filter(is.na(get(filter_variable)))
      }
    }
  }
  
  # Refactor repeated code using a mapping strategy
  value_function_map <- list(
    bar_table = function() bar_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet),
    boxplot_table = function() boxplot_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet),
    bar_freq = function() bar_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet),
    bar_summary = function() bar_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet),
    boxplot_freq = function() boxplot_table(data = data_frame_read, variable = variable, type = "freq", spreadsheet = filtered_spreadsheet),
    boxplot_summary = function() boxplot_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet),
    scatter_summary = function() scatter_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet),
    specify_plot = function() specify_plot(data = data_frame_read, spreadsheet = filtered_spreadsheet)
  )
  
  # Execute the appropriate function based on the 'value'
  value <- filtered_spreadsheet$value
  if (!value %in% names(value_function_map)) {
    stop("Invalid value type.")
  }
  return_object <- value_function_map[[value]]()
  
  # Initialize all_return with named elements
  all_return <- list(table_obj = NULL, plot_obj = NULL)
  all_return$table_obj <- return_object[[1]]
  all_return$plot_obj <- return_object[[2]]
  return(all_return)
}