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
server_box_function <- function(data_frame, spreadsheet, unique_ID, list_of_reactives){
  all_return <- NULL
  spreadsheet <- spreadsheet %>% dplyr::filter(name == unique_ID)
  
  # get data frame
  if (is.null(spreadsheet$data)) { 
    data_frame_read <- data_frame
  } else {
    data_frame_read <- list_of_reactives[[spreadsheet$data]]()
  }
  
  # we repeat for each row later in the plh_shiny function
  # for now, just get the data
  #spreadsheet <- testing_shiny
  variable <- spreadsheet$variable
  if (!variable %in% names(data_frame_read)) stop(paste0(variable, " not in data."))
  
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
  if (value == "bar_table"){
    return_object <- bar_table(data = data_frame_read, variable = variable)
  } else if (value == "boxplot_table"){
    return_object <- boxplot_table(data = data_frame_read, variable = variable)
  } else if (value == "bar_freq"){
    return_object <- bar_table(data = data_frame_read, variable = variable)
  } else if (value == "bar_summary"){
    return_object <- bar_table(data = data_frame_read, variable = variable, type = "summary")
  } else if (value == "boxplot_freq"){
    return_object <- boxplot_table(data = data_frame_read, variable = variable, type = "freq")
  } else if (value == "boxplot_summary"){
    return_object <- boxplot_table(data = data_frame_read, variable = variable, type = "summary")
  }
  all_return[[1]] <- return_object[[1]]
  all_return[[2]] <- return_object[[2]]
  names(all_return) <- c("table_obj", "plot_obj")
  return(all_return)
}
