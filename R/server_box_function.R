#' Server Box Function
#'
#' This function processes and generates tables and plots based on specified parameters. It is used within a Shiny application to dynamically create visual and tabular content.
#'
#' @param data_frame The data frame containing the data to be visualised or summarised.
#' @param spreadsheet A data frame specifying the operations, filters, and configurations to be applied.
#' @param unique_ID A string that uniquely identifies the operation or plot type being executed.
#' @param list_of_reactives A list of reactive data frames used to dynamically update content in a Shiny app.
#'
#' @return A list containing `table_obj`, a data frame or table of results, and `plot_obj`, a ggplot2 object representing the plot.
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
  
  # if we have grouping in our main df, we should use these as grouping variables in our other dfs.
  grouped_vars <- dplyr::group_vars(data_frame)   # these need to be read in as factors in our plots and tables
  
  # if it doesn't exist, or it does exist and is NA
  # doing this way to account for partial matching with "data_manip"
  if (is.na(match("data", names(filtered_spreadsheet))) || 
      (!is.na(match("data", names(filtered_spreadsheet))) && is.na(filtered_spreadsheet %>% dplyr::pull("data"))) ){
    data_frame_read <- data_frame
  } else {
    data_frame_read <- list_of_reactives[[filtered_spreadsheet$data]]()
  }
  data_frame_read <- data_frame_read %>% dplyr::ungroup()
  
  # # group our new df.
  # if (length(grouped_vars) > 0){
  #   data_frame_read <- data_frame_read %>% group_by(!!sym(grouped_vars))
  # } else {
  #   data_frame_read <- data_frame_read %>% ungroup()
  # }
  # #print(head(data_frame_read))
  
  value <- filtered_spreadsheet$value
  variable <- filtered_spreadsheet$variable
  filter_value <- filtered_spreadsheet$filter_value
  filter_variable <- filtered_spreadsheet$filter_variable
  if (!is.null(filtered_spreadsheet$filter_variable)){
    if (!is.na(filtered_spreadsheet$filter_variable)){
      if (!is.na(filtered_spreadsheet$filter_value)){
        data_frame_read <- data_frame_read %>% dplyr::filter(get(filter_variable) %in% filter_value)
      } else {
        warning("NA given for filter_value. Filtering to NA values.")
        data_frame_read <- data_frame_read %>% dplyr::filter(is.na(get(filter_variable)))
      }
    }
  }
  
  if (length(grouped_vars) == 0) grouped_vars <- NULL
  # Refactor repeated code using a mapping strategy
  value_function_map <- list(
    bar_table = function() bar_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    boxplot_table = function() boxplot_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    bar_freq = function() bar_table(data = data_frame_read, variable = variable, spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    bar_summary = function() bar_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    boxplot_freq = function() boxplot_table(data = data_frame_read, variable = variable, type = "freq", spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    boxplot_summary = function() boxplot_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    scatter_summary = function() scatter_table(data = data_frame_read, variable = variable, type = "summary", spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars),
    specify_plot = function() specify_plot(data = data_frame_read, spreadsheet = filtered_spreadsheet, grouped_vars = grouped_vars)
  )
  
  # Execute the appropriate function based on the 'value'
  all_return <- list(table_obj = NULL, plot_obj = NULL)
  value <- filtered_spreadsheet$value
  if (!value %in% names(value_function_map)) {
    if (value %in% "data_frame"){
      all_return$table_obj <- data_frame_read
      all_return$plot_obj <- NULL
      return(all_return)
    } else {
      stop("Invalid value type.")
    }
  }
  return_object <- value_function_map[[value]]()
  
  # Initialise all_return with named elements
  all_return$table_obj <- return_object[[1]]
  all_return$plot_obj <- return_object[[2]]
  return(all_return)
}