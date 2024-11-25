#' Create Scatter Plot Table for Shiny
#'
#' The `scatter_table` function generates a table and scatter plot based on the specified variables from the dataset. This function is intended to be used within a Shiny application to create interactive visualisations and summaries.
#'
#' @param data A data frame or list used as the data source. If a list is provided, the function returns the table and an empty plot.
#' @param variable A string specifying the variable(s) to be used for the scatter plot. The variable should be in a format suitable for plotting (e.g., two numeric columns for X and Y axes).
#' @param type A string indicating the type of table to generate. Options are `"freq"` for frequency tables or `"summary"` for summary statistics. Default is `"freq"`.
#' @param spreadsheet A data frame containing metadata or additional manipulation instructions for creating the plot.
#' @param grouped_vars Optional. A string or vector of strings specifying variables by which the data should be grouped before generating the plot or table.
#'
#' @return A list with two elements: `table` containing the summary or frequency table, and `plot`, a ggplot2 object representing the scatter plot.
scatter_table <- function(data, variable, type = c("freq", "summary"), spreadsheet, grouped_vars = NULL) {
  type <- match.arg(type)
  all_return <- list(table = NULL, plot = NULL)

  if (class(data) == "list") {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  if (!is.null(grouped_vars) && (grouped_vars %in% variable)) grouped_vars <- NULL
  
  # Check if data manipulation command is not null or NA
  if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$data_manip
    
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading %>% if present
    command_string <- paste0("data %>%", command_string)       # Append the dataset reference
    
    # Construct the full command
    if (!is.null(grouped_vars)){
      full_command <- paste0("data %>% dplyr::group_by(", grouped_vars, ")", command_string)
    } else {
      full_command <- paste0("data ", command_string)
    }
    
    # Evaluate the command
    data <- tryCatch({
      eval(parse(text = full_command))
    }, error = function(e) {
      message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
      data  # Return NULL or handle the error as appropriate
    })
  }
  
  #spreadsheet <- data_l$engagement %>% filter(name == "box10")
  variable <- strsplit(spreadsheet$variable, ",")[[1]]
  variable <- trimws(variable)
  # Refactor the histogram plotting into a separate function
  create_scatter_plot <- function(data, variable, grouped_vars) {
    return_plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable[1]]], y = .data[[variable[2]]])) +
      ggplot2::geom_point() +
      ggplot2::labs(x = naming_conventions(variable[1]), y = naming_conventions(variable[2]))
    
    if (!is.null(grouped_vars)){
      return_plot <- return_plot + ggplot2::facet_wrap(grouped_vars)
    }
    return(return_plot)
  }
  
  # Extract these variables from the data frame
  # group by?
  
  if (!is.null(grouped_vars)){
    table_to_return <- data %>% dplyr::group_by(!!rlang::sym(group))
  } else {
    table_to_return <- data
  }
  all_return$table <- data.frame(table_to_return %>% dplyr::summarise(Correlation = stats::cor(!!rlang::sym(variable[1]), !!rlang::sym(variable[2]), use = "pairwise.complete.obs")))
  
  plot_obj <- create_scatter_plot(data, variable)
  if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
    command_string <- spreadsheet$graph_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    command_string <- gsub("^+\\s*", "", command_string) # Remove leading %>% if present
    plot_command <- paste0("plot_obj + ", command_string)
    
    plot_obj <- tryCatch({
      eval(parse(text = plot_command))
    }, error = function(e) {
      message("Error in evaluating graph manipulation code: ", e$message)
      plot_obj  # Return the original plot object in case of an error
    })
  }
  all_return$plot <- plot_obj
  return(all_return)
}