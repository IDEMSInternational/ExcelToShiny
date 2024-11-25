#' Create a Plot and Table Based on Data Manipulation Instructions
#'
#' This function generates a plot and a table from a dataset based on manipulation instructions provided in a `spreadsheet` object. 
#' The function supports optional grouping and allows for dynamic data manipulation and plot creation.
#'
#' @param data A dataset to manipulate and plot. Can be a list or a data frame.
#' @param spreadsheet An object containing data manipulation and graph manipulation instructions.
#' @param grouped_vars Optional. Variables to group the data by, provided as a character vector.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{`table`}{A table generated from the data based on the `spreadsheet$table_manip` instructions.}
#'   \item{`plot`}{A `ggplot` object created based on the `spreadsheet$graph_manip` instructions.}
#' }
specify_plot <- function(data, spreadsheet, grouped_vars = NULL) {
  all_return <- list(table = NULL, plot = NULL)

  if (any(class(data) %in% "list")) {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  if (!is.null(grouped_vars)) grouped_vars <- NULL

  if (!is.null(grouped_vars)){
    group_cmd <- paste0("%>% group_by(", grouped_vars, ")")
  } else {
    group_cmd <- paste0("")
  }
  
  # Table manipulation
  if (!is.null(spreadsheet$table_manip) && !is.na(spreadsheet$table_manip)) {
    
    # Command string from the spreadsheet
    command_string <- spreadsheet$table_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    #command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading %>% if present
    #command_string <- paste0("data %>%", command_string)       # Append the dataset reference
    
    if (startsWith(trimws(spreadsheet$table_manip), "%>%")){
      all_return$table <- eval(parse(text = paste0("data ", group_cmd, command_string)))
    } else{
      all_return$table <- eval(parse(text = command_string))      
    }
  } else {
    all_return$table <- "No Table Given"
  }
  
  # Data manipulation for data in graphs:
  if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$data_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    #command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading %>% if present
    #command_string <- paste0("data %>%", command_string)       # Append the dataset reference

    # Construct the full command
    if (startsWith(trimws(spreadsheet$data_manip), "%>%")){
      full_command <- paste0("data ", group_cmd, command_string)
    } else{
      full_command <- paste0(group_cmd, command_string)
    }
    
    # Evaluate the command
    data <- tryCatch({
      eval(parse(text = full_command))
    }, error = function(e) {
      message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
      data  # Return NULL or handle the error as appropriate
    })
  }
  
  # Plot it
  plot_obj <- ggplot2::ggplot(data)
  
  # Manipulations in the graphic
  add_string <- spreadsheet$graph_manip
  
  # Clean and construct the full command
  # Remove extra whitespace or control characters from command_string
  add_string <- gsub("\r\n", "\n", add_string)  # Replace CRLF with LF for consistency
  add_string <- gsub("^+\\s*", "", add_string) # Remove leading + if present

  environment(plot_obj$mapping) <- environment()
  
  if (!is.null(grouped_vars)){
    plot_command <- paste0("plot_obj + ", add_string, " + facet_wrap(vars(", grouped_vars, "))")
  } else {
    plot_command <- paste0("plot_obj + ", add_string)
  }
  plot_obj <- tryCatch({
    eval(parse(text = plot_command))
  }, error = function(e) {
    message("Error in evaluating graph manipulation code: ", e$message)
    plot_obj  # Return the original plot object in case of an error
  })
  
  all_return$plot <- plot_obj
  return(all_return)
}