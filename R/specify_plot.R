#' Creating box to be used in `PLH_shiny` function
#'
#' @return Box for use in `Shiny`
#' @export
specify_plot <- function(data, spreadsheet) {
  all_return <- list(table = NULL, plot = NULL)
  
  if (class(data) == "list") {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  # Check if data manipulation command is not null or NA
  if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$data_manip
    
    # Construct the full command
    full_command <- paste0("data ", command_string)
    
    # Evaluate the command
    data <- tryCatch({
      eval(parse(text = full_command))
    }, error = function(e) {
      message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
      data  # Return NULL or handle the error as appropriate
    })
  }
  
  # Refactor the histogram plotting into a separate function
  create_plot <- function(data) {
    ggplot2::ggplot(data)
  }
  
  plot_obj <- create_plot(data)

  # Your string to be added
  add_string <- spreadsheet$graph_manip
  
  # Set the environment of plot_obj to the current environment
  environment(plot_obj$mapping) <- environment()
  
  # TODO: currently doesn't work if you give a variable
  # Parse and evaluate the string
  plot_obj <- tryCatch({
    eval(parse(text = paste0("plot_obj + ", add_string)))
  }, error = function(e) {
    message("Error in evaluating graph manipulation code: ", e$message)
    plot_obj  # Return the original plot object in case of an error
  })
  all_return$table <- "A"
  all_return$plot <- plot_obj
  
  return(all_return)
}