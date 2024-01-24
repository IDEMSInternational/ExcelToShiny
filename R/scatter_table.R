#' Creating box to be used in `PLH_shiny` function
#'
#' @return table for use in `Shiny`
#' @export
scatter_table <- function(data, variable, type = c("freq", "summary"), spreadsheet) {
  type <- match.arg(type)
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
  
  #spreadsheet <- data_l$engagement %>% filter(name == "box10")
  variable <- strsplit(spreadsheet$variable, ",")[[1]]
  variable <- trimws(variable)
  # Refactor the histogram plotting into a separate function
  create_scatter_plot <- function(data, variable) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable[1]]], y = .data[[variable[2]]])) +
      ggplot2::geom_point() +
      ggplot2::labs(x = naming_conventions(variable[1]), y = naming_conventions(variable[2]))
  }
  
  # Extract these variables from the data frame
  all_return$table <- data.frame(Correlation = cor(data[[variable[1]]], data[[variable[2]]], use = "pairwise.complete.obs"))

  plot_obj <- create_scatter_plot(data, variable)
  if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
    plot_command <- paste0("plot_obj + ", spreadsheet$graph_manip)
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