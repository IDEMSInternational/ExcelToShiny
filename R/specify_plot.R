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
  all_return$table <- ""
  all_return$plot <- plot_obj
  
  return(all_return)
}