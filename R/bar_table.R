#' Creating box to be used in `PLH_shiny` function
#'
#' @return Box for use in `Shiny`
#' @export
bar_table <- function(data, variable, type = c("freq", "summary"), spreadsheet) {
  type <- match.arg(type)
  all_return <- list(table = NULL, plot = NULL)
  
  if (class(data) == "list") {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  # Refactor the histogram plotting into a separate function
  create_histogram_plot <- function(data, variable) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
      ggplot2::geom_histogram(stat = "count") +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      ggplot2::labs(y = "Count", x = naming_conventions(variable))
  }
  
  if (type == "freq") {
    all_return$table <- summary_table(data = data, factors = .data[[variable]], include_margins = FALSE)
    plot_obj <- create_histogram_plot(data, variable)
    if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
      plot_obj <- tryCatch({
        plot_obj + eval(parse(text = spreadsheet$graph_manip))
      }, error = function(e) {
        message("Error in evaluating graph manipulation code: ", e$message)
        plot_obj  # Return the original plot object in case of an error
      })
    }
    all_return$plot <- plot_obj
  } else {
    table_data <- dplyr::filter(data, !is.na(data[[variable]]))
    all_return$table <- table_data %>%
      dplyr::summarise(Median = round(median(table_data[[variable]], na.rm = TRUE), 2),
                       SD = round(stats::sd(table_data[[variable]], na.rm = TRUE), 2),
                       N = length(table_data[[variable]]))
    all_return$plot <- create_histogram_plot(data, variable)
  }
  
  return(all_return)
}