#' Creating box to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param text Variable containing titles for each box.
#' @param colour Variable containing colours for each box.
#' @param label_table Variable containing the table label for each box.
#' @param label_plot Variable containing the plot label for each box.
#' @param variable Variable containing the variable for each box.
#'
#' @return Box for use in `Shiny`
#' @export
box_function <- function(data_frame, text, colour, label_table, label_plot, variable){
  
  all_return <- NULL
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
  all_return[[1]] <- shinydashboard::box(width=NULL,
                         collapsible = FALSE,
                         title = text,
                         status = status, # primary, success, info, warning, danger
                         solidHeader = TRUE,
                         plotly::plotlyOutput(outputId = label_plot, height = "240"),
                         shiny::tableOutput(label_table))
  
  plot_to_return <- ggplot2::ggplot()
  if (is.numeric(data_frame[[variable]])){
    plot_to_return <- plot_to_return +
      ggplot2::geom_boxplot(data = data_frame, ggplot2::aes(y = .data[[variable]])) +
      ggplot2::labs(x = "Count")
    table_to_return <- data_frame %>%
      dplyr::summarise(Mean = round(mean(data_frame[[variable]], na.rm = TRUE), 2),
                SD = round(stats::sd(data_frame[[variable]], na.rm = TRUE), 2))
  } else {
    table_to_return <- summary_table(data = data_frame,
                                     factors = .data[[variable]],
                                     include_margins = TRUE,
                                     replace = NULL)
    plot_to_return <- plot_to_return +
      ggplot2::geom_histogram(data = data_frame, ggplot2::aes(x = .data[[variable]]), stat = "count")  +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      ggplot2::labs(y = "Count", x = naming_conventions(variable))
  }
  all_return[[2]] <-  table_to_return
  all_return[[3]] <- plot_to_return
  all_return[[4]] <- label_table
  all_return[[5]] <- label_plot
  names(all_return) <- c("gui_obj", "table_obj", "plot_obj", "label_table", "label_plot")
  return(all_return)
}