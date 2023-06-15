#' Creating boxplot to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier.
#' @param label_table ID for the table.
#' @param label_plot ID for the plot.
#'
#' @return Box for use in `Shiny`
#' @export
boxplot_table <- function(data, variable){
  all_return <- NULL
  plot_to_return <- ggplot2::ggplot()
    plot_to_return <- plot_to_return +
      ggplot2::geom_boxplot(data = data, ggplot2::aes(y = .data[[variable]])) +
      ggplot2::labs(x = "Count")
    table_to_return <- data %>%
      dplyr::summarise(Mean = round(mean(data[[variable]], na.rm = TRUE), 2),
                       SD = round(stats::sd(data[[variable]], na.rm = TRUE), 2))
    all_return[[1]] <- table_to_return
    all_return[[2]] <- plot_to_return
  return(all_return)
}
