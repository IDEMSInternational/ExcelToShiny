#' Creating box to be used in `PLH_shiny` function
#'
#'
#' @return Box for use in `Shiny`
#' @export
bar_table <- function(data, variable){
  all_return <- NULL
  plot_to_return <- ggplot2::ggplot()
  table_to_return <- summary_table(data = data,
                                   factors = .data[[variable]],
                                   include_margins = TRUE,
                                   replace = NULL)
  plot_to_return <- plot_to_return +
    ggplot2::geom_histogram(data = data, ggplot2::aes(x = .data[[variable]]), stat = "count")  +
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
    ggplot2::labs(y = "Count", x = naming_conventions(variable))
  all_return[[1]] <- table_to_return
  all_return[[2]] <- plot_to_return
  return(all_return)
}
