#' Creating box to be used in `PLH_shiny` function
#'
#' @return Box for use in `Shiny`
#' @export
bar_table <- function(data, variable, type = c("freq", "summary")){
  type <- match.arg(type)
  all_return <- NULL
  plot_to_return <- ggplot2::ggplot()
  if (class(data) == "list"){
    all_return[[1]] <- data[[variable]]
    all_return[[2]] <- plot_to_return
  } else {
    all_return <- NULL
    
    if (type == "freq"){
      table_to_return <- summary_table(data = data,
                                       factors = .data[[variable]],
                                       include_margins = TRUE,
                                       replace = NULL)
    } else {
      table_to_return <- data %>%
        dplyr::filter(!is.na(data[[variable]]))
      table_to_return <- table_to_return %>%
        dplyr::summarise(Mean = round(mean(table_to_return[[variable]], na.rm = TRUE), 2),
                         SD = round(stats::sd(table_to_return[[variable]], na.rm = TRUE), 2),
                         N = length(table_to_return[[variable]]))
    }

    plot_to_return <- plot_to_return +
      ggplot2::geom_histogram(data = data, ggplot2::aes(x = .data[[variable]]), stat = "count")  +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      ggplot2::labs(y = "Count", x = naming_conventions(variable))
    all_return[[1]] <- table_to_return
    all_return[[2]] <- plot_to_return
  }
  return(all_return)
}
