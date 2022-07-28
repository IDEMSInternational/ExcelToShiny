#' Plot PLH data
#' @description Plot either a histogram or boxplot for PLH data.
#'
#' @param data Data frame to calculate summaries from.
#' @param columns_to_summarise Variables to dplyr::summarise.
#' @param naming_convention logical. Default `TRUE`. Whether to apply naming conventions.
#' @param replace String of values in the `columns_to_summarise` variable to remove in the table (before the value to keep).
#' @param replace_after String of values in the `columns_to_summarise` variable to remove in the table (after the value to keep).
#' @param plot_type Whether to display a `histogram` or `boxplot`
#'
#' @return `ggplot` object
#' @export
#'
#' @examples # TODO
summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data %>% dplyr::select(.data[[columns_to_summarise]])), replace = replace, replace_after = replace_after)	
  
  return_plot <- ggplot2::ggplot(data) +	
    viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +	
    ggplot2::labs(x = x_axis_label, y = "Count") +	
    ggplot2::theme_classic()	
  
  if(plot_type == "histogram"){
    return_plot <- return_plot + ggplot2::geom_histogram(data = data, ggplot2::aes(x = .data[[columns_to_summarise]]), stat = "count")
  } else {
    return_plot <- return_plot + ggplot2::geom_boxplot(data = data, ggplot2::aes(y = .data[[columns_to_summarise]]))
  }
  
  return(return_plot)	
}