#' Plot Summaries from PLH Data
#' 
#' This function generates either a histogram or boxplot for a specified variable in PLH data. 
#' It allows for customisation of axis labels and supports naming conventions.
#'
#' @param data A data frame to plot.
#' @param columns_to_summarise The variable to plot.
#' @param naming_convention Logical. Default `TRUE`. Whether to apply naming conventions to axis labels.
#' @param replace A string indicating values to remove from the `columns_to_summarise` variable (before the value to keep).
#' @param replace_after A string indicating values to remove from the `columns_to_summarise` variable (after the value to keep).
#' @param plot_type A character string indicating whether to create a `histogram` or `boxplot`.
#'
#' @return A `ggplot` object representing the plot.
summary_plot <- function(data = plhdata_org_clean, columns_to_summarise, naming_convention = TRUE, replace = "rp.contact.field.",
                         replace_after = NULL,
                         plot_type = c("histogram", "boxplot")) {	
  plot_type <- match.arg(plot_type)
  x_axis_label = naming_conventions(colnames(data %>% dplyr::select(tidyr::all_of(columns_to_summarise))), replace = replace, replace_after = replace_after)	
  
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