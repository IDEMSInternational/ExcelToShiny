#' Generate Multiple ggplot2 Plots
#'
#' This function generates a list of ggplot2 plots based on the provided data and columns to summarize. 
#' It supports creating either histograms or boxplots for each specified variable.
#'
#' @param data Data frame to calculate summaries from. Default is `plhdata_org_clean`.
#' @param columns_to_summarise A vector of variable names (as strings) to summarize and plot.
#' @param replace A string indicating the pattern to remove from the variable names before displaying them. Default is `"rp.contact.field."`.
#' @param replace_after A string indicating the pattern to remove after the variable names. Default is `NULL`.
#' @param plot_type A string indicating the type of plot to generate, either `"histogram"` or `"boxplot"`. Default is `c("histogram", "boxplot")`.
#'
#' @return A list of ggplot2 objects, where each object is a plot for the corresponding variable.
multiple_plot_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.",
                                 replace_after = NULL, plot_type = c("histogram", "boxplot")){
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_plot_values <- data %>%
    purrr::map(.x = columns_to_summarise,
               .f = ~summary_plot(columns_to_summarise = .x, plot_type = plot_type, replace = replace, replace_after = replace_after))
  
  names(summary_plot_values) <- variable_display_names
  return(summary_plot_values)
}
