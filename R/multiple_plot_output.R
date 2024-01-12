#' Return a list of ggplot2 plots
#'
#' @param data Data frame to calculate summaries from.
#' @param columns_to_summarise Variables to summarise.
#' @param replace String of which values to remove before the final string.
#' @param replace_after String of which values to remove after the final string.
#' @param plot_type Whether to display a `histogram` or `boxplot`
#'
#' @return List of ggplot2 objects.
#' @export
#'
#' @examples # TODO
multiple_plot_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.",
                                 replace_after = NULL, plot_type = c("histogram", "boxplot")){
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_plot_values <- data %>%
    purrr::map(.x = columns_to_summarise,
               .f = ~summary_plot(columns_to_summarise = .x, plot_type = plot_type, replace = replace, replace_after = replace_after))
  
  names(summary_plot_values) <- variable_display_names
  return(summary_plot_values)
}
