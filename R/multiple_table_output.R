#' Return a list of summary tables
#'
#' @param data Data frame to calculate summaries from.
#' @param columns_to_summarise Variables to summarise.
#' @param replace String of which values to remove before the final string.
#' @param replace_after String of which values to remove after the final string.
#' @param summaries Whether `frequencies` or `mean` summaries are calculated.
#' @param na.rm logical. Default `TRUE`. If `summaries = "mean"`, whether to include `NA` values.
#'
#' @return List of summary tables.
#' @export
#'
#' @examples #TODO
multiple_table_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.", replace_after = NULL, summaries = c("frequencies", "mean"), na.rm = TRUE){
  summaries <- match.arg(summaries)
  
  # run: add_na_variable here with warning 
  data <- add_na_variable(data = data, variable = columns_to_summarise)
  
  variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
  summary_table_values <- data %>%
    purrr::map(.x = columns_to_summarise, .f = ~tidyr::replace_na(.x, "unknown"))  %>%
    purrr::map(.x = columns_to_summarise, .f = ~summary_table(columns_to_summarise = .x,
                                                              display = FALSE,
                                                              include_margins = TRUE,
                                                              summaries = summaries,
                                                              na.rm = na.rm))
  
  names(summary_table_values) <- variable_display_names
  return(summary_table_values)
}
