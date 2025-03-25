#' Generate Multiple Summary Tables
#'
#' This function generates a list of summary tables based on the provided data and columns to summarise.
#' It supports calculating either frequencies or means for each specified variable.
#'
#' @param data Data frame to calculate summaries from. Default is `plhdata_org_clean`.
#' @param columns_to_summarise A vector of variable names (as strings) to summarise.
#' @param replace A string indicating the pattern to remove from the variable names before displaying them. Default is `"rp.contact.field."`.
#' @param replace_after A string indicating the pattern to remove after the variable names. Default is `NULL`.
#' @param summaries A string indicating whether to calculate `"frequencies"` or `"mean"` summaries. Default is `c("frequencies", "mean")`.
#' @param na.rm Logical indicating whether to remove `NA` values when calculating means. Default is `TRUE`.
#'
#' @return A list of summary tables, where each table corresponds to the summary of the respective variable.
multiple_table_output <- function(data = plhdata_org_clean, columns_to_summarise, replace = "rp.contact.field.", replace_after = NULL, summaries = c("frequencies", "mean"), na.rm = TRUE){
#   summaries <- match.arg(summaries)
#   
#   # run: add_na_variable here with warning 
#   data <- add_na_variable(data = data, variable = columns_to_summarise)
#   
#   variable_display_names <- naming_conventions(columns_to_summarise, replace = replace, replace_after = replace_after)
#   summary_table_values <- data %>%
#     purrr::map(.x = columns_to_summarise, .f = ~tidyr::replace_na(.x, "unknown"))  %>%
#     purrr::map(.x = columns_to_summarise, .f = ~summary_table(columns_to_summarise = .x,
#                                                               display = FALSE,
#                                                               include_margins = TRUE,
#                                                               summaries = summaries,
#                                                               na.rm = na.rm))
#   
#   names(summary_table_values) <- variable_display_names
#   return(summary_table_values)
}