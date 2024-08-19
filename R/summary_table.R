#' Create and Format Summary Tables from PLH Data
#'
#' This function calculates and formats summary tables from PLH data, supporting both frequency and mean summaries. 
#' It can output the table in a wide format, apply naming conventions, and optionally display the table using `gt`.
#'
#' @param data A data frame to summarise.
#' @param factors A list of factors to group by.
#' @param columns_to_summarise Variables to summarise.
#' @param summaries A character string indicating whether to calculate `frequencies` or `mean` summaries.
#' @param replace A string of values in the `columns_to_summarise` variable to remove in the table (before the value to keep).
#' @param include_margins Logical. Default `FALSE`. Whether to include margins in the summary.
#' @param wider_table Logical. Default `TRUE`. Whether to present a wider table if `summaries = "frequencies"`.
#' @param display_table Logical. Default `FALSE`. Whether to return the table in `gt::gt()` format.
#' @param naming_convention Logical. Default `TRUE`. Whether to apply naming conventions when `summaries = "mean"`.
#' @param include_percentages Logical. Default `FALSE`. Whether to include percentages when `summaries = "frequencies"`.
#' @param together Logical. Default `FALSE`. If `summaries = "frequencies"`, whether to combine the count and percentage into one cell.
#' @param drop Logical. Default `FALSE`. When running `group_by`, whether to drop unused columns.
#' 
#' @return A summary table as a `tibble` or a `gt` table, depending on the `display_table` parameter.
summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                          replace = "rp.contact.field.", include_margins = FALSE, wider_table = TRUE,
                          display_table = FALSE, naming_convention = TRUE, include_percentages = FALSE,
                          together = TRUE, drop = FALSE){
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      summaries = summaries,
                                      together = together,
                                      drop = drop)
  return_table_names <- naming_conventions(colnames(return_table), replace = replace)
  if (summaries == "mean"){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
    }
  }
  if (display_table){
    if (summaries == "frequencies"){
      return_table <- return_table %>% tidyr::pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n)
    }
    
    return_table <- gt::gt(tibble::as_tibble(return_table)) %>%
      gt::tab_header(
        title = paste(return_table_names[1], "by", return_table_names[2])  # fix up. 
      ) %>%
      gt::tab_style(locations = list(gt::cells_body(columns = 1)),
                style = list(gt::cell_borders(
                  sides = "right",
                  color = "black",
                  weight = gt::px(2)),
                  gt::cell_text(weight = "bold"))) %>%
      gt::tab_style(locations = list(gt::cells_column_labels(columns = gt::everything())),
                style = list(gt::cell_borders( 
                  sides = "bottom",
                  color = "black",
                  weight = gt::px(2)),
                  gt::cell_text(weight = "bold")))
    #if (summaries == "mean"){
    #  names(return_table$`_data`) <- naming_conventions(names(return_table$`_data`), replace = replace)
    #}
  } else {
    if (summaries == "frequencies"){
      all_factors <- stringr::str_split(gsub("^c\\(|\\)$", "", deparse(substitute(factors))), pattern = ", ")
      all_columns_to_summarise <- stringr::str_split(gsub("^c\\(|\\)$", "", deparse(substitute(columns_to_summarise))), pattern = ", ")
      if (wider_table && !missing(columns_to_summarise) && (any(all_factors[[1]] %in% (all_columns_to_summarise)[[1]]) == FALSE)){
        if (together){
          values_from <- "Count (%)"
        } else {
          values_from <- "n"
        }
        return_table <- return_table %>% tidyr::pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = values_from, names_prefix = "")
      }
      if (naming_convention){
        colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace)
      }
    }
  }
  if ("Total" %in% colnames(return_table)){
    return_table <- return_table %>%
      dplyr::relocate(Total, .after = dplyr::last_col())
  }
  return(return_table)
}
