#' Calculate and format summaries from PLH data
#' 
#' @description Calculate and format summaries table
#'
#' @param data Data frame to calculate summaries from.
#' @param factors List of factors to group by.
#' @param columns_to_summarise Variables to summarise.
#' @param summaries Whether `frequencies` or `mean` summaries are calculated.
#' @param replace String of values in the `columns_to_summarise` variable to remove in the table (before the value to keep).
#' @param include_margins logical. Default `FALSE`. Whether to include margins.
#' @param wider_table logical. Default `TRUE`. Whether to have a wider table if `summaries = "frequencies"`.
#' @param display_table logical. Default `FALSE`. Return table in `gt::gt()` table format.
#' @param naming_convention logical. Default `TRUE`. Whether to apply naming conventions when `summaries = "mean"`.
#' @param include_percentages logical. Default `FALSE`. Whether to include percentages when `summaries = "frequencies"`.
#' @param together logical. Default `FALSE`. If `summaries = "frequencies"`, whether to combine the count and percentage into one cell.
#' @param drop logical. Default `FALSE`. When running `group_by`, whether to drop unused columns.
#'
#' @return Summaries table as `tibble` or `gt`.
#' @export
#' @importFrom rlang .data
#'
#' @examples # TODO
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