#' Calculate and format summaries from PLH data
#' 
#' @description Calculate and format summaries table
#'
#' @param data Data frame to calculate summaries from.
#' @param factors List of factors to group by.
#' @param columns_to_summarise Variables to dplyr::summarise.
#' @param summaries Whether `frequencies` or `mean` summaries are calculated.
#' @param include_margins logical. Default `FALSE`. Whether to include margins.
#' @param include_country_margins logical. Whether to include the country margins.
#' @param country_factor If `include_country_margins = TRUE`, the factor variable for the country.
#' @param include_percentages logical. Default `FALSE`. Whether to include percentages when `summaries = "frequencies"`.
#' @param together logical. Default `FALSE`. If `summaries = "frequencies"`, whether to combine the count and percentage into one cell.
#' @param na.rm logical. Default `TRUE`. If `summaries = "mean"`, whether to include `NA` values.
#' @param wider_table logical. Default `TRUE`. Whether to have a wider table if `summaries = "frequencies"`.
#' @param display_table logical. Default `FALSE`. Return table in `gt::gt()` table format.
#' @param naming_convention logical. Default `TRUE`. Whether to apply naming conventions when `summaries = "mean"`.
#' @param replace String of values in the `columns_to_summarise` variable to remove in the table (before the value to keep).
#' @param replace_after String of values in the `columns_to_summarise` variable to remove in the table (after the value to keep).
#'
#' @return Summaries table as `tibble` or `gt`.
#' @export
#' @importFrom rlang .data
#'
#' @examples # TODO
summary_table <- function(data = plhdata_org_clean, factors = Org, columns_to_summarise, summaries = c("frequencies", "mean"),
                          include_margins = FALSE, include_country_margins = TRUE, country_factor = "country", 
                          include_percentages = FALSE, together = TRUE, na.rm = TRUE, wider_table = TRUE, 
                          display_table = FALSE, naming_convention = TRUE, replace = "rp.contact.field.", replace_after = NULL){
  
  summaries <- match.arg(summaries)
  
  return_table <- summary_calculation(data = data,
                                      factors = c({{ factors }}),
                                      columns_to_summarise = c({{ columns_to_summarise }}),
                                      include_margins = include_margins,
                                      include_country_margins = include_country_margins,
                                      country_factor = country_factor,
                                      summaries = summaries,
                                      together = together,
                                      na.rm = na.rm)
  
  return_table_names <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
  if (summaries == "mean"){
    if (naming_convention){
      colnames(return_table) <- naming_conventions(colnames(return_table), replace = replace, replace_after = replace_after)
    }
  }
  if (display_table){
    if (summaries == "frequencies"){
      return_table <- return_table %>% tidyr::pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = dplyr::n)
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
    if (wider_table & summaries == "frequencies"){
      return_table <- return_table %>% tidyr::pivot_wider(id_cols = {{ factors }}, names_from =  {{ columns_to_summarise }}, values_from = n, names_prefix = "")
    }
  }
  return(return_table)
}