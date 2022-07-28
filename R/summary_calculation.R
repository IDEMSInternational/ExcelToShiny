#' Calculate summaries in PLH data
#'
#' @param data Data frame to calculate summaries from.
#' @param factors List of factors to group by.
#' @param columns_to_summarise Variables to dplyr::summarise.
#' @param summaries Whether `frequencies` or `mean` summaries are calculated.
#' @param include_country_margins logical. Whether to include the country margins.
#' @param country_factor If `include_country_margins = TRUE`, the factor variable for the country.
#' @param together logical. Default `FALSE`. If `summaries = "frequencies"`, whether to combine the count and percentage into one cell.
#' @param include_margins logical. Default `FALSE`. Whether to include margins.
#' @param na.rm logical. Default `TRUE`. If `summaries = "mean"`, whether to include `NA` values.
#'
#' @return Summaries table
#' @export
#'
#' @examples # TODO
summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise, summaries = c("frequencies", "mean"),
                                include_country_margins, country_factor, together = FALSE, include_margins = FALSE, na.rm = TRUE){
  
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      dplyr::group_by(dplyr::across(c({{ columns_to_summarise }}, {{ factors }})), .drop = FALSE) %>%
      dplyr::summarise(n = n())
    
    if (include_margins){
      cts_margin <- data %>%
        dplyr::group_by(dplyr::across(c({{ columns_to_summarise }})), .drop = FALSE) %>%
        dplyr::summarise(n = n())
      
      ftr_margin <- data %>%
        dplyr::group_by(dplyr::across(c({{ factors }})), .drop = FALSE) %>%
        dplyr::summarise(n = n())
      
      corner_margin <- data %>%
        dplyr::summarise(n = n())
      
      if (include_country_margins){
        summary_output_country <- data %>%
          dplyr::group_by(dplyr::across(c({{ columns_to_summarise }}, {{ country_factor }})), .drop = FALSE) %>%
          dplyr::summarise(n = n())
        corner_margin_country <- data %>%
          dplyr::group_by(dplyr::across(c({{ country_factor }})), .drop = FALSE) %>%
          dplyr::summarise(n = n())
        names(summary_output_country)[length(summary_output_country)-1] <- names(summary_output)[length(summary_output)-1]
        names(corner_margin_country)[length(corner_margin_country)-1] <- names(summary_output)[length(summary_output)-1]
        #summary_output_country <- dplyr::rename(summary_output_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
        #corner_margin_country <- dplyr::rename(corner_margin_country, Org = {{ country_factor }}) #"{{ factors }}" := {{ country_factor }})
      } else {
        summary_output_country <- NULL
        corner_margin_country <- NULL
      }
      
      summary_output <- bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, summary_output_country, corner_margin_country, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        dplyr::mutate(dplyr::across({{ factors }}, as.character)) %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4, 6), "Total", .x)))
      
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::select(-c("id"))
    }
  } else {
    summary_output <- data %>%
      dplyr::group_by(dplyr::across({{ factors }}), .drop = FALSE) %>%
      #dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      dplyr::summarise(dplyr::across({{ columns_to_summarise }}, ~mean(.x, na.rm = na.rm)))
    
    if (include_margins){
      corner_margin <- data %>%
        dplyr::summarise(dplyr::across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = na.rm)))
      
      summary_output <- bind_rows(summary_output, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        ungroup() %>%
        dplyr::mutate(dplyr::across({{ factors }}, as.character)) %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~ifelse(id == 2, "Total", .x)))
      
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::select(-c("id"))
    }
    
  }
  return(summary_output)
}