#' Calculate summaries from PLH data
#'
#' @param data Data frame to calculate summaries from.
#' @param factors List of factors to group by.
#' @param columns_to_summarise Variables to dplyr::summarise.
#' @param summaries Whether `frequencies` or `mean` summaries are calculated.
#' @param together logical. Default `FALSE`. If `summaries = "frequencies"`, whether to combine the count and percentage into one cell.
#' @param include_margins logical. Default `FALSE`. Whether to include margins.
#' @param drop logical. Default `FALSE`. Whether to drop columns in `group_by`.
#' @importFrom rlang :=
#' @return Summaries table
#' @export
summary_calculation <- function(data = plhdata_org_clean, factors, columns_to_summarise = NULL, summaries = c("frequencies", "mean"),
                                together = FALSE, include_margins = FALSE, drop = FALSE){
  summaries <- match.arg(summaries)
  if (summaries == "frequencies"){
    summary_output <- data %>%
      dplyr::mutate(dplyr::across(c({{ columns_to_summarise }}), ~ (as.character(.x)))) %>%
      dplyr::group_by(dplyr::across(c({{ columns_to_summarise }}, {{ factors }})), .drop = drop) %>%
      dplyr::summarise(n = dplyr::n(),
                       perc = dplyr::n()/nrow(data) * 100) %>%
      dplyr::ungroup()
    if (include_margins){
      cts_margin <- data %>%
        dplyr::group_by(dplyr::across(c({{ columns_to_summarise }})), .drop = drop) %>%
        dplyr::summarise(n = dplyr::n(),
                         perc = dplyr::n()/nrow(data) * 100)      
      ftr_margin <- data %>%
        dplyr::group_by(dplyr::across(c({{ factors }})), .drop = drop) %>%
        dplyr::summarise(n = dplyr::n(),
                         perc = dplyr::n()/nrow(data) * 100)      
      corner_margin <- data %>%
        dplyr::summarise(n = dplyr::n(),
                         perc = dplyr::n()/nrow(data) * 100)
      summary_output <- dplyr::bind_rows(summary_output, cts_margin, ftr_margin, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        #ungroup() %>%
        dplyr::mutate(dplyr::across({{ factors }}, as.character)) %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~ifelse(id %in% c(2, 4), "Total", .x))) %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~ifelse(id %in% c(3, 4), "Total", .x)))
      
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::select(-c("id"))
    }
    if (together){
      summary_output <- summary_output %>%
        dplyr::mutate("Count (%)" := stringr::str_c(`n`, ' (', round(`perc`, 2), ")")) %>%
        dplyr::select(-c(n, perc))
    }
  } else {
    summary_output <- data %>%
      dplyr::group_by(dplyr::across({{ factors }}), .drop = drop) %>%
      #dplyr::mutate(dplyr::across({{ columns_to_summarise }}, ~as.numeric(.))) %>%
      dplyr::summarise(dplyr::across({{ columns_to_summarise }}, ~mean(.x, na.rm = TRUE)))
    
    if (include_margins){
      corner_margin <- data %>%
        dplyr::summarise(dplyr::across(c({{ columns_to_summarise }}), ~mean(.x, na.rm  = TRUE)))
      
      summary_output <- dplyr::bind_rows(summary_output, corner_margin, .id = "id")
      
      summary_output <- summary_output %>%
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across({{ factors }}, as.character)) %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~ifelse(id == 2, "Total", .x)))
      
      summary_output <- summary_output %>%
        dplyr::mutate(dplyr::across({{ factors }}, ~forcats::fct_relevel(.x, "Total", after = Inf))) %>%
        dplyr::select(-c("id"))
    }
  }
  if (length(data %>% dplyr::select({{ factors }})) == 1){
    cell_values_levels <- data %>% dplyr::pull({{ factors }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    summary_output <- summary_output %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ factor(.x))) %>%
      dplyr::mutate(dplyr::across({{ factors }},
                                  ~ forcats::fct_relevel(.x, cell_values_levels)))
    summary_output <- summary_output %>% dplyr::arrange({{ factors }})
  }
  if (length(data %>% dplyr::select({{ columns_to_summarise }})) == 1){
    cell_values_levels <- data %>% dplyr::pull({{ columns_to_summarise }}) %>% levels()
    if (include_margins){ cell_values_levels <- c(cell_values_levels, "Total") }
    
    summary_output <- summary_output %>%
      dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                  ~ factor(.x))) %>%
      dplyr::mutate(dplyr::across({{ columns_to_summarise }},
                                  ~ forcats::fct_relevel(.x, cell_values_levels)))
    summary_output <- summary_output %>% dplyr::arrange({{ columns_to_summarise }})
  }
  return(unique(summary_output))
}
