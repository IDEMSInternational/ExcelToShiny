#' Calculate summaries from data
#'
#' @param data Data frame to calculate summaries from.
#' @param factors List of factors to group by.
#' @param columns_to_summarise Variables to summarise.
#' @param summaries Types of summaries: "frequencies", "mean", "median", "sd", "min", "max".
#' @param together Combine count and percentage into one cell if `summaries = "frequencies"`.
#' @param include_margins Include margins in the summaries.
#' @param drop Drop columns in `group_by`.
#' @return Summaries table
#' 
#' @export
summary_calculation <- function(data, factors, columns_to_summarise = NULL, summaries = c("frequencies", "mean", "median", "sd", "min", "max"),
                                 together = FALSE, include_margins = FALSE, drop = FALSE){
  
  # Ensure summaries is a vector
  summaries <- match.arg(summaries, several.ok = TRUE)
  
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
    return(summary_output)
  } else {
    
    # TODO: work for if no factors
    summary_output <- list()
    summary_funcs <- list(
      mean = ~mean(.x, na.rm = TRUE), 
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
    if (!is.null(columns_to_summarise)) {
      for (summary_type in summaries[summaries != "frequencies"]) {
        summary_func <- summary_funcs[[summary_type]]
        calc_summary <- dplyr::group_by(data, dplyr::across(all_of(factors)), .drop = drop)
        
        if (length(columns_to_summarise) > 1){
          calc_summary <- calc_summary %>% 
            dplyr::summarise(dplyr::across(all_of(columns_to_summarise), summary_func, .names = "{summary_type}_{.col}"))
        } else {
          if (!is.numeric(calc_summary[[columns_to_summarise]])) {
            calc_summary <- calc_summary %>%
              dplyr::mutate(dplyr::across(columns_to_summarise, ~is.numeric(is.character(.x))))
          }
          calc_summary <- calc_summary %>% 
            dplyr::summarise(dplyr::across(all_of(columns_to_summarise), summary_func, .names = "{summary_type}"))
        }
        
        if (include_margins) {
          corner_margin <- dplyr::summarise(data, dplyr::across(all_of(columns_to_summarise), summary_func, .names = "{summary_type}_{.col}"))
          calc_summary <- dplyr::bind_rows(calc_summary, corner_margin, .id = "id") %>%
            dplyr::mutate(dplyr::across(all_of(factors), ~ifelse(id == "2", "Total", as.character(.x)))) %>%
            dplyr::mutate(dplyr::across(all_of(factors), ~forcats::fct_relevel(factor(.x), "Total", after = Inf))) %>%
            dplyr::select(-c("id"))
        }
        
        summary_output[[summary_type]] <- calc_summary
      }
    }
    if (is.null(factors)) {
      combined_data <- dplyr::bind_cols(summary_output)
    } else {
      combined_data <- reduce(summary_output, inner_join, by = factors)
    }
    
    return(combined_data)
  }
}