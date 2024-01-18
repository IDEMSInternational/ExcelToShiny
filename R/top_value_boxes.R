#' Creating top value box to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier.
#'
#' @return Top value box for use in `Shiny`
#' @export
top_value_boxes <- function(data_frame, spreadsheet, unique_ID){
  spreadsheet <- spreadsheet %>% dplyr::filter(name == unique_ID)
  spreadsheet_parameters <- spreadsheet$parameter_list
  spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, ", ", simplify = TRUE))
  spreadsheet_parameters_names <- sub("\\= .*", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- gsub(".*= ", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- stringr::str_remove_all(spreadsheet_parameters_values, stringr::fixed("\""))
  values <- spreadsheet_parameters_values
  names <- spreadsheet_parameters_names
  spreadsheet_df <- data.frame(names, values)
  
  text <- spreadsheet_finder(data = spreadsheet_df, "text ")
  icon_pic <- spreadsheet_finder(data = spreadsheet_df, "icon ")
  colour <- spreadsheet_finder(data = spreadsheet_df, "colour ")
  variable <- spreadsheet$variable
  #if (!variable %in% names(data_frame)) stop(paste0(variable, " not in data."))
  variable_value <- spreadsheet$variable_value
  value_box_type <- spreadsheet$value
  if (value_box_type == "value_box"){
    if (!is.na(spreadsheet$variable_value)){
      df_box <- summary_table(data_frame, factors = .data[[variable]], summaries = "frequencies", wider_table = TRUE, together = FALSE, naming_convention = FALSE)
      df_box <- df_box %>% dplyr::mutate(group = .data[[variable]], count = n, .drop = FALSE) %>%
        dplyr::select(c(group, count))
      value <- (df_box %>% dplyr::filter(group == variable_value))$count
    } else {
      value <- nrow(data_frame)
    }
  } else if (value_box_type == "mean_box"){
    value <- round((data_frame %>% dplyr::summarise(mean = mean(.data[[variable]], na.rm = TRUE)))$mean, 2)
  } else {
    mean_value <- round((data_frame %>% dplyr::summarise(mean = mean(.data[[variable]], na.rm = TRUE)))$mean, 2)
    sd_value <- round((data_frame %>% dplyr::summarise(sd = sd(.data[[variable]], na.rm = TRUE)))$sd, 2)
    value <- paste0(mean_value, " (", sd_value, ")")
  }
    return(shinydashboard::valueBox(value, subtitle = text, icon = shiny::icon(icon_pic), color = colour))
}




