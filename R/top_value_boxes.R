#' Creating top value box to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier.
#'
#' @return Top value box for use in `Shiny`
#' @export
top_value_boxes <- function(data_frame, spreadsheet, processed_spreadsheet, unique_ID){
  
  # Extract the relevant row from the processed spreadsheet
  spreadsheet_row <- spreadsheet %>% dplyr::filter(name == unique_ID)
  processed_spreadsheet <- processed_spreadsheet %>% dplyr::filter(name == unique_ID)
  
  # Extract specific parameters
  text <- spreadsheet_finder(data = processed_spreadsheet, "text")
  icon_pic <- spreadsheet_finder(data = processed_spreadsheet, "icon")
  colour <- spreadsheet_finder(data = processed_spreadsheet, "colour")
  variable <- spreadsheet_row$variable
  variable_value <- spreadsheet_row$variable_value
  value_box_type <- spreadsheet_row$value
  if (value_box_type == "value_box"){
    if (!is.na(spreadsheet_row$variable_value)){
      df_box <- summary_table(data_frame, factors = .data[[variable]], wider_table = TRUE, together = FALSE, naming_convention = FALSE)
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
  print("HELLLO")
  print(value)
  print(text)
  print(icon_pic)
  print(colour)
  return(shinydashboard::valueBox(value, subtitle = text, icon = shiny::icon(icon_pic), color = colour))
}
