#' Create a Top Value Box for Shiny
#'
#' This function creates a top value box for use in a Shiny application. The value box displays summary information 
#' based on a specified variable from a data frame, with additional options for mean or value-based boxes.
#'
#' @param data_frame A data frame containing the data to be summarised.
#' @param spreadsheet A data frame representing the spreadsheet template.
#' @param processed_spreadsheet A data frame representing the processed spreadsheet data.
#' @param unique_ID A string specifying the unique identifier for the value box.
#' @param list_of_reactives A list of reactive data frames used to dynamically update content in a Shiny app.
#'
#' @return A `shinydashboard::valueBox` object for use in a Shiny application.
top_value_boxes <- function(data_frame, spreadsheet, processed_spreadsheet, unique_ID, list_of_reactives){
  
  # Extract the relevant row from the processed spreadsheet
  spreadsheet_row <- spreadsheet %>% dplyr::filter(name == unique_ID)
  processed_spreadsheet <- processed_spreadsheet %>% dplyr::filter(name == unique_ID)
  
  # calling in the data argument in spreadsheet
  # doing this way to account for partial matching with "data_manip"
  if (is.na(match("data", names(spreadsheet_row))) || 
      (!is.na(match("data", names(spreadsheet_row))) && is.na(spreadsheet_row %>% dplyr::pull("data"))) ){
    data_frame_read <- data_frame
  } else {
    data_frame_read <- list_of_reactives[[spreadsheet_row$data]]()
  }
  data_frame_read <- data_frame_read %>% dplyr::ungroup()
  
  # Extract specific parameters
  text <- spreadsheet_finder(data = processed_spreadsheet, "text")
  icon_pic <- spreadsheet_finder(data = processed_spreadsheet, "icon")
  colour <- spreadsheet_finder(data = processed_spreadsheet, "colour")
  variable <- spreadsheet_row$variable
  variable_value <- spreadsheet_row$variable_value
  value_box_type <- spreadsheet_row$value
  data_frame_read <- data_frame_read %>% dplyr::ungroup()
  
  get_fn <- function(data_frame_read, variable, fun_str) {
    # Remove "_box" from the function string
    fun_clean <- sub("_box", "", fun_str)
    
    # Match the cleaned string to the actual function
    fun <- match.fun(fun_clean)
    
    # Apply the function
    value <- round((data_frame_read %>% 
                      dplyr::summarise(result = fun(.data[[variable]], na.rm = TRUE)))$result, 2)
    return(value)
  }
  
  if (value_box_type == "value_box_rows"){ # get the number of rows in a filtered data frame
    value <- nrow(data_frame_read)
  } else if (value_box_type == "value_box"){ # get a specific value 
    if (!is.na(spreadsheet_row$variable_value)){
      df_box <- summary_table(data_frame_read, factors = .data[[variable]], wider_table = TRUE, together = FALSE, naming_convention = FALSE)
      df_box <- df_box %>% dplyr::mutate(group = .data[[variable]], count = n, .drop = FALSE) %>%
        dplyr::select(c(group, count))
      value <- (df_box %>% dplyr::filter(group == variable_value))$count
      if (length(value) == 0) value <- 0
    } else {
      value <- nrow(data_frame_read)
    }
  } else if (value_box_type == "mean_sd_box"){
    mean_value <- round((data_frame_read %>% dplyr::summarise(mean = mean(.data[[variable]], na.rm = TRUE)))$mean, 2)
    sd_value <- round((data_frame_read %>% dplyr::summarise(sd = stats::sd(.data[[variable]], na.rm = TRUE)))$sd, 2)
    value <- paste0(mean_value, " (", sd_value, ")")
  } else {
    value <- get_fn(data_frame_read = data_frame_read, variable = variable, fun_str = value_box_type)
  }
  return(shinydashboard::valueBox(value, subtitle = text, icon = shiny::icon(icon_pic), color = colour))
}