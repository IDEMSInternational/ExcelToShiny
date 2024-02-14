#' Main Page Group
#' 
#' This function generates a group input widget for a shinydashboard main page.
#' 
#' @param spreadsheet A data frame containing parameters for generating the group input.
#' 
#' @return A shiny input widget for grouping data.
#' 
#' @details This function is designed to create a group input widget for a shinydashboard main page. 
#' It currently supports only one group by.
#' 
#' @export
#' @examples
#' # Sample usage
#' #main_page_group(my_spreadsheet_data)
#' 
main_page_group <- function(spreadsheet){
  # For the group input
  checkbox_group_input <- NULL
  checkbox_group_data <- spreadsheet

  # for value == "checkbox_group"
  if (nrow(checkbox_group_data) > 1){
    stop("Currently only support one group by")
  } else {
    #name <- checkbox_group_data$name
    spreadsheet_parameters <- checkbox_group_data$parameter_list
      print(spreadsheet_parameters)
      label <- get_parameter_value(spreadsheet_parameters, name = "label")
      value <- get_parameter_value(spreadsheet_parameters, name = "value", logical = TRUE)
      print(spreadsheet_parameters)
      width <- get_parameter_value(spreadsheet_parameters, name = "width")
      # todo - set up for inline (TRUE/FALSE parameter)
      
      checkbox_group_input[[1]] <- shiny::checkboxInput(inputId = paste0("group_by_button"),
                                                             label = label,
                                                        value = value,
                                                        width = width)
      return(checkbox_group_input)
  }
  # return(shinydashboard::box(width = 6,
  #                            checkbox_group_input,
  #                            # then run the actionButton
  #                            # this means currently only checkbox filtering on the main page.
  #                            shiny::actionButton(paste0("group_by_button"), "Submit", class = "btn-success")))
}
