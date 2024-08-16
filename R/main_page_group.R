#' Generate Main Page Group Input Widget
#'
#' This function generates a group input widget for a `shinydashboard` main page. The widget is created based on the parameters provided in the `spreadsheet` data frame and is currently designed to support a single group input.
#'
#' @param spreadsheet A data frame containing the parameters for generating the group input widget. The data frame should include columns such as `name` and `parameter_list` that define the input widget's configuration.
#'
#' @return A list of shiny input widgets for grouping data, each created according to the specifications in the `spreadsheet`.
#'
#' @details This function creates a checkbox input widget (or multiple widgets) for grouping data on a `shinydashboard` main page. Currently, the function supports only one group by input at a time.
main_page_group <- function(spreadsheet){
  # For the group input
  checkbox_group_input <- NULL
  checkbox_group_data <- spreadsheet
  
  # for value == "checkbox_group"
  #if (nrow(checkbox_group_data) > 1){
  #  stop("Currently only support one group by")
  #} else {
  #name <- checkbox_group_data$name
  
  if (nrow(checkbox_group_data) > 0){
    for (i in 1:nrow(checkbox_group_data)){
      checkbox_group_data_i <- checkbox_group_data[i,]
      spreadsheet_parameters <- checkbox_group_data_i$parameter_list
      name <- checkbox_group_data_i$name
      label <- get_parameter_value(spreadsheet_parameters, name = "label")
      value <- get_parameter_value(spreadsheet_parameters, name = "value", logical = TRUE)
      width <- get_parameter_value(spreadsheet_parameters, name = "width")
      # todo - set up for inline (TRUE/FALSE parameter)
      checkbox_group_input[[i]] <- shiny::checkboxInput(inputId = paste0(name),
                                                        label = label,
                                                        value = value,
                                                        width = width)
    }
  }
  return(checkbox_group_input)
  #}
  # return(shinydashboard::box(width = 6,
  #                            checkbox_group_input,
  #                            # then run the actionButton
  #                            # this means currently only checkbox filtering on the main page.
  #                            shiny::actionButton(paste0("group_by_button"), "Submit", class = "btn-success")))
}