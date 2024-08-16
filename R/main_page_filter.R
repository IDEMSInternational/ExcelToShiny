#' Generate Checkbox Group Input Elements
#'
#' This function generates a set of checkbox group input elements based on the provided checkbox data, and returns them within a Shiny dashboard box along with a submit action button.
#'
#' @param spreadsheet A data frame containing information about the checkbox groups to be created. The data frame should include columns such as `type`, `name`, and `parameter_list` that define the checkbox group inputs.
#'
#' @return A `shinydashboard::box` object containing the generated checkbox group input elements and an action button for submission.
main_page_filter <- function(spreadsheet){
  # For the group input
  checkbox_group_input <- NULL
  checkbox_group_data <- spreadsheet %>% dplyr::filter(type == "filter_box")
  
  # for value == "checkbox_group"
  if (nrow(checkbox_group_data) > 0){
    for (i in 1:nrow(checkbox_group_data)){
      checkbox_group_data_i <- checkbox_group_data[i,]
      name <- checkbox_group_data_i$name
      spreadsheet_parameters <- checkbox_group_data_i$parameter_list
      label <- get_parameter_value(spreadsheet_parameters, name = "label")
      choices <- get_parameter_value(spreadsheet_parameters, name = "choices", TRUE)
      selected <- get_parameter_value(spreadsheet_parameters, name = "selected", TRUE)
      width <- get_parameter_value(spreadsheet_parameters, name = "width")
      choiceNames <- get_parameter_value(spreadsheet_parameters, name = "choiceNames", TRUE)
      choiceValues <- get_parameter_value(spreadsheet_parameters, name = "choiceValues", TRUE)
      # todo - set up for inline (TRUE/FALSE parameter)
      
      checkbox_group_input[[i]] <- shiny::checkboxGroupInput(inputId = paste0(name),
                                                      label = label, width = width,
                                                      choices = choices, selected = selected,
                                                      choiceNames = choiceNames, choiceValues = choiceValues)
    }
  }
  
  # todo:
  return(shinydashboard::box(width = 6,
             checkbox_group_input,
             # then run the actionButton
             # this means currently only checkbox filtering on the main page.
             shiny::actionButton(paste0("goButton_group"), "Submit", class = "btn-success")))
}
